import argparse
import asyncio
import struct
import os
import sqlite3

from flask import Flask, request, Response
from flask_cors import CORS
app = Flask(__name__)
CORS(app)

parser = argparse.ArgumentParser()
parser.add_argument('txt')
parser.add_argument('lines')

args = parser.parse_args()

textdata_length = os.stat(args.txt).st_size
linedata_length = os.stat(args.lines).st_size
textdata = open(args.txt, 'rb')
linedata = open(args.lines, 'rb')

filelock = asyncio.Lock()

def line_from_params(low, high):
    low = int(low)
    high = int(high)
    return high * 0x10000000 + low

def consume_line(block, offset):
    start = offset
    while offset < len(block) and block[offset] != 10:
        offset += 1
    return block[start:offset].decode('iso-8859-1')

def getline(line):
    linedata.seek(line * 8)
    lineblock = linedata.read(16)

    if len(lineblock) == 0:
        return "<eol>"
    elif len(lineblock) == 8:
        decoded1 = struct.unpack('<Q', lineblock)
        decoded = (decoded1, textdata_length)
    else:
        decoded = struct.unpack('<QQ', lineblock)

    textdata.seek(decoded[0])
    textblock = textdata.read(decoded[1] - decoded[0])
    return consume_line(textblock, 0)

@app.route('/line', methods = ['GET'])
async def get_line():
    line = line_from_params(request.args.get('low'), request.args.get('high'))
    async with filelock:
        text = getline(line)
        return {'content': text, 'line': decompose_line(line)}

@app.route('/lines', methods = ['GET', 'OPTIONS'])
async def get_lines():
    line = line_from_params(request.args.get('low'), request.args.get('high'))
    async with filelock:
        line = (line | 0x3ff) ^ 0x3ff

        # Find the region to read from the text file
        try:
            linedata.seek(line * 8)
            lineblock = linedata.read(8 * 0x401)
            decoded_zero = struct.unpack('<Q', lineblock[:8])[0]

            # Get the end of the text file if we overran
            if len(lineblock) < 8 * 0x401:
                decoded_end = textdata_length
            else:
                decoded_end = struct.unpack('<Q', lineblock[-8:])[0]

            relative_offsets = list(map(lambda x: struct.unpack('<Q', lineblock[x * 8:(x+1) * 8])[0] - decoded_zero, range(int(len(lineblock) / 8))))
            if len(relative_offsets) > 0x400:
                relative_offsets = relative_offsets[:0x400]

            textdata.seek(decoded_zero)
            textblock = textdata.read(decoded_end - decoded_zero)
            lines = list(map(lambda x: consume_line(textblock, x), relative_offsets))
            return list(map(lambda x: {'content': x}, lines))
        except Exception as e:
            print(e)
            return {"error": str(e)}

def materialize_db():
    sq = sqlite3.connect('tracks.db')
    cursor = sq.execute('create table if not exists tracks (track varchar(255) not null, line integer not null, adj integer not null, primary key (track, line))')
    sq.commit()
    return sq

def add_entry(track, line, adj):
    with materialize_db() as sq:
        cursor = sq.execute('insert or replace into tracks values (?, ?, ifnull(select adj from track as t1 where t1.track = ? and t1.line < ? order by t1.line desc limit 1, 0) + ?)', (track, line, track, line, adj))
        sq.commit()

# Get 1024 more lines after the given line number.
def get_track_lines(track, line, depth, limit = 1024):
    with materialize_db() as sq:
        cursor = sq.execute('select line, adj from tracks where track = ? and line < ? order by line desc limit 1', (track, line))
        prev_line = cursor.fetchone()
        cursor = sq.execute('select line, adj from tracks where track = ? and line >= ? order by line asc limit 1024', (track, line))
        line_to_depth_map = dict(cursor.fetchall())

        result = []

        if prev_line is not None:
            d = prev_line[1]
        else:
            d = 0

        while len(result) < limit and line < (linedata_length / 8):
            d = line_to_depth_map.get(line, d)
            if d <= depth:
                lt = getline(line)

                # Skip if we're at depth.
                if lt.startswith('cpu0:') and d == depth:
                    line += 1
                    continue

                result.append(line)

            line += 1

        return result

@app.route('/deepen', methods=['POST'])
async def deepen():
    track = request.args.get('track')
    line = line_from_params(request.args.get('low'), request.args.get('high'))
    add_entry(track, low, high, 1)
    return {"ok", True}

@app.route('/shallow', methods=['POST'])
async def shallow():
    track = request.args.get('track')
    line = line_from_params(request.args.get('low'), request.args.get('high'))
    add_entry(track, low, high, -1)
    return {"ok", True}

def decompose_line(x):
    return {'low': x & 0xfffffff, 'high': x >> 28}

@app.route('/track', methods=['GET'])
async def get_track():
    track = request.args.get('track')
    depth = int(request.args.get('depth'))
    line = line_from_params(request.args.get('low'), request.args.get('high'))
    lines = get_track_lines(track, line, depth, limit = 1024)
    return list(map(decompose_line, lines))

def get_prev_lines(track, depth, line):
    stride_before = 50

    have_lines = set()
    new_line = line - stride_before
    while len(have_lines) < 50:
        new_lines = get_track_lines(track, new_line, depth, limit = 50)
        have_lines = have_lines.union(set(filter(lambda x: x < line, new_lines)))
        new_line = new_line - stride_before

    # have_lines is a set of lines we can use
    return have_lines

# Find a line that gives us a maximum of 1024 lines before the target line.
# The result is a new line number that we can seek to that can fill up a
# display window afterward with lines prior to the desired line.
#
# We'll search backward by strides multiplied by 2.  We can generate each set
# of prior lines individually until we have enough lines.
#
# Return a list of lines that fulfill pageup from the given line.
@app.route('/prev_page', methods=['GET'])
async def prev_page():
    track = request.args.get('track')
    depth = int(request.args.get('depth'))
    line = line_from_params(request.args.get('low'), request.args.get('high'))

    have_lines = get_prev_lines(track, depth, line)
    return list(map(decompose_line, sorted(list(have_lines))))

@app.route('/preview_prev_page', methods=['GET'])
async def preview_prev_page():
    track = request.args.get('track')
    depth = int(request.args.get('depth'))
    line = line_from_params(request.args.get('low'), request.args.get('high'))

    lines = get_prev_lines(track, depth, line)
    result = []
    for line in lines:
        result.append({"low": line & 0xfffffff, "high": line >> 28, "content": getline(line) })

    return result

# Get the line numbers and then render the actual lines.
@app.route('/preview', methods=['GET'])
async def get_preview():
    track = request.args.get('track')
    depth = int(request.args.get('depth'))
    line = line_from_params(request.args.get('low'), request.args.get('high'))

    result = []

    lines = get_track_lines(track, line, depth)
    for line in lines:
        result.append({"low": line & 0xfffffff, "high": line >> 28, "content": getline(line) })

    return result

@app.route('/', methods=['GET'])
def index():
    return Response(open('public/index.html'), mimetype='text/html')

@app.route('/index.js', methods=['GET'])
def js():
    return Response(open('public/index.js'), mimetype='application/javascript')

@app.route('/index.css', methods=['GET'])
def css():
    return Response(open('public/index.css'), mimetype='text/css')

@app.route('/IBMPlexMono-Medium.ttf', methods=['GET'])
def font():
    return Response(open('public/IBMPlexMono-Medium.ttf', 'rb'), mimetype='font/ttf')

if __name__ == '__main__':
    print('local wallet connection on port 5000')
    app.run(port=5000)
