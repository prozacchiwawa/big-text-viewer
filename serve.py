import argparse
import asyncio
import struct
import os
from lines import Lines
from track import add_entry, get_track_lines

from flask import Flask, request, Response
from flask_cors import CORS
app = Flask(__name__)
CORS(app)

parser = argparse.ArgumentParser()
parser.add_argument('txt')
parser.add_argument('lines')

args = parser.parse_args()
lines = None

def line_from_params(low, high):
    low = int(low)
    high = int(high)
    return high * 0x10000000 + low

@app.route('/line', methods = ['GET'])
async def get_line():
    global lines
    line = line_from_params(request.args.get('low'), request.args.get('high'))
    async with lines.filelock:
        text = lines.getline(line)
        return {'content': text, 'line': decompose_line(line)}

@app.route('/lines', methods = ['GET', 'OPTIONS'])
async def get_lines():
    global lines
    line = line_from_params(request.args.get('low'), request.args.get('high'))
    async with lines.filelock:
        line = (line | 0x3ff) ^ 0x3ff

        # Find the region to read from the text file
        try:
            text_lines = lines.getblock(line)
            return list(map(lambda x: {'content': x}, text_lines))
        except Exception as e:
            print(e)
            return {"error": str(e)}

@app.route('/deepen', methods=['POST'])
async def deepen():
    track = request.args.get('track')
    line = line_from_params(request.args.get('low'), request.args.get('high'))
    linedata = decompose_line(line)
    add_entry(track, linedata['low'], linedata['high'], 1)
    return {"ok", True}

@app.route('/shallow', methods=['POST'])
async def shallow():
    track = request.args.get('track')
    line = line_from_params(request.args.get('low'), request.args.get('high'))
    linedata = decompose_line(line)
    add_entry(track, linedata['low'], linedata['high'], -1)
    return {"ok", True}

def decompose_line(x):
    return {'low': x & 0xfffffff, 'high': x >> 28}

@app.route('/track', methods=['GET'])
async def get_track():
    global lines
    track = request.args.get('track')
    depth = int(request.args.get('depth'))
    line = line_from_params(request.args.get('low'), request.args.get('high'))
    track_lines = get_track_lines(lines, track, line, depth, limit = 1024)
    return list(map(decompose_line, track_lines))

def get_prev_lines(track, depth, line):
    global lines
    stride_before = 50

    have_lines = set()
    new_line = line - stride_before
    while len(have_lines) < 50:
        new_lines = get_track_lines(lines, track, new_line, depth, limit = 50)
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
    global lines
    track = request.args.get('track')
    depth = int(request.args.get('depth'))
    line = line_from_params(request.args.get('low'), request.args.get('high'))

    have_lines = get_prev_lines(track, depth, line)
    return list(map(decompose_line, sorted(list(have_lines))))

@app.route('/preview_prev_page', methods=['GET'])
async def preview_prev_page():
    global lines
    track = request.args.get('track')
    depth = int(request.args.get('depth'))
    line = line_from_params(request.args.get('low'), request.args.get('high'))

    prev_lines = get_prev_lines(lines, track, depth, line)
    result = []
    for line in prev_lines:
        result.append({"low": line & 0xfffffff, "high": line >> 28, "content": lines.getline(line) })

    return result

# Get the line numbers and then render the actual lines.
@app.route('/preview', methods=['GET'])
async def get_preview():
    global lines
    track = request.args.get('track')
    depth = int(request.args.get('depth'))
    line = line_from_params(request.args.get('low'), request.args.get('high'))

    result = []

    track_lines = get_track_lines(lines, track, line, depth)
    for line in track_lines:
        result.append({"low": line & 0xfffffff, "high": line >> 28, "content": lines.getline(line) })

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

async def main():
    global lines
    lines = Lines(args.txt, args.lines)
    print('local wallet connection on port 5000')
    app.run(port=5000)

if __name__ == '__main__':
    asyncio.run(main())
