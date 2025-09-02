import argparse
import asyncio
import struct
import os

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

@app.route('/lines', methods = ['GET', 'OPTIONS'])
async def get_lines():
    low = int(request.args.get('low'))
    high = int(request.args.get('high'))
    async with filelock:
        low = (low | 0x3ff) ^ 0x3ff
        low += high * 0xfffffff

        def consume_line(block, offset):
            start = offset
            while offset < len(block) and block[offset] != 10:
                offset += 1
            return block[start:offset].decode('iso-8859-1')

        # Find the region to read from the text file
        try:
            linedata.seek(low * 8)
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

@app.route('/', methods=['GET'])
def index():
    return Response(open('public/index.html'), mimetype='text/html')

@app.route('/index.js', methods=['GET'])
def js():
    return Response(open('public/index.js'), mimetype='application/javascript')

@app.route('/index.css', methods=['GET'])
def css():
    return Response(open('public/index.css'), mimetype='text/css')

if __name__ == '__main__':
    print('local wallet connection on port 5000')
    app.run(port=5000)
