import os
import struct
import asyncio

def consume_line(block, offset):
    start = offset
    while offset < len(block) and block[offset] != 10:
        offset += 1
    return block[start:offset].decode('iso-8859-1')

class Lines:
    def __init__(self, text, lineoff):
        self.textdata_length = os.stat(text).st_size
        self.linedata_length = os.stat(lineoff).st_size
        self.textdata = open(text, 'rb')
        self.linedata = open(lineoff, 'rb')
        self.filelock = asyncio.Lock()

    def length(self):
        return int(self.linedata_length / 8)

    def getline(self, line):
        line = max(line, 0)
        self.linedata.seek(line * 8)
        lineblock = self.linedata.read(16)

        if len(lineblock) == 0:
            return "<eol>"
        elif len(lineblock) == 8:
            decoded1 = struct.unpack('<Q', lineblock)
            decoded = (decoded1, self.textdata_length)
        else:
            decoded = struct.unpack('<QQ', lineblock)

        self.textdata.seek(decoded[0])
        textblock = self.textdata.read(decoded[1] - decoded[0])
        return consume_line(textblock, 0)

    def getblock(self, line):
        line = max(line, 0)
        self.linedata.seek(line * 8)
        lineblock = self.linedata.read(8 * 0x401)
        decoded_zero = struct.unpack('<Q', lineblock[:8])[0]

        # Get the end of the text file if we overran
        if len(lineblock) < 8 * 0x401:
            decoded_end = self.textdata_length
        else:
            decoded_end = struct.unpack('<Q', lineblock[-8:])[0]

        relative_offsets = list(map(lambda x: struct.unpack('<Q', lineblock[x * 8:(x+1) * 8])[0] - decoded_zero, range(int(len(lineblock) / 8))))
        if len(relative_offsets) > 0x400:
            relative_offsets = relative_offsets[:0x400]

        self.textdata.seek(decoded_zero)
        textblock = self.textdata.read(decoded_end - decoded_zero)
        return list(map(lambda x: consume_line(textblock, x), relative_offsets))

