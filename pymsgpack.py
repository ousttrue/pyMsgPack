import struct

POSITIVE_FIXINT=0x00

NIL=b'\xC0'

FALSE=b'\xC2'
TRUE=b'\xC3'

FLOAT32=0xCA
FLOAT64=0xCB

POSITIVE_FIXINT_MAP={
        0x00: 0,
        0x01: 1,
        0x02: 2,
        0x03: 3,
        0x04: 4,
        0x05: 5,
        0x06: 6,
        0x07: 7,
        0x08: 8,
        0x09: 9,
        0x0A: 10,
        0x0B: 11,
        0x0C: 12,
        0x0D: 13,
        0x0E: 14,
        0x0F: 15,
        0x10: 16,
        0x11: 17,
        0x12: 18,
        0x13: 19,
        0x14: 20,
        0x15: 21,
        0x16: 22,
        0x17: 23,
        0x18: 24,
        0x19: 25,
        0x1A: 26,
        0x1B: 27,
        0x1C: 28,
        0x1D: 29,
        0x1E: 30,
        0x1F: 31,
        0x20: 32,
        0x21: 33,
        0x22: 34,
        0x23: 35,
        0x24: 36,
        0x25: 37,
        0x26: 38,
        0x27: 39,
        0x28: 40,
        0x29: 41,
        0x2A: 42,
        0x2B: 43,
        0x2C: 44,
        0x2D: 45,
        0x2E: 46,
        0x2F: 47,
        0x30: 48,
        0x31: 49,
        0x32: 50,
        0x33: 51,
        0x34: 52,
        0x35: 53,
        0x36: 54,
        0x37: 55,
        0x38: 56,
        0x39: 57,
        0x3A: 58,
        0x3B: 59,
        0x3C: 60,
        0x3D: 61,
        0x3E: 62,
        0x3F: 63,
        0x40: 64,
        0x41: 65,
        0x42: 66,
        0x43: 67,
        0x44: 68,
        0x45: 69,
        0x46: 70,
        0x47: 71,
        0x48: 72,
        0x49: 73,
        0x4A: 74,
        0x4B: 75,
        0x4C: 76,
        0x4D: 77,
        0x4E: 78,
        0x4F: 79,
        0x50: 80,
        0x51: 81,
        0x52: 82,
        0x53: 83,
        0x54: 84,
        0x55: 85,
        0x56: 86,
        0x57: 87,
        0x58: 88,
        0x59: 89,
        0x5A: 90,
        0x5B: 91,
        0x5C: 92,
        0x5D: 93,
        0x5E: 94,
        0x5F: 95,
        0x60: 96,
        0x61: 97,
        0x62: 98,
        0x63: 99,
        0x64: 100,
        0x65: 101,
        0x66: 102,
        0x67: 103,
        0x68: 104,
        0x69: 105,
        0x6A: 106,
        0x6B: 107,
        0x6C: 108,
        0x6D: 109,
        0x6E: 110,
        0x6F: 111,
        0x70: 112,
        0x71: 113,
        0x72: 114,
        0x73: 115,
        0x74: 116,
        0x75: 117,
        0x76: 118,
        0x77: 119,
        0x78: 120,
        0x79: 121,
        0x7A: 122,
        0x7B: 123,
        0x7C: 124,
        0x7D: 125,
        0x7E: 126,
        0x7F: 127,
        }


def pack(obj):
    if obj is None:
        return NIL
    elif obj is False:
        return FALSE
    elif obj is True:
        return TRUE
    elif isinstance(obj, int):
        if obj>=0 and obj<= 0x7f:
            return struct.pack('b', obj)
    elif isinstance(obj, float):
        return struct.pack('>Bd', FLOAT64, obj) 

    raise NotImplementedError('pack failed. %s' % obj)


class Parser:
    def __init__(self, bytedata):
        self.bytedata=bytedata

    def is_nil(self):
        return self.bytedata[0:1]==NIL

    def get_bool(self):
        if self.bytedata[0:1]==FALSE:
            return False
        elif self.bytedata[0:1]==TRUE:
            return True

        raise NotImplementedError('parse failed. %s' % self.bytedata[0:1])

    def get_int(self):
        head=self.bytedata[0]
        if head in POSITIVE_FIXINT_MAP:
            return POSITIVE_FIXINT_MAP[head]

        raise NotImplementedError('get_int failed. %s' % head)

    def get_float(self):
        head=self.bytedata[0]
        if head==FLOAT32:
            return struct.unpack('>f', self.bytedata[1:5])[0]
        elif head==FLOAT64:
            return struct.unpack('>d', self.bytedata[1:9])[0]
        else:
            raise ValueError('is not float. %s' % head)

