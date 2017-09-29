import struct
from enum import Enum


class ValueType(Enum):
    NIL=0
    BOOL=1
    INT=2
    FLOAT=3
    BIN=4
    STR=5
    ARRAY=6
    MAP=7


class MsgPackFormat(Enum):
    POSITIVE_FIXINT=0x00
    FIXMAP=0x80
    FIXARRAY=0x90
    FIXSTR=0xA0
    NIL=0xC0
    FALSE=0xC2
    TRUE=0xC3
    BIN8=0xC4
    BIN16=0xC5
    BIN32=0xC6
    # 0xC7
    # 0xC8
    # 0xC9
    FLOAT32=0xCA
    FLOAT64=0xCB
    UINT8=0xCC
    UINT16=0xCD
    UINT32=0xCE
    UINT64=0xCF
    INT8=0xD0
    INT16=0xD1
    INT32=0xD2
    INT64=0xD3
    # 0xD4
    # 0xD5
    # 0xD6
    # 0xD7
    # 0xD8
    STR8=0xD9
    STR16=0xDA
    STR32=0xDB
    ARRAY16=0xDC
    ARRAY32=0xDD
    MAP16=0xDE
    MAP32=0xDF
    NEGATIVE_FIXINT=0xE0

def split_index(b, offset, count):
    end=offset+count
    return b[offset:end], end

def split_array(b, fmt, count_len):
    count=struct.unpack(fmt, b[:count_len])[0]
    return count, count_len

HEAD_MAP={
        0x00: (ValueType.INT, lambda b: (0, 0)),
        0x01: (ValueType.INT, lambda b: (1, 0)),
        0x02: (ValueType.INT, lambda b: (2, 0)),
        0x03: (ValueType.INT, lambda b: (3, 0)),
        0x04: (ValueType.INT, lambda b: (4, 0)),
        0x05: (ValueType.INT, lambda b: (5, 0)),
        0x06: (ValueType.INT, lambda b: (6, 0)),
        0x07: (ValueType.INT, lambda b: (7, 0)),
        0x08: (ValueType.INT, lambda b: (8, 0)),
        0x09: (ValueType.INT, lambda b: (9, 0)),
        0x0A: (ValueType.INT, lambda b: (10, 0)),
        0x0b: (ValueType.INT, lambda b: (11, 0)),
        0x0C: (ValueType.INT, lambda b: (12, 0)),
        0x0D: (ValueType.INT, lambda b: (13, 0)),
        0x0E: (ValueType.INT, lambda b: (14, 0)),
        0x0F: (ValueType.INT, lambda b: (15, 0)),
        0x10: (ValueType.INT, lambda b: (16, 0)),
        0x11: (ValueType.INT, lambda b: (17, 0)),
        0x12: (ValueType.INT, lambda b: (18, 0)),
        0x13: (ValueType.INT, lambda b: (19, 0)),
        0x14: (ValueType.INT, lambda b: (20, 0)),
        0x15: (ValueType.INT, lambda b: (21, 0)),
        0x16: (ValueType.INT, lambda b: (22, 0)),
        0x17: (ValueType.INT, lambda b: (23, 0)),
        0x18: (ValueType.INT, lambda b: (24, 0)),
        0x19: (ValueType.INT, lambda b: (25, 0)),
        0x1A: (ValueType.INT, lambda b: (26, 0)),
        0x1B: (ValueType.INT, lambda b: (27, 0)),
        0x1C: (ValueType.INT, lambda b: (28, 0)),
        0x1D: (ValueType.INT, lambda b: (29, 0)),
        0x1E: (ValueType.INT, lambda b: (30, 0)),
        0x1F: (ValueType.INT, lambda b: (31, 0)),
        0x20: (ValueType.INT, lambda b: (32, 0)),
        0x21: (ValueType.INT, lambda b: (33, 0)),
        0x22: (ValueType.INT, lambda b: (34, 0)),
        0x23: (ValueType.INT, lambda b: (35, 0)),
        0x24: (ValueType.INT, lambda b: (36, 0)),
        0x25: (ValueType.INT, lambda b: (37, 0)),
        0x26: (ValueType.INT, lambda b: (38, 0)),
        0x27: (ValueType.INT, lambda b: (39, 0)),
        0x28: (ValueType.INT, lambda b: (40, 0)),
        0x29: (ValueType.INT, lambda b: (41, 0)),
        0x2A: (ValueType.INT, lambda b: (42, 0)),
        0x2B: (ValueType.INT, lambda b: (43, 0)),
        0x2C: (ValueType.INT, lambda b: (44, 0)),
        0x2D: (ValueType.INT, lambda b: (45, 0)),
        0x2E: (ValueType.INT, lambda b: (46, 0)),
        0x2F: (ValueType.INT, lambda b: (47, 0)),
        0x30: (ValueType.INT, lambda b: (48, 0)),
        0x31: (ValueType.INT, lambda b: (49, 0)),
        0x32: (ValueType.INT, lambda b: (50, 0)),
        0x33: (ValueType.INT, lambda b: (51, 0)),
        0x34: (ValueType.INT, lambda b: (52, 0)),
        0x35: (ValueType.INT, lambda b: (53, 0)),
        0x36: (ValueType.INT, lambda b: (54, 0)),
        0x37: (ValueType.INT, lambda b: (55, 0)),
        0x38: (ValueType.INT, lambda b: (56, 0)),
        0x39: (ValueType.INT, lambda b: (57, 0)),
        0x3A: (ValueType.INT, lambda b: (58, 0)),
        0x3B: (ValueType.INT, lambda b: (59, 0)),
        0x3C: (ValueType.INT, lambda b: (60, 0)),
        0x3D: (ValueType.INT, lambda b: (61, 0)),
        0x3E: (ValueType.INT, lambda b: (62, 0)),
        0x3F: (ValueType.INT, lambda b: (63, 0)),
        0x40: (ValueType.INT, lambda b: (64, 0)),
        0x41: (ValueType.INT, lambda b: (65, 0)),
        0x42: (ValueType.INT, lambda b: (66, 0)),
        0x43: (ValueType.INT, lambda b: (67, 0)),
        0x44: (ValueType.INT, lambda b: (68, 0)),
        0x45: (ValueType.INT, lambda b: (69, 0)),
        0x46: (ValueType.INT, lambda b: (70, 0)),
        0x47: (ValueType.INT, lambda b: (71, 0)),
        0x48: (ValueType.INT, lambda b: (72, 0)),
        0x49: (ValueType.INT, lambda b: (73, 0)),
        0x4A: (ValueType.INT, lambda b: (74, 0)),
        0x4B: (ValueType.INT, lambda b: (75, 0)),
        0x4C: (ValueType.INT, lambda b: (76, 0)),
        0x4D: (ValueType.INT, lambda b: (77, 0)),
        0x4E: (ValueType.INT, lambda b: (78, 0)),
        0x4F: (ValueType.INT, lambda b: (79, 0)),
        0x50: (ValueType.INT, lambda b: (80, 0)),
        0x51: (ValueType.INT, lambda b: (81, 0)),
        0x52: (ValueType.INT, lambda b: (82, 0)),
        0x53: (ValueType.INT, lambda b: (83, 0)),
        0x54: (ValueType.INT, lambda b: (84, 0)),
        0x55: (ValueType.INT, lambda b: (85, 0)),
        0x56: (ValueType.INT, lambda b: (86, 0)),
        0x57: (ValueType.INT, lambda b: (87, 0)),
        0x58: (ValueType.INT, lambda b: (88, 0)),
        0x59: (ValueType.INT, lambda b: (89, 0)),
        0x5A: (ValueType.INT, lambda b: (90, 0)),
        0x5b: (ValueType.INT, lambda b: (91, 0)),
        0x5C: (ValueType.INT, lambda b: (92, 0)),
        0x5D: (ValueType.INT, lambda b: (93, 0)),
        0x5E: (ValueType.INT, lambda b: (94, 0)),
        0x5F: (ValueType.INT, lambda b: (95, 0)),
        0x60: (ValueType.INT, lambda b: (96, 0)),
        0x61: (ValueType.INT, lambda b: (97, 0)),
        0x62: (ValueType.INT, lambda b: (98, 0)),
        0x63: (ValueType.INT, lambda b: (99, 0)),
        0x64: (ValueType.INT, lambda b: (100, 0)),
        0x65: (ValueType.INT, lambda b: (101, 0)),
        0x66: (ValueType.INT, lambda b: (102, 0)),
        0x67: (ValueType.INT, lambda b: (103, 0)),
        0x68: (ValueType.INT, lambda b: (104, 0)),
        0x69: (ValueType.INT, lambda b: (105, 0)),
        0x6A: (ValueType.INT, lambda b: (106, 0)),
        0x6B: (ValueType.INT, lambda b: (107, 0)),
        0x6C: (ValueType.INT, lambda b: (108, 0)),
        0x6D: (ValueType.INT, lambda b: (109, 0)),
        0x6E: (ValueType.INT, lambda b: (110, 0)),
        0x6F: (ValueType.INT, lambda b: (111, 0)),
        0x70: (ValueType.INT, lambda b: (112, 0)),
        0x71: (ValueType.INT, lambda b: (113, 0)),
        0x72: (ValueType.INT, lambda b: (114, 0)),
        0x73: (ValueType.INT, lambda b: (115, 0)),
        0x74: (ValueType.INT, lambda b: (116, 0)),
        0x75: (ValueType.INT, lambda b: (117, 0)),
        0x76: (ValueType.INT, lambda b: (118, 0)),
        0x77: (ValueType.INT, lambda b: (119, 0)),
        0x78: (ValueType.INT, lambda b: (120, 0)),
        0x79: (ValueType.INT, lambda b: (121, 0)),
        0x7A: (ValueType.INT, lambda b: (122, 0)),
        0x7B: (ValueType.INT, lambda b: (123, 0)),
        0x7C: (ValueType.INT, lambda b: (124, 0)),
        0x7D: (ValueType.INT, lambda b: (125, 0)),
        0x7E: (ValueType.INT, lambda b: (126, 0)),
        0x7F: (ValueType.INT, lambda b: (127, 0)),

        0x80: (ValueType.MAP, lambda b: (0, 0)),
        0x81: (ValueType.MAP, lambda b: (1, 0)),
        0x82: (ValueType.MAP, lambda b: (2, 0)),
        0x83: (ValueType.MAP, lambda b: (3, 0)),
        0x84: (ValueType.MAP, lambda b: (4, 0)),
        0x85: (ValueType.MAP, lambda b: (5, 0)),
        0x86: (ValueType.MAP, lambda b: (6, 0)),
        0x87: (ValueType.MAP, lambda b: (7, 0)),
        0x88: (ValueType.MAP, lambda b: (8, 0)),
        0x89: (ValueType.MAP, lambda b: (9, 0)),
        0x8A: (ValueType.MAP, lambda b: (10, 0)),
        0x8B: (ValueType.MAP, lambda b: (11, 0)),
        0x8C: (ValueType.MAP, lambda b: (12, 0)),
        0x8D: (ValueType.MAP, lambda b: (13, 0)),
        0x8E: (ValueType.MAP, lambda b: (14, 0)),
        0x8F: (ValueType.MAP, lambda b: (15, 0)),

        0x90: (ValueType.ARRAY, lambda b: (0, 0)),
        0x91: (ValueType.ARRAY, lambda b: (1, 0)),
        0x92: (ValueType.ARRAY, lambda b: (2, 0)),
        0x93: (ValueType.ARRAY, lambda b: (3, 0)),
        0x94: (ValueType.ARRAY, lambda b: (4, 0)),
        0x95: (ValueType.ARRAY, lambda b: (5, 0)),
        0x96: (ValueType.ARRAY, lambda b: (6, 0)),
        0x97: (ValueType.ARRAY, lambda b: (7, 0)),
        0x98: (ValueType.ARRAY, lambda b: (8, 0)),
        0x99: (ValueType.ARRAY, lambda b: (9, 0)),
        0x9A: (ValueType.ARRAY, lambda b: (10, 0)),
        0x9B: (ValueType.ARRAY, lambda b: (11, 0)),
        0x9C: (ValueType.ARRAY, lambda b: (12, 0)),
        0x9D: (ValueType.ARRAY, lambda b: (13, 0)),
        0x9E: (ValueType.ARRAY, lambda b: (14, 0)),
        0x9F: (ValueType.ARRAY, lambda b: (15, 0)),

        0xA0: (ValueType.STR, lambda b: split_index(b, 0, 0)),
        0xA1: (ValueType.STR, lambda b: split_index(b, 0, 1)),
        0xA2: (ValueType.STR, lambda b: split_index(b, 0, 2)),
        0xA3: (ValueType.STR, lambda b: split_index(b, 0, 3)),
        0xA4: (ValueType.STR, lambda b: split_index(b, 0, 4)),
        0xA5: (ValueType.STR, lambda b: split_index(b, 0, 5)),
        0xA6: (ValueType.STR, lambda b: split_index(b, 0, 6)),
        0xA7: (ValueType.STR, lambda b: split_index(b, 0, 7)),
        0xA8: (ValueType.STR, lambda b: split_index(b, 0, 8)),
        0xA9: (ValueType.STR, lambda b: split_index(b, 0, 9)),
        0xAA: (ValueType.STR, lambda b: split_index(b, 0, 10)),
        0xAB: (ValueType.STR, lambda b: split_index(b, 0, 11)),
        0xAC: (ValueType.STR, lambda b: split_index(b, 0, 12)),
        0xAD: (ValueType.STR, lambda b: split_index(b, 0, 13)),
        0xAE: (ValueType.STR, lambda b: split_index(b, 0, 14)),
        0xAF: (ValueType.STR, lambda b: split_index(b, 0, 15)),
        0xB0: (ValueType.STR, lambda b: split_index(b, 0, 16)),
        0xB1: (ValueType.STR, lambda b: split_index(b, 0, 17)),
        0xB2: (ValueType.STR, lambda b: split_index(b, 0, 18)),
        0xB3: (ValueType.STR, lambda b: split_index(b, 0, 19)),
        0xB4: (ValueType.STR, lambda b: split_index(b, 0, 20)),
        0xB5: (ValueType.STR, lambda b: split_index(b, 0, 21)),
        0xB6: (ValueType.STR, lambda b: split_index(b, 0, 22)),
        0xB7: (ValueType.STR, lambda b: split_index(b, 0, 23)),
        0xB8: (ValueType.STR, lambda b: split_index(b, 0, 24)),
        0xB9: (ValueType.STR, lambda b: split_index(b, 0, 25)),
        0xBA: (ValueType.STR, lambda b: split_index(b, 0, 26)),
        0xBB: (ValueType.STR, lambda b: split_index(b, 0, 27)),
        0xBC: (ValueType.STR, lambda b: split_index(b, 0, 28)),
        0xBD: (ValueType.STR, lambda b: split_index(b, 0, 29)),
        0xBE: (ValueType.STR, lambda b: split_index(b, 0, 30)),
        0xBF: (ValueType.STR, lambda b: split_index(b, 0, 31)),

        0xC0: (ValueType.NIL, lambda b: (None, 0)),
        0xC2: (ValueType.BOOL, lambda b: (False, 0)),
        0xC3: (ValueType.BOOL, lambda b: (True, 0)),

        0xC4: (ValueType.BIN, lambda b: split_index(b, 1, struct.unpack('B', b[:1])[0])),
        0xC5: (ValueType.BIN, lambda b: split_index(b, 2, struct.unpack('>H', b[:2])[0])),
        0xC6: (ValueType.BIN, lambda b: split_index(b, 4, struct.unpack('>I', b[:4])[0])),

        MsgPackFormat.FLOAT32.value: (ValueType.FLOAT, lambda b: (struct.unpack('>f', b[:4])[0], 4)),
        MsgPackFormat.FLOAT64.value: (ValueType.FLOAT, lambda b: (struct.unpack('>d', b[:8])[0], 8)),
        MsgPackFormat.UINT8.value: (ValueType.INT, lambda b: (struct.unpack('>B', b[:1])[0], 1)),
        MsgPackFormat.UINT16.value: (ValueType.INT, lambda b: (struct.unpack('>H', b[:2])[0], 2)),
        MsgPackFormat.UINT32.value: (ValueType.INT, lambda b: (struct.unpack('>I', b[:4])[0], 4)),
        MsgPackFormat.UINT64.value: (ValueType.INT, lambda b: (struct.unpack('>Q', b[:8])[0], 8)),
        MsgPackFormat.INT8.value: (ValueType.INT, lambda b: (struct.unpack('>b', b[:1])[0], 1)),
        MsgPackFormat.INT16.value: (ValueType.INT, lambda b: (struct.unpack('>h', b[:2])[0], 2)),
        MsgPackFormat.INT32.value: (ValueType.INT, lambda b: (struct.unpack('>i', b[:4])[0], 4)),
        MsgPackFormat.INT64.value: (ValueType.INT, lambda b: (struct.unpack('>q', b[:8])[0], 8)),

        0xD9: (ValueType.STR, lambda b: split_index(b, 1, struct.unpack('B', b[:1])[0])),
        0xDA: (ValueType.STR, lambda b: split_index(b, 2, struct.unpack('>H', b[:2])[0])),
        0xDB: (ValueType.STR, lambda b: split_index(b, 4, struct.unpack('>I', b[:4])[0])),

        0xDC: (ValueType.ARRAY, lambda b: split_array(b, '>H', 2)),
        0xDD: (ValueType.ARRAY, lambda b: split_array(b, '>I', 4)),

        0xDE: (ValueType.MAP, lambda b: split_array(b, '>H', 2)),
        0xDF: (ValueType.MAP, lambda b: split_array(b, '>I', 4)),

        0xE0: (ValueType.INT, lambda b: (-32, 0)),
        0xE1: (ValueType.INT, lambda b: (-31, 0)),
        0xE2: (ValueType.INT, lambda b: (-30, 0)),
        0xE3: (ValueType.INT, lambda b: (-29, 0)),
        0xE4: (ValueType.INT, lambda b: (-28, 0)),
        0xE5: (ValueType.INT, lambda b: (-27, 0)),
        0xE6: (ValueType.INT, lambda b: (-26, 0)),
        0xE7: (ValueType.INT, lambda b: (-25, 0)),
        0xE8: (ValueType.INT, lambda b: (-24, 0)),
        0xE9: (ValueType.INT, lambda b: (-23, 0)),
        0xEA: (ValueType.INT, lambda b: (-22, 0)),
        0xEB: (ValueType.INT, lambda b: (-21, 0)),
        0xEC: (ValueType.INT, lambda b: (-20, 0)),
        0xED: (ValueType.INT, lambda b: (-19, 0)),
        0xEE: (ValueType.INT, lambda b: (-18, 0)),
        0xEF: (ValueType.INT, lambda b: (-17, 0)),
        0xF0: (ValueType.INT, lambda b: (-16, 0)),
        0xF1: (ValueType.INT, lambda b: (-15, 0)),
        0xF2: (ValueType.INT, lambda b: (-14, 0)),
        0xF3: (ValueType.INT, lambda b: (-13, 0)),
        0xF4: (ValueType.INT, lambda b: (-12, 0)),
        0xF5: (ValueType.INT, lambda b: (-11, 0)),
        0xF6: (ValueType.INT, lambda b: (-10, 0)),
        0xF7: (ValueType.INT, lambda b: (-9, 0)),
        0xF8: (ValueType.INT, lambda b: (-8, 0)),
        0xF9: (ValueType.INT, lambda b: (-7, 0)),
        0xFA: (ValueType.INT, lambda b: (-6, 0)),
        0xFB: (ValueType.INT, lambda b: (-5, 0)),
        0xFC: (ValueType.INT, lambda b: (-4, 0)),
        0xFD: (ValueType.INT, lambda b: (-3, 0)),
        0xFE: (ValueType.INT, lambda b: (-2, 0)),
        0xFF: (ValueType.INT, lambda b: (-1, 0)),
        }


class Packer:
    def __init__(self):
        self.payload=bytearray()

    def array(self, count):
        if count<=0xF:
            self.payload.append(MsgPackFormat.FIXARRAY.value + count)
        elif count<=0xFFFF:
            self.payload.append(MsgPackFormat.ARRAY16.value)
            self.payload.extend(struct.pack('>H', count))
        elif count<=0xFFFFFFFF:
            self.payload.append(MsgPackFormat.ARRAY32.value)
            self.payload.extend(struct.pack('>I', count))
        else:
            raise OverflowError('array %d' % count)

    def map(self, count):
        if count<=0xF:
            self.payload.append(MsgPackFormat.FIXMAP.value + count)
        elif count<=0xFFFF:
            self.payload.append(MsgPackFormat.MAP16.value)
            self.payload.extend(struct.pack('>H', count))
        elif count<=0xFFFFFFFF:
            self.payload.append(MsgPackFormat.MAP32.value)
            self.payload.extend(struct.pack('>I', count))
        else:
            raise OverflowError('map %d' % count)

    def pack(self, value):
        packed=pack(value)
        self.payload.extend(packed)


def pack(obj):
    if obj is None:
        return struct.pack('B', MsgPackFormat.NIL.value)
    elif obj is False:
        return struct.pack('B', MsgPackFormat.FALSE.value)
    elif obj is True:
        return struct.pack('B', MsgPackFormat.TRUE.value)
    elif isinstance(obj, int):
        if obj>=0:
            if obj<= 0x7f:
                return struct.pack('b', obj)
            elif obj<= 0xFF:
                return struct.pack('>BB', MsgPackFormat.UINT8.value, obj)
            elif obj<= 0xFFFF:
                return struct.pack('>BH', MsgPackFormat.UINT16.value, obj)
            elif obj<= 0xFFFFFFFF:
                return struct.pack('>BI', MsgPackFormat.UINT32.value, obj)
            elif obj<= 0xFFFFFFFFFFFFFFFF:
                return struct.pack('>BQ', MsgPackFormat.UINT64.value, obj)
            else:
                raise OverflowError('pack failed. %s' % obj)
        else:
            if obj>=-32:
                return struct.pack('B', 0x100 + obj)
            elif obj>= -128:
                return struct.pack('>Bb', MsgPackFormat.INT8.value, obj)
            elif obj>= -32768:
                return struct.pack('>Bh', MsgPackFormat.INT16.value, obj)
            elif obj>= -2147483648:
                return struct.pack('>Bi', MsgPackFormat.INT32.value, obj)
            elif obj>= -9223372036854775808:
                return struct.pack('>Bq', MsgPackFormat.INT64.value, obj)
            else:
                raise OverflowError('pack failed. %s' % obj)

    elif isinstance(obj, float):
        return struct.pack('>Bd', MsgPackFormat.FLOAT64.value, obj) 

    elif isinstance(obj, str):
        utf8=obj.encode('utf-8')
        utf8_len=len(utf8)
        if utf8_len==0:
            return struct.pack('B', MsgPackFormat.FIXSTR.value)
        elif utf8_len<32:
            return struct.pack('B%ds' % utf8_len, MsgPackFormat.FIXSTR.value+utf8_len, utf8)
        elif utf8_len<=0xFF:
            return struct.pack('BB%ds' % utf8_len, MsgPackFormat.STR8.value, utf8_len, utf8)
        elif utf8_len<=0xFFFF:
            return struct.pack('>BH%ds' % utf8_len, MsgPackFormat.STR16.value, utf8_len, utf8)
        elif utf8_len<=0xFFFFFFFF:
            return struct.pack('>BI%ds' % utf8_len, MsgPackFormat.STR32.value, utf8_len, utf8)
        else:
            raise OverflowError('pack failed. %s' % obj)

    elif isinstance(obj, bytes) or isinstance(obj, bytearray):
        bin_len=len(obj)
        if bin_len==0:
            return struct.pack('B', MsgPackFormat.BIN8.value)
        elif bin_len<=0xFF:
            return struct.pack('BB%ds' % bin_len, MsgPackFormat.BIN8.value, bin_len, obj)
        elif bin_len<=0xFFFF:
            return struct.pack('>BH%ds' % bin_len, MsgPackFormat.BIN16.value, bin_len, obj)
        elif bin_len<=0xFFFFFFFF:
            return struct.pack('>BI%ds' % bin_len, MsgPackFormat.BIN32.value, bin_len, obj)
        else:
            raise OverflowError('pack failed. %s' % obj)

    elif isinstance(obj, Parser):
        return obj.get_bytes()

    elif isinstance(obj, dict):
        packer=Packer()
        packer.map(len(obj))
        for k, v in obj.items():
            packer.pack(k)
            packer.pack(v)
        return packer.payload

    elif hasattr(obj, '__iter__'):
        packer=Packer()
        packer.array(len(obj))
        for x in obj:
            packer.pack(x)
        return packer.payload

    raise ValueError('unknown type. %s' % type(obj))


class Parser:
    __slots__=('bytedata', 'filled')
    def __init__(self, bytedata, filled=True):
        self.bytedata=bytedata
        self.filled=filled

    def get_type(self):
        head=self.bytedata[0]
        t, _=HEAD_MAP[head]
        return t

    def get(self):
        head=self.bytedata[0]
        t, value=HEAD_MAP[head]
        if t==ValueType.ARRAY:
            return [x.get() for x in self]
        elif t==ValueType.MAP:
            return {k.get(): v.get() for k, v in self.items()}
        else:
            x, _=value(self.bytedata[1:])
            if t==ValueType.STR:
                return x.decode('utf-8')
            else:
                return x

    def is_nil(self):
        head=self.bytedata[0]
        t, _=HEAD_MAP[head]
        if t==ValueType.NIL:
            return True
        return False

    def is_array(self):
        head=self.bytedata[0]
        t, _=HEAD_MAP[head]
        return t==ValueType.ARRAY

    def is_map(self):
        head=self.bytedata[0]
        t, _=HEAD_MAP[head]
        return t==ValueType.MAP

    def get_bool(self):
        head=self.bytedata[0]
        t, value=HEAD_MAP[head]
        if t==ValueType.BOOL:
            x, _=value(self.bytedata[1:])
            return x

        raise ValueError('is not bool. %s' % t)

    def get_int(self):
        head=self.bytedata[0]
        t, value=HEAD_MAP[head]
        if t==ValueType.INT:
            x, _=value(self.bytedata[1:])
            return x

        raise ValueError('is not int. %s' % t)

    def get_float(self):
        head=self.bytedata[0]
        t, value=HEAD_MAP[head]
        if t==ValueType.FLOAT:
            x, _=value(self.bytedata[1:])
            return x

        raise ValueError('is not float. %s' % t)

    def get_number(self):
        head=self.bytedata[0]
        t, value=HEAD_MAP[head]
        if t==ValueType.INT or t==ValueType.FLOAT:
            x, _=value(self.bytedata[1:])
            return x

        raise ValueError('is not number. %s' % t)

    def get_str(self):
        head=self.bytedata[0]
        t, value=HEAD_MAP[head]
        if t==ValueType.STR:
            x, _=value(self.bytedata[1:])
            return x.decode('utf-8')

        raise ValueError('is not str. %s' % t)

    def get_bin(self):
        head=self.bytedata[0]
        t, value=HEAD_MAP[head]
        if t==ValueType.BIN:
            x, _=value(self.bytedata[1:])
            return x

        raise ValueError('is not bin. %s' % t)

    def __len__(self):
        head=self.bytedata[0]
        t, value=HEAD_MAP[head]
        if t==ValueType.ARRAY or t==ValueType.MAP:
            count, _=value(self.bytedata[1:])
            return count

        raise ValueError('is not array or map. %s' % t)

    def __getitem__(self, index):
        if isinstance(index, int):
            for i, x in enumerate(self):
                if i==index:
                    return x
        else:
            for k, v in self.items():
                if k.get()==index:
                    return v

    def __iter__(self):
        head=self.bytedata[0]
        t, value=HEAD_MAP[head]
        if t==ValueType.ARRAY:
            count, offset=value(self.bytedata[1:])
            x=0
            current=None
            while x<count:
                if current is None:
                    current=Parser(self.bytedata[1+offset:])
                else:
                    current=current.next()
                yield current
                x+=1
        else:
            raise ValueError('is not array. %s' % t)

    def get_bytes(self):
        if self.filled:
            return self.bytedata

        head=self.bytedata[0]
        t, value=HEAD_MAP[head]
        count, offset=value(self.bytedata[1:])
        if t==ValueType.ARRAY:
            x=0
            pos=1+offset
            body=self.bytedata[pos:]
            current=Parser(body)
            while x<count:
                pos+=len(current.get_bytes())
                current=current.next()
                x+=1
            return self.bytedata[0:pos]
        elif t==ValueType.MAP:
            x=0
            pos=1+offset
            body=self.bytedata[pos:]
            current=Parser(body)
            while x<count:
                v=current.next()
                pos+=len(current.get_bytes())
                pos+=len(v.get_bytes())
                current=v.next()
                x+=1
            return self.bytedata[0:pos]
        else:
            return self.bytedata[0:1+offset]

    def next(self):
        head=self.bytedata[0]
        t, value=HEAD_MAP[head]
        count, offset=value(self.bytedata[1:])
        body=self.bytedata[1+offset:]
        if t==ValueType.ARRAY:
            x=0
            current=Parser(body)
            while x<count:
                current
                current=current.next()
                x+=1
            return current
        elif t==ValueType.MAP:
            x=0
            current=Parser(body)
            while x<count:
                v=current.next()
                current, v
                current=v.next()
                x+=1
            return current
        else:
            return Parser(body)

    def items(self):
        return MapIter(self)


class MapIter:
    def __init__(self, parser):
        if not parser.is_map():
            raise TypeError('is not map')
        self.bytedata=parser.bytedata

    def __iter__(self):
        head=self.bytedata[0]
        t, value=HEAD_MAP[head]
        count, offset=value(self.bytedata[1:])
        body=self.bytedata[1+offset:]
        x=0
        current=None
        while x<count:
            if current is None:
                current=Parser(body)
            else:
                current=v.next()
            v=current.next()
            yield current, v
            x+=1
