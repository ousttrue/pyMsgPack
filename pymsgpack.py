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


NUMBER_MAP={
        0x00: (ValueType.INT, lambda _: 0),
        0x01: (ValueType.INT, lambda _: 1),
        0x02: (ValueType.INT, lambda _: 2),
        0x03: (ValueType.INT, lambda _: 3),
        0x04: (ValueType.INT, lambda _: 4),
        0x05: (ValueType.INT, lambda _: 5),
        0x06: (ValueType.INT, lambda _: 6),
        0x07: (ValueType.INT, lambda _: 7),
        0x08: (ValueType.INT, lambda _: 8),
        0x09: (ValueType.INT, lambda _: 9),
        0x0A: (ValueType.INT, lambda _: 10),
        0x0B: (ValueType.INT, lambda _: 11),
        0x0C: (ValueType.INT, lambda _: 12),
        0x0D: (ValueType.INT, lambda _: 13),
        0x0E: (ValueType.INT, lambda _: 14),
        0x0F: (ValueType.INT, lambda _: 15),
        0x10: (ValueType.INT, lambda _: 16),
        0x11: (ValueType.INT, lambda _: 17),
        0x12: (ValueType.INT, lambda _: 18),
        0x13: (ValueType.INT, lambda _: 19),
        0x14: (ValueType.INT, lambda _: 20),
        0x15: (ValueType.INT, lambda _: 21),
        0x16: (ValueType.INT, lambda _: 22),
        0x17: (ValueType.INT, lambda _: 23),
        0x18: (ValueType.INT, lambda _: 24),
        0x19: (ValueType.INT, lambda _: 25),
        0x1A: (ValueType.INT, lambda _: 26),
        0x1B: (ValueType.INT, lambda _: 27),
        0x1C: (ValueType.INT, lambda _: 28),
        0x1D: (ValueType.INT, lambda _: 29),
        0x1E: (ValueType.INT, lambda _: 30),
        0x1F: (ValueType.INT, lambda _: 31),
        0x20: (ValueType.INT, lambda _: 32),
        0x21: (ValueType.INT, lambda _: 33),
        0x22: (ValueType.INT, lambda _: 34),
        0x23: (ValueType.INT, lambda _: 35),
        0x24: (ValueType.INT, lambda _: 36),
        0x25: (ValueType.INT, lambda _: 37),
        0x26: (ValueType.INT, lambda _: 38),
        0x27: (ValueType.INT, lambda _: 39),
        0x28: (ValueType.INT, lambda _: 40),
        0x29: (ValueType.INT, lambda _: 41),
        0x2A: (ValueType.INT, lambda _: 42),
        0x2B: (ValueType.INT, lambda _: 43),
        0x2C: (ValueType.INT, lambda _: 44),
        0x2D: (ValueType.INT, lambda _: 45),
        0x2E: (ValueType.INT, lambda _: 46),
        0x2F: (ValueType.INT, lambda _: 47),
        0x30: (ValueType.INT, lambda _: 48),
        0x31: (ValueType.INT, lambda _: 49),
        0x32: (ValueType.INT, lambda _: 50),
        0x33: (ValueType.INT, lambda _: 51),
        0x34: (ValueType.INT, lambda _: 52),
        0x35: (ValueType.INT, lambda _: 53),
        0x36: (ValueType.INT, lambda _: 54),
        0x37: (ValueType.INT, lambda _: 55),
        0x38: (ValueType.INT, lambda _: 56),
        0x39: (ValueType.INT, lambda _: 57),
        0x3A: (ValueType.INT, lambda _: 58),
        0x3B: (ValueType.INT, lambda _: 59),
        0x3C: (ValueType.INT, lambda _: 60),
        0x3D: (ValueType.INT, lambda _: 61),
        0x3E: (ValueType.INT, lambda _: 62),
        0x3F: (ValueType.INT, lambda _: 63),
        0x40: (ValueType.INT, lambda _: 64),
        0x41: (ValueType.INT, lambda _: 65),
        0x42: (ValueType.INT, lambda _: 66),
        0x43: (ValueType.INT, lambda _: 67),
        0x44: (ValueType.INT, lambda _: 68),
        0x45: (ValueType.INT, lambda _: 69),
        0x46: (ValueType.INT, lambda _: 70),
        0x47: (ValueType.INT, lambda _: 71),
        0x48: (ValueType.INT, lambda _: 72),
        0x49: (ValueType.INT, lambda _: 73),
        0x4A: (ValueType.INT, lambda _: 74),
        0x4B: (ValueType.INT, lambda _: 75),
        0x4C: (ValueType.INT, lambda _: 76),
        0x4D: (ValueType.INT, lambda _: 77),
        0x4E: (ValueType.INT, lambda _: 78),
        0x4F: (ValueType.INT, lambda _: 79),
        0x50: (ValueType.INT, lambda _: 80),
        0x51: (ValueType.INT, lambda _: 81),
        0x52: (ValueType.INT, lambda _: 82),
        0x53: (ValueType.INT, lambda _: 83),
        0x54: (ValueType.INT, lambda _: 84),
        0x55: (ValueType.INT, lambda _: 85),
        0x56: (ValueType.INT, lambda _: 86),
        0x57: (ValueType.INT, lambda _: 87),
        0x58: (ValueType.INT, lambda _: 88),
        0x59: (ValueType.INT, lambda _: 89),
        0x5A: (ValueType.INT, lambda _: 90),
        0x5B: (ValueType.INT, lambda _: 91),
        0x5C: (ValueType.INT, lambda _: 92),
        0x5D: (ValueType.INT, lambda _: 93),
        0x5E: (ValueType.INT, lambda _: 94),
        0x5F: (ValueType.INT, lambda _: 95),
        0x60: (ValueType.INT, lambda _: 96),
        0x61: (ValueType.INT, lambda _: 97),
        0x62: (ValueType.INT, lambda _: 98),
        0x63: (ValueType.INT, lambda _: 99),
        0x64: (ValueType.INT, lambda _: 100),
        0x65: (ValueType.INT, lambda _: 101),
        0x66: (ValueType.INT, lambda _: 102),
        0x67: (ValueType.INT, lambda _: 103),
        0x68: (ValueType.INT, lambda _: 104),
        0x69: (ValueType.INT, lambda _: 105),
        0x6A: (ValueType.INT, lambda _: 106),
        0x6B: (ValueType.INT, lambda _: 107),
        0x6C: (ValueType.INT, lambda _: 108),
        0x6D: (ValueType.INT, lambda _: 109),
        0x6E: (ValueType.INT, lambda _: 110),
        0x6F: (ValueType.INT, lambda _: 111),
        0x70: (ValueType.INT, lambda _: 112),
        0x71: (ValueType.INT, lambda _: 113),
        0x72: (ValueType.INT, lambda _: 114),
        0x73: (ValueType.INT, lambda _: 115),
        0x74: (ValueType.INT, lambda _: 116),
        0x75: (ValueType.INT, lambda _: 117),
        0x76: (ValueType.INT, lambda _: 118),
        0x77: (ValueType.INT, lambda _: 119),
        0x78: (ValueType.INT, lambda _: 120),
        0x79: (ValueType.INT, lambda _: 121),
        0x7A: (ValueType.INT, lambda _: 122),
        0x7B: (ValueType.INT, lambda _: 123),
        0x7C: (ValueType.INT, lambda _: 124),
        0x7D: (ValueType.INT, lambda _: 125),
        0x7E: (ValueType.INT, lambda _: 126),
        0x7F: (ValueType.INT, lambda _: 127),

        0xE0: (ValueType.INT, lambda _: -32),
        0xE1: (ValueType.INT, lambda _: -31),
        0xE2: (ValueType.INT, lambda _: -30),
        0xE3: (ValueType.INT, lambda _: -29),
        0xE4: (ValueType.INT, lambda _: -28),
        0xE5: (ValueType.INT, lambda _: -27),
        0xE6: (ValueType.INT, lambda _: -26),
        0xE7: (ValueType.INT, lambda _: -25),
        0xE8: (ValueType.INT, lambda _: -24),
        0xE9: (ValueType.INT, lambda _: -23),
        0xEA: (ValueType.INT, lambda _: -22),
        0xEB: (ValueType.INT, lambda _: -21),
        0xEC: (ValueType.INT, lambda _: -20),
        0xED: (ValueType.INT, lambda _: -19),
        0xEE: (ValueType.INT, lambda _: -18),
        0xEF: (ValueType.INT, lambda _: -17),
        0xF0: (ValueType.INT, lambda _: -16),
        0xF1: (ValueType.INT, lambda _: -15),
        0xF2: (ValueType.INT, lambda _: -14),
        0xF3: (ValueType.INT, lambda _: -13),
        0xF4: (ValueType.INT, lambda _: -12),
        0xF5: (ValueType.INT, lambda _: -11),
        0xF6: (ValueType.INT, lambda _: -10),
        0xF7: (ValueType.INT, lambda _: -9),
        0xF8: (ValueType.INT, lambda _: -8),
        0xF9: (ValueType.INT, lambda _: -7),
        0xFA: (ValueType.INT, lambda _: -6),
        0xFB: (ValueType.INT, lambda _: -5),
        0xFC: (ValueType.INT, lambda _: -4),
        0xFD: (ValueType.INT, lambda _: -3),
        0xFE: (ValueType.INT, lambda _: -2),
        0xFF: (ValueType.INT, lambda _: -1),

        MsgPackFormat.FLOAT32.value: (ValueType.FLOAT, lambda b: struct.unpack('>f', b)[0]),
        MsgPackFormat.FLOAT64.value: (ValueType.FLOAT, lambda b: struct.unpack('>d', b)[0]),
        MsgPackFormat.UINT8.value: (ValueType.INT, lambda b: struct.unpack('>B', b)[0]),
        MsgPackFormat.UINT16.value: (ValueType.INT, lambda b: struct.unpack('>H', b)[0]),
        MsgPackFormat.UINT32.value: (ValueType.INT, lambda b: struct.unpack('>I', b)[0]),
        MsgPackFormat.UINT64.value: (ValueType.INT, lambda b: struct.unpack('>Q', b)[0]),
        MsgPackFormat.INT8.value: (ValueType.INT, lambda b: struct.unpack('>b', b)[0]),
        MsgPackFormat.INT16.value: (ValueType.INT, lambda b: struct.unpack('>h', b)[0]),
        MsgPackFormat.INT32.value: (ValueType.INT, lambda b: struct.unpack('>i', b)[0]),
        MsgPackFormat.INT64.value: (ValueType.INT, lambda b: struct.unpack('>q', b)[0]),
}

BODY_MAP={
        0xA0: (ValueType.STR, lambda b: b''),
        0xA1: (ValueType.STR, lambda b: b[:1]),
        0xA2: (ValueType.STR, lambda b: b[:2]),
        0xA3: (ValueType.STR, lambda b: b[:3]),
        0xA4: (ValueType.STR, lambda b: b[:4]),
        0xA5: (ValueType.STR, lambda b: b[:5]),
        0xA6: (ValueType.STR, lambda b: b[:6]),
        0xA7: (ValueType.STR, lambda b: b[:7]),
        0xA8: (ValueType.STR, lambda b: b[:8]),
        0xA9: (ValueType.STR, lambda b: b[:9]),
        0xAA: (ValueType.STR, lambda b: b[:10]),
        0xAB: (ValueType.STR, lambda b: b[:11]),
        0xAC: (ValueType.STR, lambda b: b[:12]),
        0xAD: (ValueType.STR, lambda b: b[:13]),
        0xAE: (ValueType.STR, lambda b: b[:14]),
        0xAF: (ValueType.STR, lambda b: b[:15]),
        0xB0: (ValueType.STR, lambda b: b[:16]),
        0xB1: (ValueType.STR, lambda b: b[:17]),
        0xB2: (ValueType.STR, lambda b: b[:18]),
        0xB3: (ValueType.STR, lambda b: b[:19]),
        0xB4: (ValueType.STR, lambda b: b[:20]),
        0xB5: (ValueType.STR, lambda b: b[:21]),
        0xB6: (ValueType.STR, lambda b: b[:22]),
        0xB7: (ValueType.STR, lambda b: b[:23]),
        0xB8: (ValueType.STR, lambda b: b[:24]),
        0xB9: (ValueType.STR, lambda b: b[:25]),
        0xBA: (ValueType.STR, lambda b: b[:26]),
        0xBB: (ValueType.STR, lambda b: b[:27]),
        0xBC: (ValueType.STR, lambda b: b[:28]),
        0xBD: (ValueType.STR, lambda b: b[:29]),
        0xBE: (ValueType.STR, lambda b: b[:30]),
        0xBF: (ValueType.STR, lambda b: b[:31]),
        }


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
            fmt='B%ds' % utf8_len
            return struct.pack(fmt, MsgPackFormat.FIXSTR.value+utf8_len, utf8)

    raise NotImplementedError('pack failed. %s' % obj)


class Parser:
    def __init__(self, bytedata):
        self.bytedata=bytedata

    def is_nil(self):
        return self.bytedata[0]==MsgPackFormat.NIL.value

    def get_bool(self):
        head=self.bytedata[0]
        if head==MsgPackFormat.FALSE.value:
            return False
        elif head==MsgPackFormat.TRUE.value:
            return True

        raise NotImplementedError('parse failed. 0x%02x' % head)

    def get_number(self):
        head=self.bytedata[0]
        if head in NUMBER_MAP:
            return NUMBER_MAP[head][1](self.bytedata[1:])

        raise NotImplementedError('get_number failed. 0x%02x' % head)

    def get_str(self):
        head=self.bytedata[0]
        if head in BODY_MAP:
            body=BODY_MAP[head][1](self.bytedata[1:])
            return body.decode('utf-8')

