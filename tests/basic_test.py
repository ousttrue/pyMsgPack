# coding: utf-8
import unittest
import pymsgpack
import struct
import random
import string
import os


class TestPyMsgPack(unittest.TestCase):

    def test_nil(self):
        # pack
        packed=pymsgpack.pack(None)
        self.assertEqual(struct.pack('B', 0xc0), packed)
        # parse
        parsed=pymsgpack.Parser(packed)
        self.assertTrue(parsed.is_nil())

        self.assertEqual(1, len(parsed.get_bytes()))

    def test_false(self):
        # pack
        packed=pymsgpack.pack(False)
        self.assertEqual(struct.pack('B', 0xc2), packed)
        # parse
        parsed=pymsgpack.Parser(packed)
        self.assertFalse(parsed.get_bool())

    def test_true(self):
        # pack
        packed=pymsgpack.pack(True)
        self.assertEqual(struct.pack('B', 0xc3), packed)
        # parse
        parsed=pymsgpack.Parser(packed)
        self.assertTrue(parsed.get_bool())

    def test_number(self):
        #
        # positive fixint
        #
        def _positive(n):
            # pack
            packed=pymsgpack.pack(n)
            self.assertEqual(struct.pack('B', n), packed)
            # parse
            parsed=pymsgpack.Parser(packed)
            self.assertEqual(n, parsed.get_number())

        for i in range(0, 0x80):
            _positive(i)

        #
        # negative
        #
        def _negative(n):
            # pack
            packed=pymsgpack.pack(n)
            self.assertEqual(struct.pack('B', 0x100+n), packed)
            # parse
            parsed=pymsgpack.Parser(packed)
            self.assertEqual(n, parsed.get_number())

        self.assertEqual(0xFF, pymsgpack.pack(-1)[0])
        self.assertEqual(0xE0, pymsgpack.pack(-32)[0])
        for i in range(-32, 0):
            _negative(i)

        #
        # numbers
        #
        def _number(src, fmt, head):
            dst=struct.pack(fmt, head, src)
            packed=pymsgpack.pack(src)
            if isinstance(src, float):
                self.assertAlmostEqual(dst, packed)
            else:
                self.assertEqual(dst, packed)

            parsed=pymsgpack.Parser(packed)
            if isinstance(src, float):
                self.assertAlmostEqual(src, parsed.get_number())
            else:
                self.assertEqual(src, parsed.get_number())

        # float
        _number(1.2, '>Bd', 0xcb)

        # uint8
        _number(0x80, '>BB', 0xcc)
        # uint16
        _number(0x100, '>BH', 0xcd)
        # uint32
        _number(0x10000, '>BI', 0xce)
        # uint64
        _number(0x100000000, '>BQ', 0xcf)

        # int8
        _number(-32-1, '>Bb', 0xd0)
        # int16
        _number(-128-1, '>Bh', 0xd1)
        # int32
        _number(-32768-1, '>Bi', 0xd2)
        # int64
        _number(-2147483648-1, '>Bq', 0xd3)

    def test_str(self):
        def random_str(n):
            return ''.join([random.choice(string.ascii_letters + string.digits) for i in range(n)])

        def _test_str(src):
            # pack
            packed=pymsgpack.pack(src)
            utf8=src.encode('utf-8')
            fmt='B%ds' % len(utf8)
            self.assertEqual(struct.pack(fmt, 0xa0 + len(utf8), utf8), packed)
            # parse
            parsed=pymsgpack.Parser(packed)
            self.assertEqual(src, parsed.get_str())

        for i in range(1, 32):
            _test_str(random_str(i))

        def _test_longstr(src, fmt, head):
            # pack
            packed=pymsgpack.pack(src)
            utf8=src.encode('utf-8')
            utf8_len=len(utf8)
            _fmt=fmt % utf8_len
            self.assertEqual(struct.pack(_fmt, head, utf8_len, utf8), packed)
            # parse
            parsed=pymsgpack.Parser(packed)
            self.assertEqual(src, parsed.get_str())

        # str8
        _test_longstr(random_str(32), 'BB%ds', 0xd9)
        # str16
        _test_longstr(random_str(256), '>BH%ds', 0xda)
        # str32
        _test_longstr(random_str(65536), '>BI%ds', 0xdb)

    def test_bin(self):
        def _test_bin(src, fmt, head):
            # pack
            packed=pymsgpack.pack(src)
            src_len=len(src)
            _fmt=fmt % src_len
            self.assertEqual(struct.pack(_fmt, head, src_len, src), packed)
            # parse
            parsed=pymsgpack.Parser(packed)
            self.assertEqual(src, parsed.get_bin())

        # bin8
        _test_bin(os.urandom(32), 'BB%ds', 0xc4)
        # bin16
        _test_bin(os.urandom(256), '>BH%ds', 0xc5)
        # bin32
        _test_bin(os.urandom(65536), '>BI%ds', 0xc6)

    def test_array(self):

        def _test_array(src, dst):
            # pack
            packed=pymsgpack.pack(src)
            self.assertEqual(dst, bytes(packed))
            # parse
            parsed=pymsgpack.Parser(packed)
            self.assertTrue(parsed.is_array())
            self.assertEqual(src, parsed.get())

        # fixarray
        _test_array([1, 2, 3]
                , b'\x93\x01\x02\x03')
        _test_array([1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15]
                , b'\x9F\x01\x02\x03\x04\x05\x06\x07\x08\x09\x0A\x0B\x0C\x0D\x0E\x0F')

        # array16
        n=0xF+1
        _test_array([1 for x in range(n)]
                , b''.join([b'\xdc', struct.pack('>H', n), b'\x01'*n]))

        # array32
        n=0xFFFF+1
        _test_array([1 for x in range(n)]
                , b''.join([b'\xdd', struct.pack('>I', n), b'\x01'*n]))

    def test_map(self):

        def _test_map(src, dst):
            # pack
            packed=pymsgpack.pack(src)
            self.assertEqual(dst, packed)
            # parse
            parsed=pymsgpack.Parser(packed)
            self.assertTrue(parsed.is_map())
            self.assertEqual(src, parsed.get())

        # fixmap
        _test_map({
            'a': 1,
            'b': 2,
            'c': 3,
        }, b'\x83\xa1\x61\x01\xa1\x62\x02\xa1\x63\x03')

        # map16
        n=0xF+1
        b=bytearray(b'\xde')
        b.extend(struct.pack('>H', n))
        b.extend(b''.join([struct.pack('BB', x, x) for x in range(n)]))
        _test_map({x: x for x in range(n)}, b)

        # map32
        def _pack_short(n):
            if n<=0x7F:
                return struct.pack('BB', n, n)
            elif n<=0xFF:
                return b''.join([
                        b'\xcc',
                        struct.pack('B', n),
                        b'\xcc',
                        struct.pack('B', n)
                        ])
            elif n<=0xFFFF:
                return b''.join([
                        b'\xcd',
                        struct.pack('>H', n),
                        b'\xcd',
                        struct.pack('>H', n)
                        ])
            else:
                raise NotImplementedError()
        n=0xFFFF+1
        b=bytearray(b'\xdf')
        b.extend(struct.pack('>I', n))
        b.extend(b''.join([_pack_short(x) for x in range(n)]))
        #_test_map({x: x for x in range(n)}, b)

    def test_nest(self):

        src={
                'nest': {
                    'hoge': 1
                    }
        }

        # pack
        packed=pymsgpack.pack(src)
        #self.assertEqual(dst, packed)
        # parse
        parsed=pymsgpack.Parser(packed)
        #self.assertTrue(parsed.is_map())
        self.assertEqual(src, parsed.get())

    def test_construct(self):

        partial=pymsgpack.pack([1, 2, 3])
        parsed=pymsgpack.Parser(partial)
        packed=pymsgpack.pack({
            'key': parsed
            })

        self.assertEqual(b'\x81\xa3\x6b\x65\x79\x93\x01\x02\x03',bytes(packed))

if __name__ == '__main__':
    t=TestPyMsgPack()
    t.test_construct()
    #unittest.main(TestPyMsgPack.test_construct)
