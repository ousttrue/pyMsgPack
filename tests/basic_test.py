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



if __name__ == '__main__':
    unittest.main()

