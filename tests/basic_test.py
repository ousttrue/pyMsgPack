import unittest
import pymsgpack
import struct
import random
import string


class TestPyMsgPack(unittest.TestCase):

    def test_nil(self):
        # pack
        packed=pymsgpack.pack(None)
        self.assertEquals(struct.pack('B', 0xc0), packed)
        # parse
        parsed=pymsgpack.Parser(packed)
        self.assertTrue(parsed.is_nil())

    def test_false(self):
        # pack
        packed=pymsgpack.pack(False)
        self.assertEquals(struct.pack('B', 0xc2), packed)
        # parse
        parsed=pymsgpack.Parser(packed)
        self.assertFalse(parsed.get_bool())

    def test_true(self):
        # pack
        packed=pymsgpack.pack(True)
        self.assertEquals(struct.pack('B', 0xc3), packed)
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
            self.assertEquals(struct.pack('B', n), packed)
            # parse
            parsed=pymsgpack.Parser(packed)
            self.assertEquals(n, parsed.get_number())

        for i in range(0, 0x80):
            _positive(i)

        #
        # negative
        #
        def _negative(n):
            # pack
            packed=pymsgpack.pack(n)
            self.assertEquals(struct.pack('B', 0x100+n), packed)
            # parse
            parsed=pymsgpack.Parser(packed)
            self.assertEquals(n, parsed.get_number())

        self.assertEquals(0xFF, pymsgpack.pack(-1)[0])
        self.assertEquals(0xE0, pymsgpack.pack(-32)[0])
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
            self.assertEquals(struct.pack(fmt, 0xa0 + len(utf8), utf8), packed)
            # parse
            parsed=pymsgpack.Parser(packed)
            self.assertEquals(src, parsed.get_str())

        for i in range(1, 32):
            _test_str(random_str(i))


if __name__ == '__main__':
    unittest.main()

