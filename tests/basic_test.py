import unittest
import pymsgpack
import struct


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

    def _test_number(self, n, base):
        # pack
        packed=pymsgpack.pack(n)
        self.assertEquals(struct.pack('B', base+n), packed)

        # parse
        parsed=pymsgpack.Parser(packed)
        self.assertEquals(n, parsed.get_int())

    def test_int(self):
        # positive fixint
        for i in range(0, 0x80):
            self._test_number(i, 0)


if __name__ == '__main__':
    print(type(True))
    unittest.main()

