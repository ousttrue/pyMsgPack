import unittest
import pymsgpack
import struct


class TestPyMsgPack(unittest.TestCase):

    def test_nil(self):
        packed=pymsgpack.pack(None)
        self.assertEquals(struct.pack('B', 0xc0), packed)


if __name__ == '__main__':
    unittest.main()

