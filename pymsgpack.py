import struct


NIL=b'\xC0'

FALSE=b'\xC2'
TRUE=b'\xC3'


def pack(obj):
    if obj==None:
        return NIL
    elif obj==False:
        return FALSE
    elif obj==True:
        return TRUE
    else:
        raise RuntimeException()


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
        else:
            raise RuntimeException()

