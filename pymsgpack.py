import struct

NIL=b'\xC0'

def pack(obj):
    if obj==None:
        return NIL

