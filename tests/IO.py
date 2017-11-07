# coding: utf8

from core import IO
from io import StringIO

########################################################################################################################


class InstanceStreamEntity(IO.StreamEntity):
    def __init__(self, name, args, kwargs, stream_class=None, read_method=None, write_method=None, return_method=None):
        super(InstanceStreamEntity, self).__init__(args, kwargs, stream_class, read_method, write_method, return_method)
        self.name = name

########################################################################################################################


def test_stream_entity():
    stream_entity = InstanceStreamEntity('entity 1', [], {})
    assert isinstance(stream_entity, IO.StreamEntity) is True
    assert (stream_entity.name == 'entity 1') is True
    assert stream_entity.streamClass is None
    assert stream_entity.readMethod is None
    assert stream_entity.writeMethod is None
    assert stream_entity.returnMethod is None
    assert (stream_entity.args == []) is True
    assert (stream_entity.kwargs == {}) is True

    ###################################################################################################################

    stream_object = StringIO('abcdefghijklmnopqrstuvwxyz')
    method_1 = IO.StreamEntity.generate_method(stream_object, 'read_method')
    assert (str(type(method_1)) == "<class 'builtin_function_or_method'>") is True
    result = method_1(1)
    assert (result == 'a') is True
    result = method_1(3)
    assert (result == 'bcd') is True

    method_2 = IO.StreamEntity.generate_method('Unknown object', 'read_method')
    assert method_2 is None

    method_3 = IO.StreamEntity.generate_method(stream_object, 'write_method')
    assert (str(type(method_3)) == "<class 'builtin_function_or_method'>") is True
    result = method_3("1")
    assert (stream_object.getvalue() == 'abcd1fghijklmnopqrstuvwxyz') is True
    result = method_3("234")
    assert (stream_object.getvalue() == 'abcd1234ijklmnopqrstuvwxyz') is True

    method_4 = IO.StreamEntity.generate_method('Unknown object', 'write_method')
    assert method_4 is None

    method_5 = IO.StreamEntity.generate_method(stream_object, 'return_method')
    assert (str(type(method_5)) == "<class 'builtin_function_or_method'>") is True
    result = method_5()
    assert (result == 'abcd1234ijklmnopqrstuvwxyz') is True

    method_6 = IO.StreamEntity.generate_method('Unknown object', 'return_method')
    assert method_6 is None

    stream_object.close()
    