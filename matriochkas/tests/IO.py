# coding: utf8

from matriochkas.core import IO
from matriochkas.core import ParsingEntities
from matriochkas.core import ModificationEntities
from io import StringIO
from collections import Counter


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

    try:
        InstanceStreamEntity('entity', None, {})
        assert False
    except TypeError:
        assert True

    try:
        InstanceStreamEntity('entity', [], None)
        assert False
    except TypeError:
        assert True

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
    method_3("1")
    assert (stream_object.getvalue() == 'abcd1fghijklmnopqrstuvwxyz') is True
    method_3("234")
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


def test_stream_reader():
    text = 'Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et ' \
       'dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ' \
       'ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu ' \
       'fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt ' \
       'mollit anim id est laborum. '
    pipeline = ((ParsingEntities.ParsingCondition(', ') | ParsingEntities.ParsingCondition('. ')) >> None) + None
    stream_entity = IO.StreamReader(text)
    result = stream_entity.read(pipeline)
    assert isinstance(result, ParsingEntities.ParsingResult) is True
    assert (result.streamClass == StringIO) is True
    assert isinstance(result.arInput['args'], tuple) is True
    assert (len(result.arInput['args']) == 1) is True
    assert (result.arInput['args'][0] == text) is True
    assert (result.arInput['kwargs'] == {}) is True
    assert (result.arIndex == [(26, ',', Counter({None: 2})), (55, ',', Counter({None: 2})),
                               (122, '.', Counter({None: 2})), (147, ',', Counter({None: 2})),
                               (230, '.', Counter({None: 2})), (333, '.', Counter({None: 2})),
                               (381, ',', Counter({None: 2})), (444, '.', Counter({None: 2}))]) is True

    stream_entity_2 = IO.StreamReader(text, read_method='read', return_method='return', close_method='close')
    result_2 = stream_entity_2.read(pipeline)
    assert isinstance(result_2, ParsingEntities.ParsingResult) is True
    assert (result_2.streamClass == StringIO) is True
    assert isinstance(result_2.arInput['args'], tuple) is True
    assert (len(result_2.arInput['args']) == 1) is True
    assert (result_2.arInput['args'][0] == text) is True
    assert (result_2.arInput['kwargs'] == {}) is True
    assert (result_2.arIndex == [(26, ',', Counter({None: 2})), (55, ',', Counter({None: 2})),
                               (122, '.', Counter({None: 2})), (147, ',', Counter({None: 2})),
                               (230, '.', Counter({None: 2})), (333, '.', Counter({None: 2})),
                               (381, ',', Counter({None: 2})), (444, '.', Counter({None: 2}))]) is True

    stream_entity_3 = IO.StreamReader('')
    try:
        stream_entity_3.read(pipeline)
        assert False
    except ValueError:
        assert True

    ###################################################################################################################

    stream_object = stream_entity_2._get_stream_object()
    assert isinstance(stream_object, StringIO) is True
    assert (stream_object.getvalue() == text) is True


def test_stream_writer():
    text = 'Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et ' \
           'dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ' \
           'ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu ' \
           'fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia ' \
           'deserunt mollit anim id est laborum. '
    text_result = 'Lorem ipsum dolor sit amet-consectetur adipiscing elit-sed do eiusmod tempor incididunt ut labore' \
                  ' et dolore magna aliqua-Ut enim ad minim veniam-quis nostrud exercitation ullamco laboris nisi ' \
                  'ut aliquip ex ea commodo consequat-Duis aute irure dolor in reprehenderit in voluptate velit esse' \
                  ' cillum dolore eu fugiat nulla pariatur-Excepteur sint occaecat cupidatat non proident-sunt in' \
                  ' culpa qui officia deserunt mollit anim id est laborum-'
    parsing_result = ParsingEntities.ParsingResult(StringIO, 'read', 'write', 'return', 'close', [text], {},
                                                   [(26, ''), (26, '-', ModificationEntities.ModificationSide.RIGHT),
                                                    (27, ''), (55, ''),
                                                    (55, '-', ModificationEntities.ModificationSide.RIGHT), (56, ''),
                                                    (122, ''), (122, '-', ModificationEntities.ModificationSide.RIGHT),
                                                    (123, ''), (147, ''),
                                                    (147, '-', ModificationEntities.ModificationSide.RIGHT), (148, ''),
                                                    (230, ''), (230, '-', ModificationEntities.ModificationSide.RIGHT),
                                                    (231, ''), (333, ''),
                                                    (333, '-', ModificationEntities.ModificationSide.RIGHT), (334, ''),
                                                    (381, ''), (381, '-', ModificationEntities.ModificationSide.RIGHT),
                                                    (382, ''), (444, ''),
                                                    (444, '-', ModificationEntities.ModificationSide.RIGHT), (445, '')])
    stream_entity = IO.StreamWriter()
    assert (stream_entity.write(parsing_result) == text_result) is True

    stream_entity_2 = IO.StreamWriter(write_method='write', return_method='getvalue', close_method='close')
    assert (stream_entity_2.write(parsing_result) == text_result) is True
