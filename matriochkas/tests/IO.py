# coding: utf8

from matriochkas.core import IO
from matriochkas.core import ParsingEntities
from matriochkas.core import ModificationEntities
from matriochkas.core.Configuration import HandlersConfiguration
from matriochkas.core.Wrappers import ReadingWrappersHandler
from matriochkas.core.Wrappers import ClosingWrappersHandler
from io import StringIO
from collections import Counter
from time import sleep


########################################################################################################################


class InstanceStreamEntity(IO.StreamEntity):
    def __init__(self, name, *args, stream_class=None, read_method=None, write_method=None,
                 return_method=None, close_method=None, **kwargs):
        super(InstanceStreamEntity, self).__init__(*args, stream_class=stream_class, read_method=read_method, write_method=write_method,
                                                   return_method=return_method, close_method=close_method, **kwargs)
        self.name = name

    def launch(self):
        pass


########################################################################################################################

class SlowStringIO(StringIO):
    def __init__(self, *args, **kwargs):
        super(SlowStringIO, self).__init__(*args, **kwargs)

    def read(self, *args, **kwargs):
        sleep(0.1)
        result = super(SlowStringIO, self).read(*args, **kwargs)
        return result


########################################################################################################################

class SlowErrorStringIO(StringIO):
    def __init__(self, *args, **kwargs):
        super(SlowErrorStringIO, self).__init__(*args, **kwargs)
        self.counter = 0

    def read(self, *args, **kwargs):
        sleep(0.1)
        self.counter += 1
        if self.counter <= 100:
            return super(SlowErrorStringIO, self).read(*args, **kwargs)
        else:
            raise ValueError('Unit test error')

########################################################################################################################


def test_stream_entity():
    stream_entity = InstanceStreamEntity('entity 1')
    assert isinstance(stream_entity, IO.StreamEntity) is True
    assert (stream_entity.name == 'entity 1') is True
    assert stream_entity.streamClass is None
    assert stream_entity.readMethod is None
    assert stream_entity.writeMethod is None
    assert stream_entity.returnMethod is None
    assert (stream_entity.args == ()) is True
    assert (stream_entity.kwargs == {}) is True

    ###################################################################################################################

    stream_object = StringIO('abcdefghijklmnopqrstuvwxyz')
    method_1 = IO.StreamEntity.generate_method(stream_object, 'read_method')
    assert (str(type(method_1)) == "<class 'builtin_function_or_method'>") is True
    result = method_1(1)
    assert (result == 'a') is True
    result = method_1(3)
    assert (result == 'bcd') is True

    try:
        IO.StreamEntity.generate_method('Unknown object', 'read_method')
        assert False
    except ValueError:
        assert True

    method_3 = IO.StreamEntity.generate_method(stream_object, 'write_method')
    assert (str(type(method_3)) == "<class 'builtin_function_or_method'>") is True
    method_3("1")
    assert (stream_object.getvalue() == 'abcd1fghijklmnopqrstuvwxyz') is True
    method_3("234")
    assert (stream_object.getvalue() == 'abcd1234ijklmnopqrstuvwxyz') is True

    try:
        IO.StreamEntity.generate_method('Unknown object', 'write_method')
        assert False
    except ValueError:
        assert True

    method_5 = IO.StreamEntity.generate_method(stream_object, 'return_method')
    assert (str(type(method_5)) == "<class 'builtin_function_or_method'>") is True
    result = method_5()
    assert (result == 'abcd1234ijklmnopqrstuvwxyz') is True

    try:
        IO.StreamEntity.generate_method('Unknown object', 'return_method')
        assert False
    except ValueError:
        assert True

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
    assert (result.origin == ParsingEntities.ParsingResultOrigin.READING) is True
    assert (result.resultType == ParsingEntities.ParsingResultType.VALUE) is True
    assert isinstance(result.arInput['args'], tuple) is True
    assert (len(result.arInput['args']) == 1) is True
    assert (result.arInput['args'][0] == text) is True
    assert (result.arInput['kwargs'] == {}) is True
    assert (result.arIndex == [(26, ',', Counter({None: 2})), (55, ',', Counter({None: 2})),
                               (122, '.', Counter({None: 2})), (147, ',', Counter({None: 2})),
                               (230, '.', Counter({None: 2})), (333, '.', Counter({None: 2})),
                               (381, ',', Counter({None: 2})), (444, '.', Counter({None: 2}))]) is True

    stream_entity_2 = IO.StreamReader(text, result_type=ParsingEntities.ParsingResultType.REFERENCE, read_method='read',
                                      return_method='return', close_method='close')
    result_2 = stream_entity_2.read(pipeline)
    assert isinstance(result_2, ParsingEntities.ParsingResult) is True
    assert (result_2.streamClass == StringIO) is True
    assert (result_2.origin == ParsingEntities.ParsingResultOrigin.READING) is True
    assert isinstance(result_2.arInput['args'], tuple) is True
    assert (len(result_2.arInput['args']) == 0) is True
    assert isinstance(result_2.arInput['kwargs'], dict) is True
    assert (len(result_2.arInput['kwargs']) == 1) is True
    assert isinstance(result_2.arInput['kwargs']['reference'], StringIO) is True
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

    ###################################################################################################################

    HandlersConfiguration.reset_reading_wrapper()
    HandlersConfiguration.reset_closing_wrapper()

    pipeline_2 = (ParsingEntities.ParsingCondition('in') >> None) + None

    stream_entity_4 = IO.StreamReader(text, stream_class=SlowStringIO,
                                      result_type=ParsingEntities.ParsingResultType.REFERENCE, read_method='read',
                                      return_method='return', close_method='close', seek_method='seek')
    stream_entity_4.launch(pipeline)

    stream_entity_5 = IO.LinkedStreamReader(stream_entity_4.get_result())
    stream_entity_5.launch(pipeline_2)

    stream_entity_4.wait_initialization()
    stream_entity_5.wait_initialization()

    HandlersConfiguration.launch()

    sleep(10)
    stream_entity_6 = IO.LinkedStreamReader(stream_entity_4.get_result())
    stream_entity_6.launch(pipeline_2)

    sleep(10)
    assert stream_entity_4.get_result().arIndex == [(26, ',', Counter({None: 2})), (55, ',', Counter({None: 2})),
                                                    (122, '.', Counter({None: 2})), (147, ',', Counter({None: 2}))]
    assert stream_entity_5.get_result().arIndex == [(47, 'i', Counter({None: 2})), (79, 'i', Counter({None: 2})),
                                                   (136, 'i', Counter({None: 2}))]
    assert stream_entity_6.get_result().arIndex == [(37, 'i', Counter({None: 2}))]

    sleep(10)
    assert stream_entity_4.get_result().arIndex == [(26, ',', Counter({None: 2})), (55, ',', Counter({None: 2})),
                                                    (122, '.', Counter({None: 2})), (147, ',', Counter({None: 2})),
                                                    (230, '.', Counter({None: 2}))]
    assert stream_entity_5.get_result().arIndex == [(47, 'i', Counter({None: 2})), (79, 'i', Counter({None: 2})),
                                                    (136, 'i', Counter({None: 2})), (254, 'i', Counter({None: 2})),
                                                    (271, 'i', Counter({None: 2}))]
    assert stream_entity_6.get_result().arIndex == [(37, 'i', Counter({None: 2})), (155, 'i', Counter({None: 2})),
                                                    (172, 'i', Counter({None: 2}))]

    sleep(10)
    assert stream_entity_4.get_result().arIndex == [(26, ',', Counter({None: 2})), (55, ',', Counter({None: 2})),
                                                    (122, '.', Counter({None: 2})), (147, ',', Counter({None: 2})),
                                                    (230, '.', Counter({None: 2})), (333, '.', Counter({None: 2})),
                                                    (381, ',', Counter({None: 2}))]
    assert stream_entity_5.get_result().arIndex == [(47, 'i', Counter({None: 2})), (79, 'i', Counter({None: 2})),
                                                    (136, 'i', Counter({None: 2})), (254, 'i', Counter({None: 2})),
                                                    (271, 'i', Counter({None: 2})), (346, 'i', Counter({None: 2})),
                                                    (388, 'i', Counter({None: 2}))]
    assert stream_entity_6.get_result().arIndex == [(37, 'i', Counter({None: 2})), (155, 'i', Counter({None: 2})),
                                                    (172, 'i', Counter({None: 2})), (247, 'i', Counter({None: 2})),
                                                    (289, 'i', Counter({None: 2}))]

    sleep(10)
    assert stream_entity_4.get_result().arIndex == [(26, ',', Counter({None: 2})), (55, ',', Counter({None: 2})),
                                                    (122, '.', Counter({None: 2})), (147, ',', Counter({None: 2})),
                                                    (230, '.', Counter({None: 2})), (333, '.', Counter({None: 2})),
                                                    (381, ',', Counter({None: 2})), (444, '.', Counter({None: 2}))]
    assert stream_entity_5.get_result().arIndex == [(47, 'i', Counter({None: 2})), (79, 'i', Counter({None: 2})),
                                                    (136, 'i', Counter({None: 2})), (254, 'i', Counter({None: 2})),
                                                    (271, 'i', Counter({None: 2})), (346, 'i', Counter({None: 2})),
                                                    (388, 'i', Counter({None: 2}))]
    assert stream_entity_6.get_result().arIndex == [(37, 'i', Counter({None: 2})), (155, 'i', Counter({None: 2})),
                                                    (172, 'i', Counter({None: 2})), (247, 'i', Counter({None: 2})),
                                                    (289, 'i', Counter({None: 2}))]

    sleep(10)
    assert stream_entity_4.get_result().arIndex == [(26, ',', Counter({None: 2})), (55, ',', Counter({None: 2})),
                                                    (122, '.', Counter({None: 2})), (147, ',', Counter({None: 2})),
                                                    (230, '.', Counter({None: 2})), (333, '.', Counter({None: 2})),
                                                    (381, ',', Counter({None: 2})), (444, '.', Counter({None: 2}))]
    assert stream_entity_5.get_result().arIndex == [(47, 'i', Counter({None: 2})), (79, 'i', Counter({None: 2})),
                                                    (136, 'i', Counter({None: 2})), (254, 'i', Counter({None: 2})),
                                                    (271, 'i', Counter({None: 2})), (346, 'i', Counter({None: 2})),
                                                    (388, 'i', Counter({None: 2}))]
    assert stream_entity_6.get_result().arIndex == [(37, 'i', Counter({None: 2})), (155, 'i', Counter({None: 2})),
                                                    (172, 'i', Counter({None: 2})), (247, 'i', Counter({None: 2})),
                                                    (289, 'i', Counter({None: 2}))]

    ###################################################################################################################

    HandlersConfiguration.reset_reading_wrapper()
    HandlersConfiguration.reset_closing_wrapper()

    stream_entity_7 = IO.StreamReader(text, stream_class=SlowStringIO,
                                      result_type=ParsingEntities.ParsingResultType.REFERENCE, read_method='read',
                                      return_method='return', close_method='close', seek_method='seek')
    stream_entity_7.launch(pipeline)

    stream_entity_8 = IO.StreamReader(text, stream_class=SlowErrorStringIO,
                                      result_type=ParsingEntities.ParsingResultType.REFERENCE, read_method='read',
                                      return_method='return', close_method='close', seek_method='seek')
    stream_entity_8.launch(pipeline_2)

    stream_entity_7.wait_initialization()
    stream_entity_8.wait_initialization()

    HandlersConfiguration.launch()

    sleep(5)

    assert stream_entity_7.get_result().arIndex == [(26, ',', Counter({None: 2}))]
    assert stream_entity_8.get_result().arIndex == [(47, 'i', Counter({None: 2}))]

    sleep(55)

    assert stream_entity_7.get_result().arIndex == [(26, ',', Counter({None: 2})), (55, ',', Counter({None: 2})),
                                                    (122, '.', Counter({None: 2})), (147, ',', Counter({None: 2})),
                                                    (230, '.', Counter({None: 2})), (333, '.', Counter({None: 2})),
                                                    (381, ',', Counter({None: 2})), (444, '.', Counter({None: 2}))]
    try:
        stream_entity_8.get_result().arIndex
        assert False
    except ValueError:
        assert True


def test_linked_stream_reader():
    text = 'Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et ' \
           'dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ' \
           'ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu ' \
           'fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia ' \
           'deserunt mollit anim id est laborum. '
    parsing_result = ParsingEntities.ParsingResult(StringIO, ParsingEntities.ParsingResultOrigin.READING,
                                                   ParsingEntities.ParsingResultType.VALUE,
                                                   'read', 'write', 'return', 'close', 'seek', (text,), {}, [])
    pipeline = ((ParsingEntities.ParsingCondition(', ') | ParsingEntities.ParsingCondition('. ')) >> None) + None
    stream_entity = IO.LinkedStreamReader(parsing_result)
    result = stream_entity.read(pipeline)
    assert isinstance(result, ParsingEntities.ParsingResult) is True
    assert (result.streamClass == StringIO) is True
    assert (result.origin == ParsingEntities.ParsingResultOrigin.READING) is True
    assert (result.resultType == ParsingEntities.ParsingResultType.VALUE) is True
    assert isinstance(result.arInput['args'], tuple) is True
    assert (len(result.arInput['args']) == 1) is True
    assert (result.arInput['args'][0] == text) is True
    assert (result.arInput['kwargs'] == {}) is True
    assert (result.arIndex == [(26, ',', Counter({None: 2})), (55, ',', Counter({None: 2})),
                               (122, '.', Counter({None: 2})), (147, ',', Counter({None: 2})),
                               (230, '.', Counter({None: 2})), (333, '.', Counter({None: 2})),
                               (381, ',', Counter({None: 2})), (444, '.', Counter({None: 2}))]) is True

    stream = StringIO(text)
    parsing_result = ParsingEntities.ParsingResult(StringIO, ParsingEntities.ParsingResultOrigin.READING,
                                                   ParsingEntities.ParsingResultType.REFERENCE,
                                                   'read', 'write', 'return', 'close', 'seek', tuple(),
                                                   {'reference': stream}, [])
    pipeline = ((ParsingEntities.ParsingCondition(', ') | ParsingEntities.ParsingCondition('. ')) >> None) + None
    stream_entity_2 = IO.LinkedStreamReader(parsing_result)
    result = stream_entity_2.read(pipeline)
    assert isinstance(result, ParsingEntities.ParsingResult) is True
    assert (result.streamClass == StringIO) is True
    assert (result.origin == ParsingEntities.ParsingResultOrigin.READING) is True
    assert (result.resultType == ParsingEntities.ParsingResultType.REFERENCE) is True
    assert isinstance(result.arInput['args'], tuple) is True
    assert (len(result.arInput['args']) == 0) is True
    assert isinstance(result.arInput['kwargs'], dict) is True
    assert (len(result.arInput['kwargs']) == 1) is True
    assert (result.arInput['kwargs']['reference'] == stream) is True
    assert (result.arIndex == [(26, ',', Counter({None: 2})), (55, ',', Counter({None: 2})),
                               (122, '.', Counter({None: 2})), (147, ',', Counter({None: 2})),
                               (230, '.', Counter({None: 2})), (333, '.', Counter({None: 2})),
                               (381, ',', Counter({None: 2})), (444, '.', Counter({None: 2}))]) is True

    ###################################################################################################################

    stream_object = stream_entity._get_stream_object()
    assert isinstance(stream_object, StringIO) is True
    assert (stream_object.getvalue() == text) is True

    stream_object_2 = stream_entity_2._get_stream_object()
    assert isinstance(stream_object_2, StringIO) is True
    assert (stream_object_2 == stream) is True


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
    parsing_result = ParsingEntities.ParsingResult(StringIO, ParsingEntities.ParsingResultOrigin.MODIFICATION,
                                                   ParsingEntities.ParsingResultType.VALUE,
                                                   'read', 'write', 'return', 'close', 'seek', [text], {},
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

    parsing_result_2 = ParsingEntities.ParsingResult(StringIO, ParsingEntities.ParsingResultOrigin.READING,
                                                     ParsingEntities.ParsingResultType.VALUE,
                                                     'read', 'write', 'return', 'close', 'seek', [text], {},
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
    stream_entity_3 = IO.StreamWriter()
    try:
        stream_entity_3.write(parsing_result_2)
        assert False
    except TypeError:
        assert True

    ###################################################################################################################

    HandlersConfiguration.reset_reading_wrapper()
    HandlersConfiguration.reset_closing_wrapper()

    stream_entity_2 = IO.StreamWriter()
    stream_entity_2.launch(parsing_result)

    HandlersConfiguration.launch()

    assert (stream_entity_2.get_result() == text_result) is True
