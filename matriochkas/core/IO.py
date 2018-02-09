# coding: utf8

from io import StringIO
from collections import deque
from matriochkas.core.ModificationEntities import ModificationSide
from matriochkas.core.ParsingEntities import ParsingResult
from matriochkas.core.ParsingEntities import ParsingResultOrigin
from matriochkas.core.ParsingEntities import ParsingResultType
from matriochkas.core.Configuration import StreamClassConfiguration
from matriochkas.core.Configuration import HandlersConfiguration
from threading import Thread
from threading import Event

import abc
import copy


class StreamEntity(Thread, metaclass=abc.ABCMeta):
    @abc.abstractmethod
    def __init__(self, args, kwargs, stream_class=None, read_method=None, write_method=None,
                 return_method=None, close_method=None, seek_method=None):
        super(StreamEntity, self).__init__()
        self.streamClass = stream_class
        self.readMethod = read_method
        self.writeMethod = write_method
        self.returnMethod = return_method
        self.closeMethod = close_method
        self.seekMethod = seek_method
        self.isMultiThreading = False

        if isinstance(args, (list, tuple)):
            self.args = args
        else:
            raise TypeError('args has to be list or tuple object')

        if isinstance(kwargs, dict):
            self.kwargs = kwargs
        else:
            raise TypeError('args has to be dict object')

    @abc.abstractmethod
    def launch(self):
        pass

    def _get_stream_object(self):
        return self.streamClass(*self.args, **self.kwargs)

    @staticmethod
    def generate_method(stream_object, method_key):
        def none_return():
            return None

        stream_class_name = type(stream_object).__name__
        for configuration in StreamClassConfiguration:
            if configuration.name == stream_class_name:
                if configuration.value[method_key] != 'None':
                    return getattr(stream_object, configuration.value[method_key])
                else:
                    return none_return
        raise ValueError(stream_class_name + " class not found in StreamClassConfiguration enumeration : please define "
                                             "methods to use during parsing process")


class StreamReader(StreamEntity):
    def __init__(self, *args, stream_class=StringIO, result_type=ParsingResultType.VALUE, read_method=None,
                 return_method=None, close_method=None, seek_method=None, **kwargs):
        super(StreamReader, self).__init__(args, kwargs, stream_class=stream_class,
                                           read_method=read_method, return_method=return_method,
                                           close_method=close_method, seek_method=seek_method)
        if isinstance(result_type, ParsingResultType):
            self.resultType = result_type
        else:
            raise TypeError('Result type has to be ParsingResultType object')

        self._readArgs = dict()
        self._readResult = {'parsing_result': None, 'error': None}
        self._isInitialized = Event()

    def run(self):
        initial_read_method = None
        initial_close_method = None

        try:
            self._readArgs['parsing_pipeline'].reset()
            min_position = self._readArgs['parsing_pipeline'].get_min_position()
            max_position = self._readArgs['parsing_pipeline'].get_max_position()
            length = max_position - min_position + 1
            stream = self._get_stream_object()
            if self.readMethod is not None:
                read_method = getattr(stream, self.readMethod)
            else:
                read_method = StreamEntity.generate_method(stream, 'read_method')
            if self.closeMethod is not None:
                close_method = getattr(stream, self.closeMethod)
            else:
                close_method = StreamEntity.generate_method(stream, 'close_method')
            if self.seekMethod is not None:
                seek_method = getattr(stream, self.seekMethod)
            else:
                seek_method = StreamEntity.generate_method(stream, 'seek_method')

            if self.isMultiThreading is True:
                initial_read_method = read_method
                read_method = HandlersConfiguration.READING_WRAPPER.get_method(read_method, self)
                initial_close_method = close_method
                close_method = HandlersConfiguration.CLOSING_WRAPPER.get_method(close_method, self)

            current_position = -min_position
            ar_index = list()

            if self.resultType == ParsingResultType.VALUE:
                self._readResult = {'parsing_result': ParsingResult(self.streamClass, ParsingResultOrigin.READING,
                                                                   self.resultType, self.readMethod, self.writeMethod,
                                                                   self.returnMethod, self.closeMethod, self.seekMethod,
                                                                   self.args, self.kwargs, ar_index),
                                   'error': None}
            else:
                self._readResult = {'parsing_result': ParsingResult(self.streamClass, ParsingResultOrigin.READING,
                                                                   self.resultType, self.readMethod, self.writeMethod,
                                                                   self.returnMethod, self.closeMethod, self.seekMethod,
                                                                   tuple(), {'reference': stream}, ar_index),
                                   'error': None}

            self._isInitialized.set()

            if self.isMultiThreading is False:
                element = deque(read_method(length))
            else:
                element = deque(read_method(length, self))

            if len(element) == length:
                while True:
                    result = self._readArgs['parsing_pipeline'].check(element, ref_position=-min_position)
                    if result is not None and result[0][0]:
                        ar_index.append((current_position, element[-min_position], result[0][1]))
                    if self.isMultiThreading is False:
                        next_character = read_method(1)
                    else:
                        next_character = read_method(1, self)
                    if next_character and result is not None:
                        element.popleft()
                        element.append(next_character)
                    else:
                        break
                    current_position += 1

                if self.isMultiThreading is False:
                    if self._readArgs['close_stream']:
                        close_method()
                    else:
                        seek_method(0)
                else:
                    close_method(self)

                self._readArgs = dict()
            else:
                if self.isMultiThreading is False:
                    close_method()
                else:
                    close_method(self)
                self._readResult = {'parsing_result': None,
                                   'error': ValueError("Not enough characters to parse : " + str(len(element)))}
        except Exception as error:
            if hasattr(self, 'close_method'):
                if self.isMultiThreading is False:
                    close_method()
                else:
                    close_method(self)
            self._readResult = {'parsing_result': None,
                               'error': error}
            self._isInitialized.set()
        finally:
            if self.isMultiThreading is True:
                if initial_read_method is not None:
                    HandlersConfiguration.READING_WRAPPER.arWrapper[initial_read_method].remove(self)
                if initial_close_method is not None:
                    HandlersConfiguration.CLOSING_WRAPPER.arWrapper[initial_close_method].remove(self)

    def read(self, parsing_pipeline, close_stream=True):
        self._readArgs = {'parsing_pipeline': parsing_pipeline, 'close_stream': close_stream}
        self._readResult = {'parsing_result': None, 'error': None}

        self.run()
        if self._readResult['error'] is None:
            if self._readResult['parsing_result'] is not None:
                return self._readResult['parsing_result']
            else:
                raise NotImplementedError('Parsing result is not implemented and no errors are detected')
        else:
            raise self._readResult['error']

    def launch(self, parsing_pipeline):
        self.isMultiThreading = True
        self._readArgs = {'parsing_pipeline': parsing_pipeline, 'close_stream': False}
        self._readResult = {'parsing_result': None, 'error': None}
        self.start()

    def wait_initialization(self):
        self._isInitialized.wait()

    def get_result(self):
        self.wait_initialization()
        if self._readResult['error'] is None:
            return self._readResult['parsing_result']
        else:
            raise self._readResult['error']


class LinkedStreamReader(StreamReader):
    def __init__(self, parsing_result):
        if isinstance(parsing_result, ParsingResult):
            super(LinkedStreamReader, self).__init__(*parsing_result.arInput['args'],
                                                     **parsing_result.arInput['kwargs'],
                                                     stream_class=parsing_result.streamClass,
                                                     result_type=parsing_result.resultType,
                                                     read_method=parsing_result.readMethod,
                                                     return_method=parsing_result.returnMethod,
                                                     close_method=parsing_result.closeMethod,
                                                     seek_method=parsing_result.seekMethod)
        else:
            raise TypeError('Parsing result has to be ParsingResult object')

    def _get_stream_object(self):
        if self.resultType == ParsingResultType.VALUE:
            return self.streamClass(*self.args, **self.kwargs)
        else:
            return self.kwargs['reference']


class StreamWriter(StreamEntity):
    def __init__(self, *args, stream_class=StringIO, write_method=None, return_method=None, close_method=None,
                 seek_method=None, **kwargs):
        super(StreamWriter, self).__init__(args, kwargs, stream_class=stream_class, write_method=write_method,
                                           return_method=return_method, close_method=close_method,
                                           seek_method=seek_method)

        self.writeArgs = dict()
        self.writeResult = {'result': None, 'error': None}
        self._isFinished = Event()

    def run(self):
        try:
            if (isinstance(self.writeArgs['parsing_result'], ParsingResult) and
                        self.writeArgs['parsing_result'].origin == ParsingResultOrigin.MODIFICATION):
                input_parsing_result = copy.deepcopy(self.writeArgs['parsing_result'])
            else:
                raise TypeError('Parsing result has to be ParsingResult object with MODIFICATION origin')

            if self.writeArgs['stream_class'] is not None:
                input_parsing_result.streamClass = self.writeArgs['stream_class']
            if self.writeArgs['read_method'] is not None:
                input_parsing_result.readMethod = self.writeArgs['read_method']
            if self.writeArgs['return_method'] is not None:
                input_parsing_result.returnMethod = self.writeArgs['return_method']
            if self.writeArgs['close_method'] is not None:
                input_parsing_result.closeMethod = self.writeArgs['close_method']
            if self.writeArgs['seek_method'] is not None:
                input_parsing_result.seekMethod = self.writeArgs['seek_method']
            if self.writeArgs['args'] is not None:
                input_parsing_result.arInput['args'] = self.writeArgs['args']
            if self.writeArgs['kwargs'] is not None:
                input_parsing_result.arInput['kwargs'] = self.writeArgs['kwargs']

            if input_parsing_result.resultType == ParsingResultType.VALUE:
                input_stream = input_parsing_result.streamClass(*input_parsing_result.arInput['args'],
                                                                **input_parsing_result.arInput['kwargs'])
            else:
                input_stream = input_parsing_result.arInput['kwargs']['reference']

            if input_parsing_result.readMethod is not None:
                input_read_method = getattr(input_stream, input_parsing_result.readMethod)
            else:
                input_read_method = StreamEntity.generate_method(input_stream, 'read_method')
            if input_parsing_result.closeMethod is not None:
                input_close_method = getattr(input_stream, input_parsing_result.closeMethod)
            else:
                input_close_method = StreamEntity.generate_method(input_stream, 'close_method')
            if input_parsing_result.seekMethod is not None:
                input_seek_method = getattr(input_stream, input_parsing_result.seekMethod)
            else:
                input_seek_method = StreamEntity.generate_method(input_stream, 'seek_method')
            output_stream = self._get_stream_object()
            if self.writeMethod is not None:
                output_write_method = getattr(output_stream, self.writeMethod)
            else:
                output_write_method = StreamEntity.generate_method(output_stream, 'write_method')
            if self.returnMethod is not None:
                output_return_method = getattr(output_stream, self.returnMethod)
            else:
                output_return_method = StreamEntity.generate_method(output_stream, 'return_method')
            if self.closeMethod is not None:
                output_close_method = getattr(output_stream, self.closeMethod)
            else:
                output_close_method = StreamEntity.generate_method(output_stream, 'close_method')

            if self.isMultiThreading is True:
                input_read_method = HandlersConfiguration.READING_WRAPPER.get_collector_method(input_read_method, self)

            index = 0
            input_parsing_result_index = 0
            character = input_read_method(1)
            while character:
                is_ended = False
                left_side = None
                right_side = None
                other = None
                while is_ended is False:
                    if input_parsing_result_index < len(input_parsing_result.arIndex) and index == input_parsing_result.arIndex[input_parsing_result_index][0]:
                        if len(input_parsing_result.arIndex[input_parsing_result_index]) == 3:
                            if input_parsing_result.arIndex[input_parsing_result_index][2] == ModificationSide.LEFT:
                                left_side = input_parsing_result.arIndex[input_parsing_result_index]
                            else:
                                right_side = input_parsing_result.arIndex[input_parsing_result_index]
                        else:
                            other = input_parsing_result.arIndex[input_parsing_result_index]
                        input_parsing_result_index += 1
                    else:
                        if left_side is not None:
                            output_write_method(left_side[1])
                        if other is None:
                            output_write_method(character)
                        if right_side is not None:
                            output_write_method(right_side[1])
                        is_ended = True
                character = input_read_method(1)
                index += 1
            result = output_return_method()

            if self.isMultiThreading is False:
                if self.writeArgs['close_reading_stream']:
                    input_close_method()
                else:
                    input_seek_method(0)
            output_close_method()
            self.writeResult = {'result': result, 'error': None}
        except Exception as error:
            if self.isMultiThreading is False:
                if 'input_close_method' in locals():
                    input_close_method()
            if 'output_close_method' in locals():
                output_close_method()
            self.writeResult = {'result': None, 'error': error}
        finally:
            self._isFinished.set()

    def write(self, parsing_result, stream_class=None, read_method=None, return_method=None, close_method=None,
              seek_method=None, args=None, kwargs=None, close_reading_stream=True):
        self.writeArgs = {'parsing_result': parsing_result, 'stream_class': stream_class, 'read_method': read_method,
                          'return_method': return_method, 'close_method': close_method, 'seek_method': seek_method,
                          'args': args, 'kwargs': kwargs, 'close_reading_stream': close_reading_stream}
        self.writeResult = {'result': None, 'error': None}

        self.run()
        if self.writeResult['error'] is None:
            return self.writeResult['result']
        else:
            raise self.writeResult['error']

    def launch(self, parsing_result, stream_class=None, read_method=None, return_method=None, close_method=None,
               seek_method=None, args=None, kwargs=None):
        self.isMultiThreading = True
        self.writeArgs = {'parsing_result': parsing_result, 'stream_class': stream_class, 'read_method': read_method,
                          'return_method': return_method, 'close_method': close_method, 'seek_method': seek_method,
                          'args': args, 'kwargs': kwargs, 'close_reading_stream': None}
        self.writeResult = {'result': None, 'error': None}
        self.start()

    def get_result(self):
        self._isFinished.wait()
        if self.writeResult['error'] is None:
            return self.writeResult['result']
        else:
            raise self.writeResult['error']
