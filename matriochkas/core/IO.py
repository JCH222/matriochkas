# coding: utf8


"""
    Parsing execution module
    ========================

    This module contains classes required to execute parsing operations.

    It contains 4 classes:

    - StreamEntity
    - StreamReader
    - LinkedStreamReader
    - StreamWriter
"""


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
    """
        Fundamental parsing execution class
        ===================================

        Superclass for all parsing execution classes from this module.
    """

    @abc.abstractmethod
    def __init__(self, *args, stream_class=None, read_method=None, write_method=None,
                 return_method=None, close_method=None, seek_method=None, **kwargs):
        """
            Initialization

            :param args: args used during the parsing stream object creation (without parameters names)
            :param stream_class: class used to create parsing stream object (class)
            :param read_method: method contained in the parsing stream class and used to read characters during parsing
            process (method of stream_class)
            :param write_method: method contained in the parsing stream class and used to write characters during
            parsing process (method of stream_class)
            :param return_method: method contained in the parsing stream class and used to return characters during
            parsing process (method of stream_class)
            :param close_method: method contained in the parsing stream class and used to close stream process
            (method of stream_class)
            :param seek_method: method contained in the parsing stream class and used for repositioning parsing cursor
             (method of stream_class)
            :param kwargs: args used during the parsing stream object creation (with parameters names)
        """

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
        """
            Executes parsing execution with thread mode.

            :return: None
        """

        pass

    def _get_stream_object(self):
        """
            Gets stream object used during parsing process

            :return: stream object (object)
        """

        return self.streamClass(*self.args, **self.kwargs)

    @staticmethod
    def generate_method(stream_object, method_key):
        """
            Generates methods from the stream object (see '[...]_method' parameters in __init__ method).

            :param stream_object: stream object (object)
            :param method_key: [...]_method parameter (str)
            :return: dedicated method (method) or None
        """

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
    """
        Reading parsing execution class
        ===============================

        Parsing execution class used for stream reading.
    """

    def __init__(self, *args, stream_class=StringIO, result_type=ParsingResultType.VALUE, read_method=None,
                 return_method=None, close_method=None, seek_method=None, **kwargs):
        """
            Initialization.

            :param args: args used during the parsing stream object creation (without parameters names)
            :param stream_class: class used to create parsing stream object (class)
            :param result_type: parsing result type used to create parsing result object during read method execution
            (See ParsingResultType class)
            :param read_method: method contained in the parsing stream class and used to read characters during parsing
            process (method of stream_class)
            :param return_method: method contained in the parsing stream class and used to return characters during
            parsing process (method of stream_class)
            :param close_method: method contained in the parsing stream class and used to close stream process
            (method of stream_class)
            :param seek_method: method contained in the parsing stream class and used for repositioning parsing cursor
             (method of stream_class)
            :param kwargs: args used during the parsing stream object creation (with parameters names)
        """

        super(StreamReader, self).__init__(*args, **kwargs, stream_class=stream_class,
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
        """
            Thread used to execute reading parsing process.

            :return: None
        """

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
        """
            Executes reading parsing process (sequential).

            :Example:

            >>> from matriochkas import StreamReader
            >>> from matriochkas import ParsingCondition
            >>> text = "a,b,c,d"
            >>> # Creates parsing pipeline
            >>> pipeline = (ParsingCondition(',') >> None) + None
            >>> # Creates stream reader object
            >>> reader = StreamReader(text)
            >>> # Executes parsing process
            >>> parsing_result = reader.read(pipeline)
            >>> parsing_result
            {'Stream class': 'StringIO', 'Origin': 'ParsingResultOrigin.READING',
            'Result type': 'ParsingResultType.VALUE', 'Inputs': "{'args': ('a,b,c,d',), 'kwargs': {}}",
            'Index result': "[(1, ',', Counter({None: 1})), (3, ',', Counter({None: 1})),
            (5, ',', Counter({None: 1}))]"}


            :param parsing_pipeline: parsing pipeline used during parsing process (ParsingPipeline object)
            :param close_stream: closes stream object at the end of the parsing process
            :return: ParsingResult object
        """

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
        """
            Executes reading parsing process (multi-thread).

            :param parsing_pipeline: parsing pipeline used during parsing process (ParsingPipeline object)
            :return: None
        """

        self.isMultiThreading = True
        self._readArgs = {'parsing_pipeline': parsing_pipeline, 'close_stream': False}
        self._readResult = {'parsing_result': None, 'error': None}
        self.start()

    def wait_initialization(self):
        """
            Waits the ParsingResult object initialization during parsing process.

            :return: None
        """

        self._isInitialized.wait()

    def get_result(self):
        """
            Gets ParsingResult object generated during parsing process.

            :return: ParsingResult object
        """

        self.wait_initialization()
        if self._readResult['error'] is None:
            return self._readResult['parsing_result']
        else:
            raise self._readResult['error']


class LinkedStreamReader(StreamReader):
    """
        Reading parsing execution class
        ===============================

        Parsing execution class used for stream reading.
    """

    def __init__(self, parsing_result):
        """
            Initialization.

            :param parsing_result: Parsing result object used to initialize StreamReader parameters (ParsingResult object)
        """

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
        """
            Gets stream object used during parsing process

            :return: stream object (object)
        """

        if self.resultType == ParsingResultType.VALUE:
            return self.streamClass(*self.args, **self.kwargs)
        else:
            return self.kwargs['reference']


class StreamWriter(StreamEntity):
    def __init__(self, *args, stream_class=StringIO, write_method=None, return_method=None, close_method=None,
                 seek_method=None, **kwargs):
        super(StreamWriter, self).__init__(*args, stream_class=stream_class, write_method=write_method,
                                           return_method=return_method, close_method=close_method,
                                           seek_method=seek_method, **kwargs)

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

            index_generator = input_parsing_result.create_stream_generator(thread_ref=self.writeArgs['thread_ref'],
                                                                           sleep_time=self.writeArgs['sleep_time'])
            try:
                index = index_generator.__next__()
            except StopIteration:
                index = None
            index_position = 0
            character = input_read_method(1)
            while character:
                is_ended = False
                left_side = None
                right_side = None
                other = None
                while is_ended is False:

                    if index is not None and index_position == index[0]:
                        if len(index) == 3:
                            if index[2] == ModificationSide.LEFT:
                                left_side = index
                            else:
                                right_side = index
                        else:
                            other = index
                        try:
                            index = index_generator.__next__()
                        except StopIteration:
                            index = None
                    else:
                        if left_side is not None:
                            output_write_method(left_side[1])
                        if other is None:
                            output_write_method(character)
                        if right_side is not None:
                            output_write_method(right_side[1])
                        is_ended = True
                character = input_read_method(1)
                index_position += 1
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
                          'args': args, 'kwargs': kwargs, 'close_reading_stream': close_reading_stream,
                          'thread_ref': None, 'sleep_time': 0.5}
        self.writeResult = {'result': None, 'error': None}

        self.run()
        if self.writeResult['error'] is None:
            return self.writeResult['result']
        else:
            raise self.writeResult['error']

    def launch(self, parsing_result, stream_class=None, read_method=None, return_method=None, close_method=None,
               seek_method=None, args=None, kwargs=None, thread_ref=None, sleep_time=0.5):
        self.isMultiThreading = True
        self.writeArgs = {'parsing_result': parsing_result, 'stream_class': stream_class, 'read_method': read_method,
                          'return_method': return_method, 'close_method': close_method, 'seek_method': seek_method,
                          'args': args, 'kwargs': kwargs, 'close_reading_stream': None, 'thread_ref': thread_ref,
                          'sleep_time': sleep_time}
        self.writeResult = {'result': None, 'error': None}
        self.start()

    def get_result(self):
        self._isFinished.wait()
        if self.writeResult['error'] is None:
            return self.writeResult['result']
        else:
            raise self.writeResult['error']
