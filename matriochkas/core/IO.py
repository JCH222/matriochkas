# coding: utf8

from io import StringIO
from collections import deque
from matriochkas.core.ModificationEntities import ModificationSide
from matriochkas.core.ParsingEntities import ParsingResult
from matriochkas.core.Configuration import StreamClassConfiguration

import abc
import copy


class StreamEntity(metaclass=abc.ABCMeta):
    @abc.abstractmethod
    def __init__(self, args, kwargs, stream_class=None, read_method=None, write_method=None, return_method=None,
                 close_method=None):
        self.streamClass = stream_class
        self.readMethod = read_method
        self.writeMethod = write_method
        self.returnMethod = return_method
        self.closeMethod = close_method

        if isinstance(args, (list, tuple)):
            self.args = args
        else:
            raise TypeError('args has to be list or tuple object')

        if isinstance(kwargs, dict):
            self.kwargs = kwargs
        else:
            raise TypeError('args has to be dict object')

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
        return None


class StreamReader(StreamEntity):
    def __init__(self, *args, stream_class=StringIO, read_method=None, return_method=None, close_method=None, **kwargs):
        super(StreamReader, self).__init__(args, kwargs, stream_class=stream_class, read_method=read_method,
                                           return_method=return_method, close_method=close_method)

    def read(self, parsing_pipeline):
        parsing_pipeline.reset()
        min_position = parsing_pipeline.get_min_position()
        max_position = parsing_pipeline.get_max_position()
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
        current_position = -min_position
        ar_index = list()
        element = deque(read_method(length))
        if len(element) == length:
            while True:
                result = parsing_pipeline.check(element, ref_position=-min_position)
                if result is not None and result[0][0]:
                    ar_index.append((current_position, element[-min_position], result[0][1]))
                next_character = read_method(1)
                if next_character and result is not None:
                    element.popleft()
                    element.append(next_character)
                else:
                    break
                current_position += 1

            close_method()
            return ParsingResult(self.streamClass, self.readMethod, self.writeMethod, self.returnMethod,
                                 self.closeMethod, self.args, self.kwargs, ar_index)
        else:
            close_method()
            raise ValueError("Not enough characters to parse : " + str(len(element)))


class StreamWriter(StreamEntity):
    def __init__(self, *args, stream_class=StringIO, write_method=None, return_method=None, close_method=None,
                 **kwargs):
        super(StreamWriter, self).__init__(args, kwargs, stream_class=stream_class, write_method=write_method,
                                           return_method=return_method, close_method=close_method)

    def write(self, parsing_result, stream_class=None, read_method=None, return_method=None, close_method=None,
              args=None, kwargs=None):
        input_parsing_result = copy.deepcopy(parsing_result)
        if stream_class is not None:
            input_parsing_result.streamClass = stream_class
        if read_method is not None:
            input_parsing_result.readMethod = read_method
        if return_method is not None:
            input_parsing_result.returnMethod = return_method
        if close_method is not None:
            input_parsing_result.closeMethod = close_method
        if args is not None:
            input_parsing_result.arInput['args'] = args
        if kwargs is not None:
            input_parsing_result.arInput['kwargs'] = kwargs

        input_stream = input_parsing_result.streamClass(*input_parsing_result.arInput['args'],
                                                        **input_parsing_result.arInput['kwargs'])
        if input_parsing_result.readMethod is not None:
            input_read_method = getattr(input_stream, input_parsing_result.readMethod)
        else:
            input_read_method = StreamEntity.generate_method(input_stream, 'read_method')
        if input_parsing_result.closeMethod is not None:
            input_close_method = getattr(input_stream, input_parsing_result.closeMethod)
        else:
            input_close_method = StreamEntity.generate_method(input_stream, 'close_method')
        output_stream = self.streamClass(*self.args, **self.kwargs)
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

        index = 0
        input_parsing_result_index = 0
        character = input_read_method(1)
        while character:
            is_ended = False
            left_side = None
            right_side = None
            other = None
            while is_ended is False:
                if input_parsing_result_index < len(input_parsing_result.arIndex) \
                        and index == input_parsing_result.arIndex[input_parsing_result_index][0]:
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

        input_close_method()
        output_close_method()
        return result
