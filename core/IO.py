# coding: utf8

from io import StringIO
from collections import deque
from core.ModificationEntities import ModificationSide
from core.ParsingEntities import ParsingResult
from core.Configuration import StreamClassConfiguration

import abc
import copy


class StreamEntity(metaclass=abc.ABCMeta):
    @abc.abstractmethod
    def __init__(self, args, kwargs, stream_class=None, read_method=None, write_method=None, return_method=None):
        self.streamClass = stream_class
        self.readMethod = read_method
        self.writeMethod = write_method
        self.returnMethod = return_method

        if isinstance(args, (list, tuple)):
            self.args = args
        else:
            raise TypeError('args has to be list or tuple object')

        if isinstance(kwargs, dict):
            self.kwargs = kwargs
        else:
            raise TypeError('args has to be dict object')

    @staticmethod
    def generate_method(stream_object, method_key):
        stream_class_name = type(stream_object).__name__
        for configuration in StreamClassConfiguration:
            if configuration.name == stream_class_name:
                if configuration.value[method_key] != 'None':
                    return getattr(stream_object, configuration.value[method_key])
                else:
                    return None
        return None


class StreamReader(StreamEntity):
    def __init__(self, *args, stream_class=StringIO, read_method=None, return_method=None, **kwargs):
        super(StreamReader, self).__init__(args, kwargs, stream_class=stream_class, read_method=read_method,
                                           return_method=return_method)

    def read(self, parsing_pipeline):
        parsing_pipeline.reset()
        min_position = parsing_pipeline.get_min_position()
        max_position = parsing_pipeline.get_max_position()
        length = max_position - min_position + 1
        stream = self.streamClass(*self.args, **self.kwargs)
        if self.readMethod is not None:
            read_method = getattr(stream, self.readMethod)
        else:
            read_method = StreamEntity.generate_method(stream, 'read_method')
        current_position = -min_position
        ar_index = list()
        element = deque(read_method(length))
        if len(element) == length:
            while True:
                result = parsing_pipeline.check(element, ref_position=-min_position)
                if result is not None and result[0]:
                    ar_index.append((current_position, element[-min_position]))
                next_character = read_method(1)
                if next_character and result is not None:
                    element.popleft()
                    element.append(next_character)
                else:
                    break
                current_position += 1

            stream.close()
            return ParsingResult(self.streamClass, self.readMethod, self.writeMethod, self.returnMethod, self.args,
                                 self.kwargs, ar_index)
        else:
            stream.close()
            raise ValueError("Not enough characters to parse : " + str(len(element)))


class StreamWriter(StreamEntity):
    def __init__(self, *args, stream_class=StringIO, write_method=None, return_method=None, **kwargs):
        super(StreamWriter, self).__init__(args, kwargs, stream_class=stream_class, write_method=write_method,
                                           return_method=return_method)

    def write(self, parsing_result, stream_class=None, read_method=None, args=None, kwargs=None):
        input_parsing_result = copy.deepcopy(parsing_result)
        if stream_class is not None:
            input_parsing_result.streamClass = stream_class
        if read_method is not None:
            input_parsing_result.readMethod = read_method
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
        output_stream = self.streamClass(*self.args, **self.kwargs)
        if self.writeMethod is not None:
            output_write_method = getattr(output_stream, self.writeMethod)
        else:
            output_write_method = StreamEntity.generate_method(output_stream, 'write_method')
        if self.returnMethod is not None:
            output_return_method = getattr(output_stream, self.returnMethod)
        else:
            output_return_method = StreamEntity.generate_method(output_stream, 'return_method')

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
        if output_return_method is None:
            return None
        else:
            result = output_return_method()

        input_stream.close()
        output_stream.close()
        return result
