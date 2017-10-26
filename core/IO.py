# coding: utf8

from io import StringIO
from collections import deque
from core.ModificationEntities import ModificationSide
from core.ParsingEntities import ParsingResult

import abc
import copy


class StreamEntity(metaclass=abc.ABCMeta):
    @abc.abstractmethod
    def __init__(self, args, kwargs, stream_class=None):
        self.streamClass = stream_class
        self.args = args
        self.kwargs = kwargs


class StreamReader(StreamEntity):
    def __init__(self, *args, stream_class=StringIO, **kwargs):
        super(StreamReader, self).__init__(args, kwargs, stream_class=stream_class)

    def read(self, parsing_pipeline):
        parsing_pipeline.reset()
        min_position = parsing_pipeline.get_min_position()
        max_position = parsing_pipeline.get_max_position()
        length = max_position - min_position + 1
        stream = self.streamClass(*self.args, **self.kwargs)
        current_position = -min_position
        ar_index = list()
        element = deque(stream.read(length))
        if len(element) == length:
            while True:
                result = parsing_pipeline.check(element, ref_position=-min_position)
                if result is not None and result[0]:
                    ar_index.append((current_position, element[-min_position]))
                next_character = stream.read(1)
                if next_character and result is not None:
                    element.popleft()
                    element.append(next_character)
                else:
                    break
                current_position += 1

            stream.close()
            return ParsingResult(self.streamClass, self.args, self.kwargs, -min_position, current_position, ar_index)
        else:
            stream.close()
            raise ValueError("Not enough characters to parse : " + str(len(element)))


class StreamWriter(StreamEntity):
    def __init__(self, *args, stream_class=StringIO, **kwargs):
        super(StreamWriter, self).__init__(args, kwargs, stream_class=stream_class)

    def write(self, parsing_result, stream_class=None, args=None, kwargs=None):
        input_parsing_result = copy.deepcopy(parsing_result)
        if stream_class is not None:
            input_parsing_result.streamClass = stream_class
        if args is not None:
            input_parsing_result.arInput['args'] = args
        if kwargs is not None:
            input_parsing_result.arInput['kwargs'] = kwargs

        input_stream = input_parsing_result.streamClass(*input_parsing_result.arInput['args'], **input_parsing_result.arInput['kwargs'])
        ouput_stream = self.streamClass(*self.args, **self.kwargs)

        index = 0
        input_parsing_result_index = 0
        character = input_stream.read(1)
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
                        ouput_stream.write(left_side[1])
                    if other is None:
                        ouput_stream.write(character)
                    if right_side is not None:
                        ouput_stream.write(right_side[1])
                    is_ended = True
            character = input_stream.read(1)
            index += 1
        result = ouput_stream.getvalue()

        input_stream.close()
        ouput_stream.close()
        return result
