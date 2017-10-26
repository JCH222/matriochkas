# coding: utf8

from io import StringIO
from collections import deque

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
    def __init__(self, *args, stream_class=None, **kwargs):
        super(StreamReader, self).__init__(args, kwargs, stream_class=stream_class)

    def write(self, parsing_result, parsing_class=None, args=None, kwargs=None):
        input_parsing_result = copy.deepcopy(parsing_result)
        if parsing_class is not None:
            input_parsing_result.parsing_class = parsing_class
        if args is not None:
            input_parsing_result.arInput['args'] = args
        if kwargs is not None:
            input_parsing_result.arInput['kwargs'] = kwargs

        input_stream = input_parsing_result.streamClass(*input_parsing_result.args, **input_parsing_result.kwargs)

        #TO DO

        return ParsingResult(self.streamClass, self.args, self.kwargs, 0, 0, {})


class ParsingResult:
    def __init__(self, parsing_class, args, kwargs, initial_character_index, final_character_index, ar_index):
        self.parsingClass = parsing_class
        self.arInput = {'args': args, 'kwargs': kwargs}
        self.initialCharacterIndex = initial_character_index
        self.finalCharacterIndex = final_character_index
        self.arIndex = ar_index

    def __add__(self, other):
        if isinstance(other, ParsingResult) and isinstance(self, ParsingResult):
            if ParsingResult.are_from_the_same_parsing(self, other):
                new_parsing_result = copy.deepcopy(self)
                for element in other.arIndex:
                    if element[0] not in new_parsing_result.arIndex:
                        new_parsing_result.arIndex.append(element)
                new_parsing_result.arIndex.sort()
                return new_parsing_result
            else:
                ValueError("Operands have to come from the same parsing")
        else:
            TypeError("Operands have to be ParsingResult classes or subclasses")

    def __contains__(self, item):
        if isinstance(item, int):
            for element in self:
                if element[0] == item:
                    return True
            return False
        else:
            for element in self:
                if element[0] == item[0] and element[1] == item[1]:
                    return True
            return False

    def __str__(self):
        return str({'Stream class': str(self.parsingClass.__name__), 'Inputs': str(self.arInput),
                    'from': str(self.initialCharacterIndex), 'to': str(self.finalCharacterIndex),
                    'Index result': str(self.arIndex)})

    def __repr__(self):
        return 'Parsing result :' + '\n' + '   Stream class : ' + str(self.parsingClass.__name__) + '\n' \
               + '   Inputs : ' + str(self.arInput) + '\n' + '   Parsed from character ' \
               + str(self.initialCharacterIndex) + ' to character ' + str(self.finalCharacterIndex) + '\n' \
               + '   Index result : ' + str(self.arIndex)

    def __copy__(self):
        return ParsingResult(self.parsingClass, self.arInput['args'], self.arInput['kwargs'],
                             self.initialCharacterIndex, self.finalCharacterIndex, self.arIndex)

    def __deepcopy__(self, memodict={}):
        copy_ar_input = copy.deepcopy(self.arInput)
        copy_ar_index = copy.deepcopy(self.arIndex)
        return ParsingResult(self.parsingClass, copy_ar_input['args'], copy_ar_input['kwargs'],
                             self.initialCharacterIndex, self.finalCharacterIndex, copy_ar_index)

    @staticmethod
    def are_from_the_same_parsing(parsing_result_a, parsing_result_b):
        if isinstance(parsing_result_a, ParsingResult) and isinstance(parsing_result_b, ParsingResult):
            return (parsing_result_a.parsingClass == parsing_result_b.parsingClass and
                    parsing_result_a.arInput['args'] == parsing_result_b.arInput['args'] and
                    parsing_result_a.arInput['kwargs'] == parsing_result_b.arInput['kwargs'] and
                    parsing_result_a.initialCharacterIndex == parsing_result_b.initialCharacterIndex and
                    parsing_result_a.finalCharacterIndex == parsing_result_b.finalCharacterIndex)
        else:
            TypeError("Operands have to be ParsingResult classes or subclasses")

    def check_indexes(self):
        previous_index_value = -1
        for index in self.arIndex:
            if index[0] > previous_index_value:
                previous_index_value = index[0]
            else:
                raise ValueError('Indexes have to be sorted in ascending order')
            if len(index[1]) != 1 or not isinstance(index[1], str):
                raise TypeError("Indexes's characters have to be 'str' objects with a length of 1")

        return True
