# coding: utf8

from io import StringIO
from collections import deque


class StreamEntity:
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

    def write(self, parsing_result):
        pass


class ParsingResult:
    def __init__(self, parsing_class, args, kwargs, initial_character_index, final_character_index, ar_index):
        self.parsingClass = parsing_class
        self.arInput = {'args': args, 'kwargs': kwargs}
        self.initialCharacterIndex = initial_character_index
        self.finalCharacterIndex = final_character_index
        self.arIndex = ar_index

    def __str__(self):
        return str({'Stream class': str(self.parsingClass.__name__), 'Inputs': str(self.arInput),
                    'from': str(self.initialCharacterIndex), 'to': str(self.finalCharacterIndex),
                    'Index result': str(self.arIndex)})

    def __repr__(self):
        return 'Parsing result :' + '\n' + '   Stream class : ' + str(self.parsingClass.__name__) + '\n' \
               + '   Inputs : ' + str(self.arInput) + '\n' + '   Parsed from character ' \
               + str(self.initialCharacterIndex) + ' to character ' + str(self.finalCharacterIndex) + '\n' \
               + '   Index result : ' + str(self.arIndex)

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

