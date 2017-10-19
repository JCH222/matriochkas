# coding: utf8

import abc
from io import StringIO
from collections import deque


class StreamReader(metaclass=abc.ABCMeta):
    @abc.abstractmethod
    def read(self, parsing_pipeline):
        pass


class InMemoryStreamReader(StreamReader):
    def __init__(self, text_to_read):
        self.textToRead = text_to_read

    def read(self, parsing_pipeline):
        stream = StringIO(self.textToRead)
        min_position = parsing_pipeline.get_min_position()
        max_position = parsing_pipeline.get_max_position()
        length = max_position - min_position + 1
        current_position = -min_position
        ar_index = dict()
        element = deque(stream.read(length))

        while True:
            result = parsing_pipeline.check(element, ref_position=-min_position)
            if result is not None and result[0]:
                ar_index[current_position] = element[-min_position]
            next_character = stream.read(1)
            current_position += 1
            if next_character and result is not None:
                element.popleft()
                element.append(next_character)
            else:
                break

        stream.close()
        return ar_index
