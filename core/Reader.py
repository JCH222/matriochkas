# coding: utf8

from io import StringIO
from collections import deque


class StreamReader:
    def __init__(self, *args, stream_class=StringIO, **kwargs):
        self.streamClass = stream_class
        self.args = args
        self.kwargs = kwargs

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
                current_position += 1
                if next_character and result is not None:
                    element.popleft()
                    element.append(next_character)
                else:
                    break

            stream.close()
            return ar_index
        else:
            stream.close()
            raise ValueError("Not enough characters to parse : " + str(len(element)))
