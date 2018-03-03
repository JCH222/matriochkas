# coding: utf8


"""
    Dedicated stream classes module
    ===============================

    This module contains stream classes to use with classes from IO module (see stream_class parameter in IO module).

    It contains 1 class:

    - StreamTwoDimDeque
"""


from collections import deque


class StreamTwoDimDeque:
    """
        Stream class to create or read two dimension array
        ==================================================

        This class enables to create custom parsing operation by combining different ParsingOperator's or
        ParsingCondition's instances (See ParsingCondition class).
    """

    def __init__(self, column_separator, row_separator, initial_deque=None):
        """
            Initialization

            :param column_separator: column separator character (str [size:1])
            :param row_separator: row separator character (str [size:1])
            :param initial_deque: initial deque to load (two dimension deque array)
        """

        self.columnSeparator = column_separator
        self.rowSeparator = row_separator
        if initial_deque is None:
            initial_deque = deque()
        self.currentDeque = initial_deque
        self.buffer = str()
        self.currentRow = 0
        if len(self.currentDeque) <= 0:
            self.currentDeque.append(deque())
        self.currentColumn = 0
        self.currentPosition = 0

    def read(self, size):
        """
            Reads the next characters.

            :Example:

            >>> from matriochkas import StreamTwoDimDeque
            >>> from collections import deque
            >>> # Initial deque array
            >>> array = deque([deque(['Lorem', 'ipsum']), deque(['Ut', 'enim', 'ad', 'minim'])])
            >>> # Creates StreamTwoDimDeque object
            >>> stream_object_a = StreamTwoDimDeque(column_separator=" ", row_separator=".", initial_deque=array)
            >>> stream_object_a.read(1)
            "L"
            >>> stream_object_a.read(10)
            "orem ipsum"
            >>> stream_object_a.read(16)
            ".Ut enim ad mini"
            >>> stream_object_a.read(100)
            "m"
            >>> stream_object_a.read(100)


            :param size: characters number to read (int)
            :return: read characters (str)
        """

        if size > 0:
            for i in range(size):
                if self.currentRow < len(self.currentDeque):
                    if self.currentColumn < len(self.currentDeque[self.currentRow]):
                        if self.currentPosition < len(self.currentDeque[self.currentRow][self.currentColumn]):
                            position = self.currentPosition
                            self.currentPosition += 1
                            self.buffer += self.currentDeque[self.currentRow][self.currentColumn][position]
                        else:
                            self.currentColumn += 1
                            self.currentPosition = 0
                            if self.currentColumn < len(self.currentDeque[self.currentRow]):
                                self.buffer += self.columnSeparator
                            elif self.currentRow + 1 < len(self.currentDeque):
                                self.currentPosition = 0
                                self.currentColumn = 0
                                self.currentRow += 1
                                self.buffer += self.rowSeparator
                    else:
                        self.currentRow += 1
                        self.currentColumn = 0
                        if self.currentRow < len(self.currentDeque):
                            self.buffer += self.rowSeparator
        else:
            raise ValueError

        if len(self.buffer) <= 0:
            return None
        else:
            result = self.buffer[0:size]
            self.buffer = self.buffer[size:-1]
            return result

    def write(self, ar_character):
        """
            Writes characters in the deque array.

            :Example:

            >>> from matriochkas import StreamTwoDimDeque
            >>> # Creates StreamTwoDimDeque object
            >>> stream_object_a = StreamTwoDimDeque(column_separator=" ", row_separator=".")
            >>> stream_object_a.write("Lorem ip")
            >>> stream_object_a.get_value()
            deque([deque(['Lorem', 'ip'])])
            >>> stream_object_a.write("sum.Ut enim ad min")
            >>> stream_object_a.get_value()
            deque([deque(['Lorem', 'ipsum']), deque(['Ut', 'enim', 'ad', 'min'])])
            >>> stream_object_a.write("im")
            >>> stream_object_a.get_value()
            deque([deque(['Lorem', 'ipsum']), deque(['Ut', 'enim', 'ad', 'minim'])])

            :param ar_character: characters to write
            :return: None
        """

        for character in ar_character:
            if len(self.buffer) >= 1:
                self.currentDeque[self.currentRow].pop()

            if character == self.columnSeparator:
                self.currentDeque[self.currentRow].append(self.buffer)
                self.buffer = str()
            elif character == self.rowSeparator:
                self.currentDeque[self.currentRow].append(self.buffer)
                self.buffer = str()
                self.currentDeque.append(deque())
                self.currentRow += 1
            else:
                self.buffer += character
                self.currentDeque[self.currentRow].append(self.buffer)

    def get_value(self):
        """
            Gets the deque array

            :return: deque array (two dimensions deque array)
        """

        return self.currentDeque

    def close(self):
        """
            Closes the stream.

            :return: None
        """

        self.seek(0)

    def seek(self, position):
        """
            Reposition the stream cursor.

            :param position: new stream cursor position
            :return: None
        """

        self.currentRow = 0
        self.currentColumn = 0
        self.currentPosition = 0
        self.buffer = str()
        if position > 0:
            for i in range(0, position):
                self.read(1)
        elif position < 0:
            raise ValueError()
