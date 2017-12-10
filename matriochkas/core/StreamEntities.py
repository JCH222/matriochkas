# coding: utf8

from collections import deque


class StreamTwoDimDeque:
    def __init__(self, column_separator, row_separator, initial_deque=None):
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
        return self.currentDeque

    def close(self):
        self.seek(0)

    def seek(self, position):
        self.currentRow = 0
        self.currentColumn = 0
        self.currentPosition = 0
        self.buffer = str()
        if position > 0:
            for i in range(0, position):
                self.read(1)
        elif position < 0:
            raise ValueError()
