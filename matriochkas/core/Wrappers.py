# coding: utf8

from threading import Event
from threading import Thread


class ReadingWrapper(Thread):
    def __init__(self, read_method):
        super(ReadingWrapper, self).__init__()
        self.readMethod = read_method
        self.arReadingEvent = dict()
        self.arTriggerEvent = dict()
        self.currentCharacter = read_method(1)
        self.nextCurrentCharacter = read_method(1)

    def get_method(self, stream_reader):
        if stream_reader not in self.arReadingEvent:
            self.arReadingEvent[stream_reader] = Event()
            self.arReadingEvent[stream_reader].set()
            self.arTriggerEvent[stream_reader] = Event()
        return self.read

    def read(self, size, stream_reader):
        result = ''
        for i in range(0, size):
            self.arReadingEvent[stream_reader].wait()
            result += self.currentCharacter
            self.arReadingEvent[stream_reader].clear()
            self.arTriggerEvent[stream_reader].set()
            if not self.nextCurrentCharacter:
                break
        return result

    def run(self):
        while self.currentCharacter:
            for key in self.arTriggerEvent:
                self.arTriggerEvent[key].wait()
            self.currentCharacter = self.nextCurrentCharacter
            self.nextCurrentCharacter = self.readMethod(1)
            for key in self.arReadingEvent:
                self.arTriggerEvent[key].clear()
                self.arReadingEvent[key].set()
