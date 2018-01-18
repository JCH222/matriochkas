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

    def remove(self, stream_reader):
        if stream_reader in self.arReadingEvent:
            self.arReadingEvent[stream_reader].set()
            self.arTriggerEvent[stream_reader].set()
            del self.arReadingEvent[stream_reader]
            del self.arTriggerEvent[stream_reader]

    def key_generator(self):
        ar_trigger_event_key = list(self.arTriggerEvent.keys())
        for key in ar_trigger_event_key:
            if key in self.arTriggerEvent:
                yield key
            else:
                yield None

    def run(self):
        while self.currentCharacter:
            for key in self.key_generator():
                if key is not None:
                    self.arTriggerEvent[key].wait()
            self.currentCharacter = self.nextCurrentCharacter
            self.nextCurrentCharacter = self.readMethod(1)
            for key in self.key_generator():
                if key is not None:
                    self.arTriggerEvent[key].clear()
                    self.arReadingEvent[key].set()


class ReadingWrappersHandler(Thread):
    def __init__(self):
        super(ReadingWrappersHandler, self).__init__()
        self.arWrapper = dict()

    def get_method(self, read_method, stream_reader):
        if read_method not in self.arWrapper:
            self.arWrapper[read_method] = ReadingWrapper(read_method)
        return self.arWrapper[read_method].get_method(stream_reader)

    def run(self):
        for key in self.arWrapper:
            self.arWrapper[key].start()


class ClosingWrapper(Thread):
    def __init__(self, close_method):
        super(ClosingWrapper, self).__init__()

    def get_method(self, stream_reader):
        pass

    def close(self, stream_reader):
        pass

    def remove(self, stream_reader):
        pass

    def run(self):
        pass
