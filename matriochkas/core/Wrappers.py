# coding: utf8

from threading import Event
from threading import Thread

import abc


class WrappersHandler(Thread, metaclass=abc.ABCMeta):
    def __init__(self):
        super(WrappersHandler, self).__init__()
        self.arWrapper = dict()

    def run(self):
        for key in self.arWrapper:
            self.arWrapper[key].start()

    @abc.abstractmethod
    def get_method(self):
        pass


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
            result = None
            if key in self.arTriggerEvent:
                result = key
            yield result

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


class ReadingWrappersHandler(WrappersHandler):
    def __init__(self):
        super(ReadingWrappersHandler, self).__init__()

    def get_method(self, read_method, stream_reader):
        if read_method not in self.arWrapper:
            self.arWrapper[read_method] = ReadingWrapper(read_method)
        return self.arWrapper[read_method].get_method(stream_reader)


class ClosingWrapper(Thread):
    def __init__(self, close_method):
        super(ClosingWrapper, self).__init__()
        self.close_method = close_method
        self.arClosingEvent = dict()

    def get_method(self, stream_reader):
        if stream_reader not in self.arClosingEvent:
            self.arClosingEvent[stream_reader] = Event()
        return self.close

    def close(self, stream_reader):
        self.arClosingEvent[stream_reader].set()

    def remove(self, stream_reader):
        if stream_reader in self.arClosingEvent:
            self.arClosingEvent[stream_reader].set()
            del self.arClosingEvent[stream_reader]

    def key_generator(self):
        ar_closing_event_key = list(self.arClosingEvent.keys())
        for key in ar_closing_event_key:
            result = None
            if key in self.arClosingEvent:
                result = key
            yield result

    def run(self):
        for key in self.key_generator():
            if key is not None:
                self.arClosingEvent[key].wait()
        self.close_method()


class ClosingWrappersHandler(WrappersHandler):
    def __init__(self):
        super(ClosingWrappersHandler, self).__init__()

    def get_method(self, close_method, stream_reader):
        if close_method not in self.arWrapper:
            self.arWrapper[close_method] = ClosingWrapper(close_method)
        return self.arWrapper[close_method].get_method(stream_reader)
