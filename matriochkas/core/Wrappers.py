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
        self.error = None
        self.readMethod = read_method
        self.arReadingEvent = dict()
        self.arTriggerEvent = dict()
        self.currentCharacter = read_method(1)
        self.nextCurrentCharacter = read_method(1)
        self.arCollector = dict()

    def get_method(self, stream_reader):
        if stream_reader not in self.arReadingEvent:
            self.arReadingEvent[stream_reader] = Event()
            self.arReadingEvent[stream_reader].set()
            self.arTriggerEvent[stream_reader] = Event()
        return self.read

    def get_collector_method(self, stream_writer):
        if stream_writer not in self.arCollector:
            self.arCollector[stream_writer] = ReadingCollector()
            self.arCollector[stream_writer].add_character(self.currentCharacter)
            self.arCollector[stream_writer].add_character(self.nextCurrentCharacter)
        return self.arCollector[stream_writer].read

    def read(self, size, stream_reader):
        result = ''
        for i in range(0, size):
            self.arReadingEvent[stream_reader].wait()
            result += self.currentCharacter
            self.arReadingEvent[stream_reader].clear()
            self.arTriggerEvent[stream_reader].set()
            if not self.nextCurrentCharacter or self.error is not None:
                break

        if self.error is None:
            return result
        else:
            raise self.error

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
        try:
            while self.currentCharacter:
                for key in self.key_generator():
                    if key is not None:
                        self.arTriggerEvent[key].wait()
                self.currentCharacter = self.nextCurrentCharacter
                self.nextCurrentCharacter = self.readMethod(1)
                for key in self.arCollector:
                    self.arCollector[key].add_character(self.nextCurrentCharacter)
                for key in self.key_generator():
                    if key is not None:
                        self.arTriggerEvent[key].clear()
                        self.arReadingEvent[key].set()
        except Exception as error:
            self.error = error
            for key in  self.arTriggerEvent:
                self.arReadingEvent[key].set()
                self.arTriggerEvent[key].set()
            for key in self.arCollector:
                self.arCollector[key].close()


class ReadingCollector:
    def __init__(self):
        super(ReadingCollector, self).__init__()
        self.stream = str()
        self.waitingEvent = Event()
        self.isClosed = False

    def read(self, size):
        while len(self.stream) < size and self.isClosed is False:
            self.waitingEvent.clear()
            self.waitingEvent.wait()
        result = self.stream[0:size]
        self.stream = self.stream[size:]
        return result

    def add_character(self, character):
        if character:
            self.stream += character
            self.waitingEvent.set()
        else:
            self.close()

    def close(self):
        self.isClosed = True
        self.waitingEvent.set()


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
        self.error = None
        self.close_method = close_method
        self.arClosingEvent = dict()

    def get_method(self, stream_reader):
        if stream_reader not in self.arClosingEvent:
            self.arClosingEvent[stream_reader] = Event()
        return self.close

    def close(self, stream_reader):
        self.arClosingEvent[stream_reader].set()
        if self.error is not None:
            raise self.error

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
        try:
            for key in self.key_generator():
                if key is not None:
                    self.arClosingEvent[key].wait()
            self.close_method()
        except Exception as error:
            self.error = error
            for key in self.arClosingEvent:
                self.arClosingEvent[key].set()


class ClosingWrappersHandler(WrappersHandler):
    def __init__(self):
        super(ClosingWrappersHandler, self).__init__()

    def get_method(self, close_method, stream_reader):
        if close_method not in self.arWrapper:
            self.arWrapper[close_method] = ClosingWrapper(close_method)
        return self.arWrapper[close_method].get_method(stream_reader)
