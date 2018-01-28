# coding: utf8

from enum import Enum

from matriochkas.core.Wrappers import ReadingWrappersHandler
from matriochkas.core.Wrappers import ClosingWrappersHandler


class StreamClassConfiguration(Enum):
    StringIO = {'read_method': 'read', 'write_method': 'write', 'return_method': 'getvalue', 'close_method': 'close',
                'seek_method': 'seek'}
    TextIOWrapper = {'read_method': 'read', 'write_method': 'write', 'return_method': 'None', 'close_method': 'close',
                     'seek_method': 'seek'}
    StreamTwoDimDeque = {'read_method': 'read', 'write_method': 'write', 'return_method': 'get_value',
                         'close_method': 'close', 'seek_method': 'seek'}


class HandlersConfiguration:
    READING_WRAPPER = ReadingWrappersHandler()
    CLOSING_WRAPPER = ClosingWrappersHandler()

    @staticmethod
    def reset_reading_wrapper():
        if HandlersConfiguration.READING_WRAPPER.is_alive() is False:
            HandlersConfiguration.READING_WRAPPER = ReadingWrappersHandler()
        else:
            raise RuntimeError("Reading wrapper is still running, it can't be reseted")

    @staticmethod
    def reset_closing_wrapper():
        if HandlersConfiguration.CLOSING_WRAPPER.is_alive() is False:
            HandlersConfiguration.CLOSING_WRAPPER = ClosingWrappersHandler()
        else:
            raise RuntimeError("Closing wrapper is still running, it can't be reseted")

    @staticmethod
    def launch():
        HandlersConfiguration.READING_WRAPPER.start()
        HandlersConfiguration.CLOSING_WRAPPER.start()
