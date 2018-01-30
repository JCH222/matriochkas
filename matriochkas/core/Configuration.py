# coding: utf8


"""
    Configuration module
    ====================

    This module contains classes required to configure parsing or modification operations.

    It contains 1 class:

    - StreamClassConfiguration
"""


from enum import Enum

from matriochkas.core.Wrappers import ReadingWrappersHandler
from matriochkas.core.Wrappers import ClosingWrappersHandler


class StreamClassConfiguration(Enum):
    """
        Configuration streams class
        ===========================

        This enumeration provides stream presets to use with classes from IO's module.

        Each value in the enumeration represents a preset for a specific stream class.
        A StreamClassConfiguration's value has to be the chosen stream class name and contains a dictionary containing
        method names (str) called by the classes from IO's module for a specific condition.

        There are currently 5 conditions represented by 5 keys (str):

        - 'read_method' : get next characters from the stream
        - 'write_method' : send characters to the stream
        - 'return_method' : get result of the stream
        - 'close_method' : close the stream
        - 'seek_method' : move the stream position

        New stream class presets can be added but selected methods must follow certain rules:

        - 'read_method' :

            - method(int : characters_nb_to_read) returns str : read_characters
            - if the stream doesn't have enough characters to read returns remaining characters:

                :Example:

                >>> from io import StringIO
                >>> stream_object = StringIO('a')
                >>> stream_object.read(2)
                'a'
                >>> stream_object.read(2)
                ''
        - 'write_method' :

            - method(str : characters_to_write)
        - 'return_method' :

            - method() returns any object (None included)
        - 'close_method' :

            - method()
        - 'seek_method' :

            - method(int : 0) moves at the beginning of the stream
    """

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
