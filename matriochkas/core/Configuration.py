# coding: utf8

from enum import Enum


class StreamClassConfiguration(Enum):
    StringIO = {'read_method': 'read', 'write_method': 'write', 'return_method': 'getvalue', 'close_method': 'close',
                'seek_method': 'seek'}
    TextIOWrapper = {'read_method': 'read', 'write_method': 'write', 'return_method': 'None', 'close_method': 'close',
                     'seek_method': 'seek'}
