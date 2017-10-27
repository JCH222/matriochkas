# coding: utf8

from enum import Enum


class StreamClassConfiguration(Enum):
    StringIO = {'read_method': 'read', 'write_method': 'write', 'return_method': 'getvalue'}
    TextIOWrapper = {'read_method': 'read', 'write_method': 'write', 'return_method': 'None'}
