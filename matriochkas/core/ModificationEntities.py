# coding: utf8

from enum import Enum
from matriochkas.core.ParsingEntities import ParsingResult
from matriochkas.core.ParsingEntities import ParsingResultOrigin
from threading import Thread

import abc


class ModificationSide(Enum):
    LEFT = -1
    RIGHT = 1


class ModificationEntity(Thread, metaclass=abc.ABCMeta):
    def __init__(self):
        super(ModificationEntity, self).__init__()
        self._modificationArgs = dict()
        self._modificationResult = {'parsing_result': None, 'error': None}

    def __add__(self, other):
        if isinstance(self, ModificationEntity) and isinstance(other, ModificationEntity):
            modification_operator = ModificationOperator(self, other)
            return modification_operator
        else:
            raise TypeError("Operands have to be ModificationEntity's subclasses")

    def run(self):
        try:
            ar_index = list()

            self._modificationResult = {'parsing_result':
                                        ParsingResult(self._modificationArgs['initial_parsing_result'].streamClass,
                                                      ParsingResultOrigin.MODIFICATION,
                                                      self._modificationArgs['initial_parsing_result'].resultType,
                                                      self._modificationArgs['initial_parsing_result'].readMethod,
                                                      self._modificationArgs['initial_parsing_result'].writeMethod,
                                                      self._modificationArgs['initial_parsing_result'].returnMethod,
                                                      self._modificationArgs['initial_parsing_result'].closeMethod,
                                                      self._modificationArgs['initial_parsing_result'].seekMethod,
                                                      self._modificationArgs['initial_parsing_result'].arInput['args'],
                                                      self._modificationArgs['initial_parsing_result'].arInput['kwargs'],
                                                      ar_index),
                                        'error': None}

            for index in self.create_indexes_generator(self._modificationArgs['initial_parsing_result'],
                                                       self._modificationArgs['thread_ref'],
                                                       self._modificationArgs['sleep_time']):
                ar_index += index
        except Exception as error:
            self._modificationResult = {'parsing_result': None, 'error': error}

    def generate_parsing_result(self, initial_parsing_result):
        self._modificationArgs = {'initial_parsing_result': initial_parsing_result, 'thread_ref': None,
                                  'sleep_time': 0.5}
        self._modificationResult = {'parsing_result': None, 'error': None}
        self.run()

        if self._modificationResult['parsing_result'] is not None:
            self._modificationResult['parsing_result'].arIndex.sort()
            return self._modificationResult['parsing_result']
        else:
            raise self._modificationResult['error']

    @abc.abstractmethod
    def create_indexes_generator(self, initial_parsing_result, thread_ref=None, sleep_time=0.5):
        raise NotImplementedError('<create_indexes_generator> method has to be implemented')


class ModificationOperator(ModificationEntity):
    def __init__(self, operand_a, operand_b):
        super(ModificationOperator, self).__init__()
        self.operandA = operand_a
        self.operandB = operand_b

    def create_indexes_generator(self, initial_parsing_result, thread_ref=None, sleep_time=0.5):
        operator_a = self.operandA.create_indexes_generator(initial_parsing_result, thread_ref=thread_ref,
                                                            sleep_time=sleep_time)
        operator_b = self.operandB.create_indexes_generator(initial_parsing_result, thread_ref=thread_ref,
                                                            sleep_time=sleep_time)

        try:
            while True:
                index_a = operator_a.__next__()
                index_b = operator_b.__next__()
                yield index_a + index_b
        except StopIteration:
            raise StopIteration()


class ModificationOperation(ModificationEntity, metaclass=abc.ABCMeta):
    def __new__(cls, rel_position=0, key_word=None):
        if isinstance(rel_position, int):
            return super(ModificationOperation, cls).__new__(cls)
        elif isinstance(rel_position, list):
            ar_rel_position_size = len(rel_position)
            if ar_rel_position_size >= 1:
                result = ModificationOperation(rel_position=rel_position[0], key_word=key_word)
                for j, i in enumerate(rel_position):
                    if j > 0:
                        result = result + ModificationOperation(rel_position=i, key_word=key_word)
                return result
            else:
                raise ValueError("Relative position list is empty")
        else:
            raise TypeError('Relative position has to be int or list object')

    def __init__(self, rel_position=0, key_word=None):
        super(ModificationOperation, self).__init__()
        self.relPosition = rel_position
        if key_word is not None and not isinstance(key_word, list) and not isinstance(key_word, tuple):
            key_word = [key_word]
        self.keyWord = key_word

    def create_indexes_generator(self, initial_parsing_result):
        raise NotImplementedError('<create_indexes_generator> method has to be implemented')


class ModificationAdd(ModificationOperation):
    def __new__(cls, ar_character, rel_position=0, modification_side=ModificationSide.RIGHT, key_word=None):
        return super(ModificationAdd, cls).__new__(cls, rel_position, key_word)

    def __init__(self, ar_character, rel_position=0, modification_side=ModificationSide.RIGHT, key_word=None):
        super(ModificationAdd, self).__init__(rel_position=rel_position, key_word=key_word)
        self.ar_character = ar_character
        self.modificationSide = modification_side

    def create_indexes_generator(self, initial_parsing_result, thread_ref=None, sleep_time=0.5):
        if isinstance(initial_parsing_result, ParsingResult):
            for element in initial_parsing_result.create_stream_generator(thread_ref=thread_ref, sleep_time=sleep_time):
                if self.keyWord is None or any(key_word in element[2] for key_word in self.keyWord):
                    yield [(element[0] + self.relPosition, self.ar_character, self.modificationSide)]
                else:
                    yield []
        else:
            raise TypeError('Parameter has to be ParsingResult class or subclass')


class ModificationRemove(ModificationOperation):
    def __new__(cls, rel_position=0, key_word=None):
        return super(ModificationRemove, cls).__new__(cls, rel_position, key_word)

    def __init__(self, rel_position=0, key_word=None):
        super(ModificationRemove, self).__init__(rel_position=rel_position, key_word=key_word)

    def create_indexes_generator(self, initial_parsing_result, thread_ref=None, sleep_time=0.5):
        if isinstance(initial_parsing_result, ParsingResult):
            for element in initial_parsing_result.create_stream_generator(thread_ref=thread_ref, sleep_time=sleep_time):
                if self.keyWord is None or any(key_word in element[2] for key_word in self.keyWord):
                    yield [(element[0] + self.relPosition, '')]
                else:
                    yield []
        else:
            raise TypeError('Parameter has to be ParsingResult class or subclass')
