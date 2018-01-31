# coding: utf8


"""
    Parsing result modification module
    ==================================

    This module contains classes required to create modification parsing result.

    Modification parsing results are used with StreamWriter objects (see ParsingResultOrigin)

    It contains 6 classes:

    - ModificationSide
    - ModificationEntity
    - ModificationOperator
    - ModificationOperation
    - ModificationAdd
    - ModificationRemove

    They can be classified in 2 groups:

        - Enumerations
        - Parsing results modification operations
"""


from enum import Enum
from matriochkas.core.ParsingEntities import ParsingResult
from matriochkas.core.ParsingEntities import ParsingResultOrigin
from threading import Thread

import abc


class ModificationSide(Enum):
    """
        Position in comparison to the parsing cursor during the parsing process
        =======================================================================

        Nothing more to say...
    """
    LEFT = -1
    RIGHT = 1


class ModificationEntity(Thread, metaclass=abc.ABCMeta):
    """
        Fundamental parsing result modification operation class
        =======================================================

        Superclass for all parsing results modification operations from this module.
    """

    def __init__(self):
        """
            Initialization
        """

        super(ModificationEntity, self).__init__()
        self._modificationArgs = dict()
        self._modificationResult = {'parsing_result': None, 'error': None}

    def __add__(self, other):
        """
            Associates two ModificationEntity objects.

            :param other: ModificationEntity object to associate
            :return: ModificationOperator object
        """

        if isinstance(self, ModificationEntity) and isinstance(other, ModificationEntity):
            modification_operator = ModificationOperator(self, other)
            return modification_operator
        else:
            raise TypeError("Operands have to be ModificationEntity's subclasses")

    def run(self):
        """
            Thread used to convert 'classic' parsing result (ParsingResultOrigin.READING) into modification parsing
            result (ParsingResultOrigin.MODIFICATION).

            :return: None
        """

        try:
            ar_index = list()
            if self._modificationArgs['thread_ref'] is not None:
                self._modificationArgs['thread_ref'].wait_initialization()

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

    def generate_parsing_result(self, initial_parsing_result, thread_ref=None, sleep_time=0.5):
        """
            Convert 'classic' parsing result (ParsingResultOrigin.READING) into modification parsing
            result (ParsingResultOrigin.MODIFICATION).

            :Example:

            >>> from matriochkas import ParsingResult
            >>> from matriochkas import ParsingResultOrigin
            >>> from matriochkas import ParsingResultType
            >>> from matriochkas import ModificationAdd
            >>> from matriochkas import ModificationRemove
            >>> from io import StringIO
            >>> from collections import Counter
            >>> # Input creation
            >>> ar_index = [(1, ',', Counter({None: 1})), (3, ',', Counter({'key': 1})), (3, 'c', Counter({None: 1}))]
            >>> # ParsingResult creation
            >>> result = ParsingResult(StringIO, ParsingResultOrigin.READING, ParsingResultType.VALUE, 'close', 'write',
             'getvalue', 'close', 'seek', [], {}, ar_index)
            >>> # Modification pattern creation
            >>> modification_pattern = ModificationAdd(';') + ModificationRemove()
            >>> modification_pattern.generate_parsing_result(result)
            Parsing result :
               Stream class : StringIO
               Origin : ParsingResultOrigin.MODIFICATION
               Result type : ParsingResultType.VALUE
               Inputs : {'args': [], 'kwargs': {}}
               Index result : [(1, ''), (1, ';', <ModificationSide.RIGHT: 1>), (3, ''), (3, ''),
               (3, ';', <ModificationSide.RIGHT: 1>), (3, ';', <ModificationSide.RIGHT: 1>)]

            :param initial_parsing_result: parsing result with origin is ParsingResultOrigin.READING
            :param thread_ref: thread used to define the end of iteration (Thread object or None)
            :param sleep_time: waiting duration if the current index array from intial parsing result is iterated and
            the thread reference is not finished (float)
            :return: the modification parsing result (ParsingResult object with origin is
            ParsingResultOrigin.MODIFICATION)
        """

        # Don't ask me why...
        from matriochkas.core.IO import StreamReader

        if thread_ref is not None and not isinstance(thread_ref, StreamReader):
            raise ValueError('Thread reference has to be a StreamReader object or None')

        self._modificationArgs = {'initial_parsing_result': initial_parsing_result, 'thread_ref': thread_ref,
                                  'sleep_time': sleep_time}
        self._modificationResult = {'parsing_result': None, 'error': None}
        self.run()

        if self._modificationResult['parsing_result'] is not None:
            self._modificationResult['parsing_result'].arIndex.sort()
            return self._modificationResult['parsing_result']
        else:
            raise self._modificationResult['error']

    @abc.abstractmethod
    def create_indexes_generator(self, initial_parsing_result, thread_ref=None, sleep_time=0.5):
        """
            Create index array iterator

            :param thread_ref: thread used to define the end of iteration (Thread object or None)
            :param sleep_time: waiting duration if the current index array is iterated and the thread reference is not
            finished (float)
            :return: element in the parsing result (tuple)
        """

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
