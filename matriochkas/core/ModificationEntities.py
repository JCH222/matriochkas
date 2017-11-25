# coding: utf8

from enum import Enum
from matriochkas.core.ParsingEntities import ParsingResult
from matriochkas.core.ParsingEntities import ParsingResultOrigin

import abc


class ModificationSide(Enum):
    LEFT = -1
    RIGHT = 1


class ModificationEntity(metaclass=abc.ABCMeta):
    def __add__(self, other):
        if isinstance(self, ModificationEntity) and isinstance(other, ModificationEntity):
            modification_operator = ModificationOperator(self, other)
            return modification_operator
        else:
            raise TypeError("Operands have to be ModificationEntity's subclasses")

    @abc.abstractmethod
    def generate_parsing_result(self, initial_parsing_result):
        pass


class ModificationOperator(ModificationEntity):
    def __init__(self, operand_a, operand_b):
        self.operandA = operand_a
        self.operandB = operand_b

    def generate_parsing_result(self, initial_parsing_result):
        parsing_result_a = self.operandA.generate_parsing_result(initial_parsing_result)
        parsing_result_b = self.operandB.generate_parsing_result(initial_parsing_result)
        return parsing_result_a + parsing_result_b


class ModificationOperation(ModificationEntity, metaclass=abc.ABCMeta):
    def __new__(cls, rel_position=0, key_word=None):
        if isinstance(rel_position, int):
            return super(ModificationOperation, cls).__new__(cls)
        elif isinstance(rel_position, list):
            ar_rel_position_size = len(rel_position)
            if ar_rel_position_size >= 1:
                result = ModificationOperation(rel_position=rel_position[0], key_word=key_word)
                for i in rel_position:
                    if i > 0:
                        result = result + ModificationOperation(rel_position=rel_position[i], key_word=key_word)
                return result
            else:
                raise ValueError("Relative position list is empty")
        else:
            raise TypeError('Relative position has to be int or list object')

    def __init__(self, rel_position=0, key_word=None):
        self.relPosition = rel_position
        self.keyWord = key_word

    def generate_parsing_result(self, initial_parsing_result):
        raise NotImplementedError('<generate_parsing_result> method has to be implemented')


class ModificationAdd(ModificationOperation):
    def __new__(cls, ar_character, rel_position=0, modification_side=ModificationSide.RIGHT, key_word=None):
        return super(ModificationAdd, cls).__new__(cls, rel_position, key_word)

    def __init__(self, ar_character, rel_position=0, modification_side=ModificationSide.RIGHT, key_word=None):
        super(ModificationAdd, self).__init__(rel_position=rel_position, key_word=key_word)
        self.ar_character = ar_character
        self.modificationSide = modification_side

    def generate_parsing_result(self, initial_parsing_result):
        if isinstance(initial_parsing_result, ParsingResult):
            ar_index = list()
            for element in initial_parsing_result.create_stream_generator():
                if self.keyWord is None or self.keyWord in element[2].keys():
                    ar_index.append((element[0] + self.relPosition, self.ar_character, self.modificationSide))
            parsing_result = ParsingResult(initial_parsing_result.streamClass,
                                           ParsingResultOrigin.MODIFICATION,
                                           initial_parsing_result.resultType,
                                           initial_parsing_result.readMethod,
                                           initial_parsing_result.writeMethod,
                                           initial_parsing_result.returnMethod,
                                           initial_parsing_result.closeMethod,
                                           initial_parsing_result.seekMethod,
                                           initial_parsing_result.arInput['args'],
                                           initial_parsing_result.arInput['kwargs'],
                                           ar_index)
            return parsing_result
        else:
            raise TypeError('Parameter has to be ParsingResult class or subclass')


class ModificationRemove(ModificationOperation):
    def __new__(cls, rel_position=0, key_word=None):
        return super(ModificationRemove, cls).__new__(cls, rel_position, key_word)

    def __init__(self, rel_position=0, key_word=None):
        super(ModificationRemove, self).__init__(rel_position=rel_position, key_word=key_word)

    def generate_parsing_result(self, initial_parsing_result):
        if isinstance(initial_parsing_result, ParsingResult):
            ar_index = list()
            for element in initial_parsing_result.create_stream_generator():
                if self.keyWord is None or self.keyWord in element[2].keys():
                    ar_index.append((element[0]+self.relPosition, ''))
            parsing_result = ParsingResult(initial_parsing_result.streamClass,
                                           ParsingResultOrigin.MODIFICATION,
                                           initial_parsing_result.resultType,
                                           initial_parsing_result.readMethod,
                                           initial_parsing_result.writeMethod,
                                           initial_parsing_result.returnMethod,
                                           initial_parsing_result.closeMethod,
                                           initial_parsing_result.seekMethod,
                                           initial_parsing_result.arInput['args'],
                                           initial_parsing_result.arInput['kwargs'],
                                           ar_index)
            return parsing_result
        else:
            raise TypeError('Parameter has to be ParsingResult class or subclass')
