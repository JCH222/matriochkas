# coding: utf8

from enum import Enum
from core.IO import ParsingResult

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
        pass


class ModificationOperation(ModificationEntity, metaclass=abc.ABCMeta):
    @abc.abstractmethod
    def __init__(self, rel_position=0):
        self.rel_position = rel_position


class ModificationAdd(ModificationOperation):
    def __init__(self, character, rel_position=0, modification_side=ModificationSide.RIGHT):
        super(ModificationAdd, self).__init__(rel_position=rel_position)
        self.character = character
        self.modificationSide = modification_side

    def generate_parsing_result(self, initial_parsing_result):
        ar_index = list()
        for element in initial_parsing_result:
            ar_index.append((element[0] + self.rel_position + self.modificationSide.value, self.character))
        parsing_result = ParsingResult(initial_parsing_result.parsingClass, initial_parsing_result.arInput['args'],
                                       initial_parsing_result.arInput['kwargs'],
                                       initial_parsing_result.initialCharacterIndex,
                                       initial_parsing_result.finalCharacterIndex, ar_index)
        return parsing_result


class ModificationRemove(ModificationOperation):
    def __init__(self, rel_position=0):
        super(ModificationAdd, self).__init__(rel_position=rel_position)

    def generate_parsing_result(self, initial_parsing_result):
        ar_index = list()
        for element in initial_parsing_result:
            ar_index.append((element[0]+self.rel_position, ''))
        parsing_result = ParsingResult(initial_parsing_result.parsingClass, initial_parsing_result.arInput['args'],
                                       initial_parsing_result.arInput['kwargs'],
                                       initial_parsing_result.initialCharacterIndex,
                                       initial_parsing_result.finalCharacterIndex, ar_index)
        return parsing_result
