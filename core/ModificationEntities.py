# coding: utf8

from enum import Enum
from core.ParsingEntities import ParsingResult

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
    def __init__(self, rel_position=0):
        self.relPosition = rel_position
        self.parsing_result = list()


class ModificationAdd(ModificationOperation):
    def __init__(self, ar_character, rel_position=0, modification_side=ModificationSide.RIGHT):
        super(ModificationAdd, self).__init__(rel_position=rel_position)
        self.ar_character = ar_character
        self.modificationSide = modification_side

    def generate_parsing_result(self, initial_parsing_result):
        if isinstance(initial_parsing_result, ParsingResult):
            ar_index = list()
            for element in initial_parsing_result.arIndex:
                ar_index.append((element[0] + self.relPosition, self.ar_character, self.modificationSide))
            parsing_result = ParsingResult(initial_parsing_result.streamClass,
                                           initial_parsing_result.readMethod,
                                           initial_parsing_result.writeMethod,
                                           initial_parsing_result.returnMethod,
                                           initial_parsing_result.arInput['args'],
                                           initial_parsing_result.arInput['kwargs'],
                                           ar_index)
            return parsing_result
        else:
            raise TypeError('Parameter has to be ParsingResult class or subclass')


class ModificationRemove(ModificationOperation):
    def __init__(self, rel_position=0):
        super(ModificationRemove, self).__init__(rel_position=rel_position)

    def generate_parsing_result(self, initial_parsing_result):
        if isinstance(initial_parsing_result, ParsingResult):
            ar_index = list()
            for element in initial_parsing_result.arIndex:
                ar_index.append((element[0]+self.relPosition, ''))
            parsing_result = ParsingResult(initial_parsing_result.streamClass,
                                           initial_parsing_result.readMethod,
                                           initial_parsing_result.writeMethod,
                                           initial_parsing_result.returnMethod,
                                           initial_parsing_result.arInput['args'],
                                           initial_parsing_result.arInput['kwargs'],
                                           ar_index)
            return parsing_result
        else:
            raise TypeError('Parameter has to be ParsingResult class or subclass')
