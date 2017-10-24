# coding: utf8

from core.ParsingEntities import Entity
from enum import Enum

import abc


class ModificationSide(Enum):
    LEFT = 0
    RIGHT = 1


class ModificationEntity(Entity):
    def __add__(self, other):
        if isinstance(self, ModificationEntity) and isinstance(other, ModificationEntity):
            modification_operator = ModificationOperator()
            return modification_operator
        else:
            raise TypeError("Operands have to be ModificationEntity's subclasses")


class ModificationOperator(ModificationEntity):
    def __init__(self, operand_a, operand_b):
        self.operandA = operand_a
        self.operandB = operand_b

    def check(self, element, ref_position):
        pass


class ModificationOperation(ModificationEntity, metaclass=abc.ABCMeta):
    @abc.abstractmethod
    def __init__(self, rel_position=0):
        self.rel_position = rel_position

    @abc.abstractmethod
    def check(self, element, ref_position=0):
        pass


class ModificationAdd(ModificationOperation):
    def __init__(self, character, rel_position=0, modification_side=ModificationSide.RIGHT):
        super(ModificationAdd, self).__init__(rel_position=rel_position)
        self.character = character
        self.modificationSide = modification_side

    def check(self, element, ref_position=0):
        pass


class ModificationRemove(ModificationOperation):
    def __init__(self, rel_position=0):
        super(ModificationAdd, self).__init__(rel_position=rel_position)

    def check(self, element, ref_position=0):
        pass
