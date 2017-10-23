# coding: utf8

from core.ParsingEntities import Entity


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


class ModificationCondition(ModificationEntity):
    def __init__(self, character, operation_type, rel_position=0):
        self.rel_position = rel_position
        self.operationType = operation_type
        self.character = character

    def check(self, element, ref_position=0):
        pass
