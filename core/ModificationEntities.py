# coding: utf8

from core.ParsingEntities import Entity


class ModificationEntity(Entity):
    def __add__(self, other):
        if isinstance(self, ModificationEntity) and isinstance(other, ModificationEntity):
            modification_operator = ModificationOperator()
            return modification_operator
        else:
            raise TypeError("Operands have to be ModificationEntity's subclasses")


class ModificationCondition(ModificationEntity):
    pass


class ModificationOperator(ModificationEntity):
    pass