# coding: utf8

from enum import Enum
import abc


class OperatorType(Enum):
    AND = 'and'
    OR = 'or'
    XOR = 'xor'


class ParsingEntity(metaclass=abc.ABCMeta):
    def __and__(self, other):
        parsing_operator = ParsingOperator(OperatorType.AND, self, other)
        return parsing_operator

    def __or__(self, other):
        parsing_operator = ParsingOperator(OperatorType.OR, self, other)
        return parsing_operator

    def __xor__(self, other):
        parsing_operator = ParsingOperator(OperatorType.XOR, self, other)
        return parsing_operator

    @abc.abstractmethod
    def __str__(self):
        pass

    @abc.abstractmethod
    def __repr__(self):
        self.__str__()

    @abc.abstractmethod
    def __copy__(self):
        pass

    @abc.abstractmethod
    def __deepcopy__(self):
        pass

    @abc.abstractmethod
    def check(self, element, ref_position):
        pass


class ParsingOperator(ParsingEntity):
    def __init__(self, operator_type, operand_a, operand_b):
        self.operatorType = operator_type
        self.operandA = operand_a
        self.operandB = operand_b

    def __str__(self):
        pass

    def __repr__(self):
        self.__str__()

    def __copy__(self):
        pass

    def __deepcopy__(self):
        pass

    def check(self, element, ref_position=0):
        if self.operatorType is OperatorType.AND:
            if self.operandA.check(element, ref_position) is True and self.operandB.check(element, ref_position) is True:
                return True
            else:
                return False
        elif self.operatorType is OperatorType.OR:
            if self.operandA.check(element, ref_position) is True or self.operandB.check(element, ref_position) is True:
                return True
            else:
                return False
        else:
            if (self.operandA.check(element, ref_position) is True) ^ (self.operandB.check(element, ref_position) is True):
                return True
            else:
                return False


class ParsingCondition(ParsingEntity):
    def __init__(self, character, rel_position=0):
        self.rel_position = rel_position
        self.character = character

    def __str__(self):
        pass

    def __repr__(self):
        self.__str__()

    def __copy__(self):
        pass

    def __deepcopy__(self):
        pass

    def check(self, element, ref_position=0):
        element_size = len(element)
        if 0 <= ref_position < element_size:
            position = ref_position + self.rel_position
            if 0 <= position < element_size:
                if self.character in element[position]:
                    return True
                else:
                    return False
            else:
                raise IndexError('relative position out of range ( 0 <= ref_position + rel_position < len(element) )')
        else:
            raise IndexError('reference position out of range ( 0 <= ref_position < len(element) )')
