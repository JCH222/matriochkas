# coding: utf8

from enum import Enum
import abc
import copy


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
    def __eq__(self, other):
        pass

    def __ne__(self, other):
        if not self.__eq__(other):
            return True
        else:
            return False

    @abc.abstractmethod
    def __contains__(self, item):
        pass

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

    def __eq__(self, other):
        if isinstance(other, ParsingOperator):
            if self.operatorType == other.operatorType and ((self.operandA == other.operandA and self.operandB == other.operandB) or (self.operandA == other.operandB and self.operandB == other.operandA)):
                return True
            else:
                return False
        else:
            return False

    def __contains__(self, item):
        pass

    def __str__(self):
        pass

    def __repr__(self):
        self.__str__()

    def __copy__(self):
        return ParsingOperator(self.operatorType, self.operandA, self.operandB)

    def __deepcopy__(self):
        return ParsingOperator(self.operatorType, copy.deepcopy(self.operandA), copy.deepcopy(self.operandB))

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

    def __eq__(self, other):
        if isinstance(other, ParsingCondition):
            if self.rel_position == other.rel_position and self.character == other.character:
                return True
            else:
                return False
        else:
            return False

    def __contains__(self, item):
        pass

    def __str__(self):
        pass

    def __repr__(self):
        self.__str__()

    def __copy__(self):
        return ParsingCondition(self.character, self.rel_position)

    def __deepcopy__(self):
        self.__copy__()

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
