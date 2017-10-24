# coding: utf8

from enum import Enum
from core.Entities import Entity

import abc
import copy


class OperatorType(Enum):
    AND = 'and'
    OR = 'or'
    XOR = 'xor'


class Entity(metaclass=abc.ABCMeta):
    @abc.abstractmethod
    def check(self, element, ref_position):
        pass

    @abc.abstractmethod
    def get_max_position(self):
        pass

    @abc.abstractmethod
    def get_min_position(self):
        pass


class ParsingEntity(Entity, metaclass=abc.ABCMeta):
    def __and__(self, other):
        if isinstance(self, ParsingEntity) and isinstance(other, ParsingEntity):
            parsing_operator = ParsingOperator(OperatorType.AND, self, other)
            return parsing_operator
        else:
            raise TypeError("Operands have to be ParsingEntity's subclasses")

    def __or__(self, other):
        if isinstance(self, ParsingEntity) and isinstance(other, ParsingEntity):
            parsing_operator = ParsingOperator(OperatorType.OR, self, other)
            return parsing_operator
        else:
            raise TypeError("Operands have to be ParsingEntity's subclasses")

    def __xor__(self, other):
        if isinstance(self, ParsingEntity) and isinstance(other, ParsingEntity):
            parsing_operator = ParsingOperator(OperatorType.XOR, self, other)
            return parsing_operator
        else:
            raise TypeError("Operands have to be ParsingEntity's subclasses")

    def __rshift__(self, other):
        if isinstance(self, ParsingEntity) and (isinstance(other, ParsingEntity) or other is None):
            parsing_block = ParsingBlock(self, other)
            return parsing_block
        else:
            raise TypeError("Operands have to be ParsingEntity's subclasses")

    @abc.abstractmethod
    def __eq__(self, other):
        pass

    def __ne__(self, other):
        return not self.__eq__(other)

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
        if isinstance(item, ParsingEntity):
            if isinstance(item, ParsingCondition):
                if item in self.operandA or item in self.operandB:
                    return True
                else:
                    return False
            elif isinstance(item, ParsingOperator):
                if self.__eq__(item):
                    return True
                else:
                    if item in self.operandA or item in self.operandB:
                        return True
                    else:
                        return False
            else:
                TypeError("Unknown ParsingEntity's subclass")
        else:
            raise TypeError("Item have to be ParsingEntity's subclasses")

    def __str__(self):
        return 'ParsingOperator object'

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

    def get_max_position(self):
        operand_a = self.operandA.get_max_position()
        operand_b = self.operandB.get_max_position()

        if operand_a < 0 and operand_b < 0:
            return 0
        else:
            return max([operand_a, operand_b])

    def get_min_position(self):
        operand_a = self.operandA.get_min_position()
        operand_b = self.operandB.get_min_position()

        if operand_a > 0 and operand_b > 0:
            return 0
        else:
            return min([operand_a, operand_b])


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
        self.__eq__(item)

    def __str__(self):
        return 'ParsingCondition object'

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

    def get_min_position(self):
        if self.rel_position > 0:
            return 0
        else:
            return self.rel_position

    def get_max_position(self):
        if self.rel_position < 0:
            return 0
        else:
            return self.rel_position


class ParsingStructure(Entity):
    def __add__(self, other):
        if isinstance(self, ParsingStructure) and (isinstance(other, ParsingStructure) or other is None):
            parsing_pipeline = ParsingPipeline(self)
            if other is not None:
                parsing_pipeline.add_structure(other)
            return parsing_pipeline
        else:
            raise TypeError("Operands have to be ParsingStructure's subclasses")

    @abc.abstractmethod
    def check(self, element, ref_position):
        pass

    @abc.abstractmethod
    def get_max_position(self):
        pass

    @abc.abstractmethod
    def get_min_position(self):
        pass


class ParsingPipeline(ParsingStructure):
    def __init__(self, first_parsing_structure):
        self.arParsingStructure = list()
        self.arParsingStructure.append(first_parsing_structure)
        self.current_parsing_block_index = 0
        self.isEnded = False

    def check(self, element, ref_position=0):
        if not self.isEnded:
            result = self.arParsingStructure[self.current_parsing_block_index].check(element, ref_position)
            if result[1]:
                if self.current_parsing_block_index < len(self.arParsingStructure)-1:
                    self.current_parsing_block_index += 1
                else:
                    self.isEnded = True
            return result
        else:
            return None

    def get_min_position(self):
        ar_min_position = list()
        for parsing_structure in self.arParsingStructure:
            ar_min_position.append(parsing_structure.get_min_position())
        return min(ar_min_position)

    def get_max_position(self):
        ar_max_position = list()
        for parsing_structure in self.arParsingStructure:
            ar_max_position.append(parsing_structure.get_max_position())
        return max(ar_max_position)

    def add_structure(self, parsing_structure):
        if isinstance(parsing_structure, ParsingPipeline):
            self.arParsingStructure = self.arParsingStructure + parsing_structure.arParsingStructure
        elif isinstance(parsing_structure, ParsingBlock):
            self.arParsingStructure.append(parsing_structure)
        else:
            raise TypeError("Object to add have to be ParsingStructure's subclasses")

    def reset(self):
        self.current_parsing_block_index = 0
        self.isEnded = False


class ParsingBlock(ParsingStructure):
    def __init__(self, parser, border_condition):
        self.parser = parser
        self.borderCondition = border_condition

    def check(self, element, ref_position=0):
        parser_result = self.parser.check(element, ref_position)
        if self.borderCondition is not None:
            border_condition_result = self.borderCondition.check(element, ref_position)
        else:
            border_condition_result = False
        return parser_result, border_condition_result

    def get_min_position(self):
        if self.borderCondition is not None:
            return min([self.parser.get_min_position(), self.borderCondition.get_min_position()])
        else:
            return self.parser.get_min_position()

    def get_max_position(self):
        if self.borderCondition is not None:
            return max([self.parser.get_max_position(), self.borderCondition.get_max_position()])
        else:
            return self.parser.get_max_position()
