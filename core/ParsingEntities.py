# coding: utf8

from enum import Enum

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
    @abc.abstractmethod
    def __init__(self):
        self.isNot = False

    def __and__(self, other):
        if isinstance(self, ParsingEntity) and isinstance(other, ParsingEntity):
            parsing_operator = ParsingOperator(OperatorType.AND, self, other)
            return parsing_operator
        else:
            raise TypeError("Operands have to be ParsingEntity subclasses")

    def __or__(self, other):
        if isinstance(self, ParsingEntity) and isinstance(other, ParsingEntity):
            parsing_operator = ParsingOperator(OperatorType.OR, self, other)
            return parsing_operator
        else:
            raise TypeError("Operands have to be ParsingEntity subclasses")

    def __xor__(self, other):
        if isinstance(self, ParsingEntity) and isinstance(other, ParsingEntity):
            parsing_operator = ParsingOperator(OperatorType.XOR, self, other)
            return parsing_operator
        else:
            raise TypeError("Operands have to be ParsingEntity subclasses")

    def __rshift__(self, other):
        if isinstance(self, ParsingEntity) and (isinstance(other, ParsingEntity) or other is None):
            parsing_block = ParsingBlock(self, other)
            return parsing_block
        else:
            raise TypeError("Left operand has to be ParsingEntity subclass and right operand has to be"
                            " ParsingEntity subclass or None")

    def __ne__(self, other):
        return not self.__eq__(other)

    def __invert__(self):
        result = copy.deepcopy(self)
        result.isNot = not result.isNot
        return result

    @abc.abstractmethod
    def __eq__(self, other):
        pass

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
    def __deepcopy__(self, memodict={}):
        pass


class ParsingOperator(ParsingEntity):
    def __init__(self, operator_type, operand_a, operand_b):
        super(ParsingOperator, self).__init__()

        if isinstance(operator_type, OperatorType):
            self.operatorType = operator_type
        else:
            raise TypeError('Operator type has to be an OperatorType value')

        if isinstance(operand_a, ParsingEntity) and isinstance(operand_b, ParsingEntity):
            self.operandA = operand_a
            self.operandB = operand_b
        else:
            raise TypeError("Operands have to be ParsingEntity subclasses")

    def __eq__(self, other):
        if isinstance(other, ParsingOperator):
            if self.operatorType == other.operatorType and self.isNot == other.isNot and\
                    ((self.operandA == other.operandA and self.operandB == other.operandB)
                     or (self.operandA == other.operandB and self.operandB == other.operandA)):
                return True
            else:
                return False
        else:
            return False

    def __contains__(self, item):
        if isinstance(item, ParsingEntity):
            if self.__eq__(item):
                return True
            else:
                if item in self.operandA or item in self.operandB:
                    return True
                else:
                    return False
        else:
            raise TypeError("Item has to be ParsingEntity subclasses")

    def __str__(self):
        return 'ParsingOperator object'

    def __repr__(self):
        return self.__str__()

    def __copy__(self):
        result = ParsingOperator(self.operatorType, self.operandA, self.operandB)
        result.isNot = self.isNot
        return result

    def __deepcopy__(self, memodict={}):
        result = ParsingOperator(self.operatorType, copy.deepcopy(self.operandA), copy.deepcopy(self.operandB))
        result.isNot = self.isNot
        return result

    def check(self, element, ref_position=0):
        if self.operatorType is OperatorType.AND:
            if self.operandA.check(element, ref_position) is True and self.operandB.check(element, ref_position) is \
                    True:
                result = True
            else:
                result = False
        elif self.operatorType is OperatorType.OR:
            if self.operandA.check(element, ref_position) is True or self.operandB.check(element, ref_position) is True:
                result = True
            else:
                result = False
        else:
            if (self.operandA.check(element, ref_position) is True) ^ \
                    (self.operandB.check(element, ref_position) is True):
                result = True
            else:
                result = False

        if self.isNot is False:
            return result
        else:
            return not result

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
    def __new__(cls,  ar_character, rel_position=0):
        if len(ar_character) > 1:
            result = ParsingCondition(ar_character[0], rel_position)
            for i, element in enumerate(ar_character):
                if i > 0:
                    result = result & ParsingCondition(ar_character[i], rel_position=rel_position+i)
            return result
        else:
            return super(ParsingCondition, cls).__new__(cls)

    def __init__(self, ar_character, rel_position=0):
        super(ParsingCondition, self).__init__()
        self.relPosition = rel_position
        self.character = ar_character[0]

    def __eq__(self, other):
        if isinstance(other, ParsingCondition):
            if self.relPosition == other.relPosition and self.character == other.character \
                    and self.isNot == other.isNot:
                return True
            else:
                return False
        else:
            return False

    def __contains__(self, item):
        return self.__eq__(item)

    def __str__(self):
        return 'ParsingCondition object'

    def __repr__(self):
        return self.__str__()

    def __copy__(self):
        result = ParsingCondition(self.character, self.relPosition)
        result.isNot = self.isNot
        return result

    def __deepcopy__(self, memodict={}):
        return self.__copy__()

    def check(self, element, ref_position=0):
        element_size = len(element)
        if 0 <= ref_position < element_size:
            position = ref_position + self.relPosition
            if 0 <= position < element_size:
                if self.character in element[position]:
                    result = True
                else:
                    result = False

                if self.isNot is False:
                    return result
                else:
                    return not result
            else:
                raise IndexError('relative position out of range ( 0 <= ref_position + rel_position < len(element) )')
        else:
            raise IndexError('reference position out of range ( 0 <= ref_position < len(element) )')

    def get_min_position(self):
        if self.relPosition > 0:
            return 0
        else:
            return self.relPosition

    def get_max_position(self):
        if self.relPosition < 0:
            return 0
        else:
            return self.relPosition


class ParsingStructure(Entity):
    def __add__(self, other):
        if isinstance(self, ParsingStructure) and (isinstance(other, ParsingStructure) or other is None):
            parsing_pipeline = ParsingPipeline(self)
            if other is not None:
                parsing_pipeline.add_structure(other)
            return parsing_pipeline
        else:
            raise TypeError("Operands have to be ParsingStructure subclasses")

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
        if isinstance(first_parsing_structure, ParsingStructure):
            self.arParsingStructure = list()
            self.arParsingStructure.append(first_parsing_structure)
            self.current_parsing_block_index = 0
            self.isEnded = False
        else:
            raise TypeError("Object has to be ParsingStructure object")

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
        elif isinstance(parsing_structure, ParsingStructure):
            self.arParsingStructure.append(parsing_structure)
        else:
            raise TypeError("Object to add has to be ParsingStructure object")

    def reset(self):
        self.current_parsing_block_index = 0
        self.isEnded = False


class ParsingBlock(ParsingStructure):
    def __init__(self, parser, border_condition):
        if isinstance(parser, ParsingEntity):
            self.parser = parser
        else:
            raise TypeError('parser has to be ParsingStructure subclass')

        if isinstance(border_condition, ParsingEntity) or border_condition is None:
            self.borderCondition = border_condition
        else:
            raise TypeError('border_condition has to be ParsingStructure subclass or None')

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


class ParsingResult:
    def __init__(self, stream_class, read_method, write_method, return_method, args, kwargs, initial_character_index,
                 final_character_index, ar_index):
        self.streamClass = stream_class
        self.readMethod = read_method
        self.writeMethod = write_method
        self.returnMethod = return_method
        self.arInput = {'args': args, 'kwargs': kwargs}
        self.initialCharacterIndex = initial_character_index
        self.finalCharacterIndex = final_character_index
        self.arIndex = ar_index

    def __add__(self, other):
        if isinstance(other, ParsingResult) and isinstance(self, ParsingResult):
            if ParsingResult.are_from_the_same_parsing(self, other):
                new_parsing_result = copy.deepcopy(self)
                for element in other.arIndex:
                    if element[0] not in new_parsing_result.arIndex or \
                            (len(element) == 3 and (element[0], None, element[2]) not in new_parsing_result.arIndex):
                        new_parsing_result.arIndex.append(element)
                new_parsing_result.arIndex.sort()
                return new_parsing_result
            else:
                ValueError("Operands have to come from the same parsing")
        else:
            TypeError("Operands have to be ParsingResult classes or subclasses")

    def __contains__(self, item):
        if isinstance(item, int):
            for element in self.arIndex:
                if element[0] == item:
                    return True
            return False
        elif isinstance(item, tuple):
            if len(item) == 2:
                for element in self.arIndex:
                    if element[0] == item[0] and element[1] == item[1]:
                        return True
                    return False
            elif len(item) == 3:
                if item[1] is not None:
                    for element in self.arIndex:
                        if len(element) == 3 and element[0] == item[0] and element[1] == item[1] \
                                and element[2] == item[2]:
                            return True
                    return False
                else:
                    for element in self.arIndex:
                        if len(element) == 3 and element[0] == item[0] and element[2] == item[2]:
                            return True
                    return False
            else:
                return False
        else:
            return False

    def __str__(self):
        return str({'Stream class': str(self.streamClass.__name__), 'Inputs': str(self.arInput),
                    'from': str(self.initialCharacterIndex), 'to': str(self.finalCharacterIndex),
                    'Index result': str(self.arIndex)})

    def __repr__(self):
        return 'Parsing result :' + '\n' + '   Stream class : ' + str(self.streamClass.__name__) + '\n' \
               + '   Inputs : ' + str(self.arInput) + '\n' + '   Parsed from character ' \
               + str(self.initialCharacterIndex) + ' to character ' + str(self.finalCharacterIndex) + '\n' \
               + '   Index result : ' + str(self.arIndex)

    def __copy__(self):
        return ParsingResult(self.streamClass, self.readMethod, self.writeMethod, self.returnMethod,
                             self.arInput['args'], self.arInput['kwargs'], self.initialCharacterIndex,
                             self.finalCharacterIndex, self.arIndex)

    def __deepcopy__(self, memodict={}):
        copy_ar_input = copy.deepcopy(self.arInput)
        copy_ar_index = copy.deepcopy(self.arIndex)
        return ParsingResult(self.streamClass, self.readMethod, self.writeMethod, self.returnMethod,
                             copy_ar_input['args'], copy_ar_input['kwargs'], self.initialCharacterIndex,
                             self.finalCharacterIndex, copy_ar_index)

    @staticmethod
    def are_from_the_same_parsing(parsing_result_a, parsing_result_b):
        if isinstance(parsing_result_a, ParsingResult) and isinstance(parsing_result_b, ParsingResult):
            return (parsing_result_a.streamClass == parsing_result_b.streamClass and
                    parsing_result_a.arInput['args'] == parsing_result_b.arInput['args'] and
                    parsing_result_a.arInput['kwargs'] == parsing_result_b.arInput['kwargs'] and
                    parsing_result_a.initialCharacterIndex == parsing_result_b.initialCharacterIndex and
                    parsing_result_a.finalCharacterIndex == parsing_result_b.finalCharacterIndex and
                    parsing_result_a.readMethod == parsing_result_b.readMethod and
                    parsing_result_a.writeMethod == parsing_result_b.writeMethod and
                    parsing_result_a.returnMethod == parsing_result_b.returnMethod)
        else:
            TypeError("Operands have to be ParsingResult classes or subclasses")

    def check_indexes(self):
        previous_index_value = -1
        for index in self.arIndex:
            if index[0] > previous_index_value:
                previous_index_value = index[0]
            else:
                raise ValueError('Indexes have to be sorted in ascending order')
            if len(index[1]) != 1 or not isinstance(index[1], str):
                raise TypeError("Indexes characters have to be 'str' objects with a length of 1")

        return True
