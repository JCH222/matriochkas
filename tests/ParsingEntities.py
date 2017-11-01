# coding: utf8

from core import ParsingEntities

import copy


def test_operator_type():

    assert len(ParsingEntities.OperatorType) == 3

    assert ParsingEntities.OperatorType.AND.name == 'AND'
    assert ParsingEntities.OperatorType.AND.value == 'and'

    assert ParsingEntities.OperatorType.OR.name == 'OR'
    assert ParsingEntities.OperatorType.OR.value == 'or'

    assert ParsingEntities.OperatorType.XOR.name == 'XOR'
    assert ParsingEntities.OperatorType.XOR.value == 'xor'


def test_entity():
    class InstanceEntity(ParsingEntities.Entity):
        def check(self, element, ref_position):
            return True

        def get_max_position(self):
            return True

        def get_min_position(self):
            return True

    ###################################################################################################################

    entity = InstanceEntity()

    assert entity.check(None, None) is True
    assert entity.get_max_position() is True
    assert entity.get_min_position() is True


########################################################################################################################

class InstanceParsingEntity(ParsingEntities.ParsingEntity):
    def __init__(self, name, check_return=True, rel_position=0):
        super(InstanceParsingEntity, self).__init__()
        self.name = name
        self.checkReturn = check_return
        self.relPosition = rel_position

    def __eq__(self, other):
        try:
            if other.name == self.name and other.checkReturn == self.checkReturn:
                return True
            else:
                return False
        except AttributeError:
            return False

    def __contains__(self, item):
        return self.__eq__(item)

    def __str__(self):
        return ''

    def __repr__(self):
        return self.__str__()

    def __copy__(self):
        return InstanceParsingEntity(self.name, self.checkReturn)

    def __deepcopy__(self, memodict={}):
        return InstanceParsingEntity(self.name, self.checkReturn)

    def check(self, element, ref_position):
        return self.checkReturn

    def get_max_position(self):
        return self.relPosition

    def get_min_position(self):
        return self.relPosition

########################################################################################################################


def test_parsing_entity():
    parsing_entity = InstanceParsingEntity('test')

    assert parsing_entity.isNot is False
    assert (parsing_entity == InstanceParsingEntity('non test')) is False
    assert (parsing_entity == InstanceParsingEntity('test')) is True
    assert (parsing_entity != InstanceParsingEntity('non test')) is True
    assert (parsing_entity != InstanceParsingEntity('test')) is False
    assert (InstanceParsingEntity('test') in parsing_entity) is True
    assert (InstanceParsingEntity('test false') in parsing_entity) is False
    assert str(parsing_entity) is ''
    assert repr(parsing_entity) is ''
    assert isinstance(copy.copy(parsing_entity), InstanceParsingEntity) is True
    assert isinstance(copy.deepcopy(parsing_entity), InstanceParsingEntity) is True
    assert parsing_entity.get_max_position() == 0
    assert parsing_entity.get_min_position() == 0

    ###################################################################################################################

    and_parsing_entity = parsing_entity & InstanceParsingEntity('test')

    assert isinstance(and_parsing_entity, ParsingEntities.ParsingOperator) is True
    assert isinstance(and_parsing_entity.operandA, InstanceParsingEntity) is True
    assert isinstance(and_parsing_entity.operandB, InstanceParsingEntity) is True
    assert and_parsing_entity.isNot is False
    assert and_parsing_entity.operatorType is ParsingEntities.OperatorType.AND

    try:
        parsing_entity & None
        assert False
    except TypeError:
        assert True

    ###################################################################################################################

    or_parsing_entity = parsing_entity | InstanceParsingEntity('test')

    assert isinstance(or_parsing_entity, ParsingEntities.ParsingOperator) is True
    assert isinstance(or_parsing_entity.operandA, InstanceParsingEntity) is True
    assert isinstance(or_parsing_entity.operandB, InstanceParsingEntity) is True
    assert or_parsing_entity.isNot is False
    assert or_parsing_entity.operatorType is ParsingEntities.OperatorType.OR

    try:
        parsing_entity | None
        assert False
    except TypeError:
        assert True

    ###################################################################################################################

    xor_parsing_entity = parsing_entity ^ InstanceParsingEntity('test')

    assert isinstance(xor_parsing_entity, ParsingEntities.ParsingOperator) is True
    assert isinstance(xor_parsing_entity.operandA, InstanceParsingEntity) is True
    assert isinstance(xor_parsing_entity.operandB, InstanceParsingEntity) is True
    assert xor_parsing_entity.isNot is False
    assert xor_parsing_entity.operatorType is ParsingEntities.OperatorType.XOR

    try:
        parsing_entity ^ None
        assert False
    except TypeError:
        assert True

    ###################################################################################################################

    rshift_parsing_block = parsing_entity >> InstanceParsingEntity('test')

    assert isinstance(rshift_parsing_block, ParsingEntities.ParsingBlock) is True
    assert isinstance(rshift_parsing_block.parser, InstanceParsingEntity) is True
    assert isinstance(rshift_parsing_block.borderCondition, InstanceParsingEntity) is True

    rshift_parsing_block_2 = parsing_entity >> None

    assert isinstance(rshift_parsing_block_2, ParsingEntities.ParsingBlock) is True
    assert isinstance(rshift_parsing_block_2.parser, InstanceParsingEntity) is True
    assert rshift_parsing_block_2.borderCondition is None

    try:
        None >> None
        assert False
    except TypeError:
        assert True

    ###################################################################################################################

    invert_parsing_entity = ~parsing_entity

    assert isinstance(invert_parsing_entity, ParsingEntities.ParsingEntity) is True
    assert invert_parsing_entity.isNot is True


def test_parsing_operator():
    and_parsing_operator = ParsingEntities.ParsingOperator(ParsingEntities.OperatorType.AND,
                                                           InstanceParsingEntity('entity 1'),
                                                           InstanceParsingEntity('entity 2'))

    assert and_parsing_operator.operatorType is ParsingEntities.OperatorType.AND
    assert isinstance(and_parsing_operator.operandA, InstanceParsingEntity)
    assert and_parsing_operator.operandA.name == 'entity 1'
    assert isinstance(and_parsing_operator.operandB, InstanceParsingEntity)
    assert and_parsing_operator.operandB.name == 'entity 2'

    ###################################################################################################################

    or_parsing_operator = ParsingEntities.ParsingOperator(ParsingEntities.OperatorType.OR,
                                                          InstanceParsingEntity('entity 3'),
                                                          InstanceParsingEntity('entity 4'))

    assert or_parsing_operator.operatorType is ParsingEntities.OperatorType.OR
    assert isinstance(or_parsing_operator.operandA, InstanceParsingEntity)
    assert or_parsing_operator.operandA.name == 'entity 3'
    assert isinstance(or_parsing_operator.operandB, InstanceParsingEntity)
    assert or_parsing_operator.operandB.name == 'entity 4'

    ###################################################################################################################

    xor_parsing_operator = ParsingEntities.ParsingOperator(ParsingEntities.OperatorType.XOR,
                                                           InstanceParsingEntity('entity 5'),
                                                           InstanceParsingEntity('entity 6'))

    assert xor_parsing_operator.operatorType is ParsingEntities.OperatorType.XOR
    assert isinstance(xor_parsing_operator.operandA, InstanceParsingEntity)
    assert xor_parsing_operator.operandA.name == 'entity 5'
    assert isinstance(xor_parsing_operator.operandB, InstanceParsingEntity)
    assert xor_parsing_operator.operandB.name == 'entity 6'

    ###################################################################################################################

    try:
        ParsingEntities.ParsingOperator(None, InstanceParsingEntity('entity 7'),
                                        InstanceParsingEntity('entity 8'))
        assert False
    except TypeError:
        assert True

    ###################################################################################################################

    try:
        ParsingEntities.ParsingOperator(ParsingEntities.OperatorType.XOR, None,
                                        InstanceParsingEntity('entity 7'))
        assert False
    except TypeError:
        assert True

    ###################################################################################################################

    try:
        ParsingEntities.ParsingOperator(ParsingEntities.OperatorType.XOR, InstanceParsingEntity('entity 8'), None)
        assert False
    except TypeError:
        assert True

    ###################################################################################################################

    parsing_operator = ParsingEntities.ParsingOperator(ParsingEntities.OperatorType.AND,
                                                       InstanceParsingEntity('entity a', True, 1),
                                                       InstanceParsingEntity('entity b', False, 2))

    assert (parsing_operator == ParsingEntities.ParsingOperator(ParsingEntities.OperatorType.AND,
                                                                InstanceParsingEntity('entity a'),
                                                                InstanceParsingEntity('entity b', False))) is True
    assert (parsing_operator == ParsingEntities.ParsingOperator(ParsingEntities.OperatorType.OR,
                                                                InstanceParsingEntity('entity a'),
                                                                InstanceParsingEntity('entity b', False))) is False
    assert (parsing_operator == ParsingEntities.ParsingOperator(ParsingEntities.OperatorType.AND,
                                                                InstanceParsingEntity('entity c'),
                                                                InstanceParsingEntity('entity b', False))) is False
    assert (parsing_operator == ParsingEntities.ParsingOperator(ParsingEntities.OperatorType.OR,
                                                                InstanceParsingEntity('entity a'),
                                                                InstanceParsingEntity('entity c', False))) is False

    assert (parsing_operator == 0) is False

    ###################################################################################################################

    super_parsing_operator = parsing_operator & InstanceParsingEntity('entity c', True, -1)

    assert isinstance(super_parsing_operator, ParsingEntities.ParsingOperator) is True
    assert (InstanceParsingEntity('entity a') in super_parsing_operator) is True
    assert (InstanceParsingEntity('entity b', False) in super_parsing_operator) is True
    assert (InstanceParsingEntity('entity c') in super_parsing_operator) is True
    assert (InstanceParsingEntity('entity d') in super_parsing_operator) is False
    assert (ParsingEntities.ParsingOperator(ParsingEntities.OperatorType.OR, InstanceParsingEntity('entity a'),
                                            InstanceParsingEntity('entity b')) in super_parsing_operator) is False
    assert (ParsingEntities.ParsingOperator(ParsingEntities.OperatorType.AND, InstanceParsingEntity('entity a'),
                                            InstanceParsingEntity('entity b', False)) in super_parsing_operator) is True

    ###################################################################################################################

    assert (str(parsing_operator) == 'ParsingOperator object') is True

    ###################################################################################################################

    assert (repr(parsing_operator) == 'ParsingOperator object') is True

    ###################################################################################################################

    super_parsing_operator.operandB.name = 'entity c'
    copy_super_parsing_operator = copy.copy(super_parsing_operator)
    assert isinstance(copy_super_parsing_operator, ParsingEntities.ParsingOperator)
    assert (copy_super_parsing_operator.operandB.name == 'entity c') is True
    super_parsing_operator.operandB.name = 'entity d'
    assert (copy_super_parsing_operator.operandB.name == 'entity c') is False
    assert (copy_super_parsing_operator.operandB.name == 'entity d') is True

    ###################################################################################################################

    super_parsing_operator.operandB.name = 'entity c'
    deep_copy_super_parsing_operator = copy.deepcopy(super_parsing_operator)
    assert isinstance(deep_copy_super_parsing_operator, ParsingEntities.ParsingOperator)
    assert (deep_copy_super_parsing_operator.operandB.name == 'entity c') is True
    super_parsing_operator.operandB.name = 'entity d'
    assert (deep_copy_super_parsing_operator.operandB.name == 'entity d') is False
    assert (deep_copy_super_parsing_operator.operandB.name == 'entity c') is True

    ###################################################################################################################

    super_parsing_operator.operandB.name = 'entity c'
    assert super_parsing_operator.check(None, None) is False
    super_parsing_operator.operandA.operandB.checkReturn = True
    assert super_parsing_operator.check(None, None) is True
    super_parsing_operator.operandA.operandB.checkReturn = False

    ###################################################################################################################

    assert (super_parsing_operator.get_min_position() == -1) is True
    super_parsing_operator.operandB.relPosition = 3
    assert (super_parsing_operator.get_min_position() == 0) is True
    super_parsing_operator.operandB.relPosition = -1

    ###################################################################################################################

    assert (super_parsing_operator.get_max_position() == 2) is True
    super_parsing_operator.operandA.operandA.relPosition = -2
    super_parsing_operator.operandA.operandB.relPosition = -3
    assert (super_parsing_operator.get_max_position() == 0) is True
    super_parsing_operator.operandA.operandA.relPosition = 1
    super_parsing_operator.operandA.operandB.relPosition = 2
