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

########################################################################################################################

    entity = InstanceEntity()

    assert entity.check(None, None) is True
    assert entity.get_max_position() is True
    assert entity.get_min_position() is True


def test_parsing_entity():
    class InstanceParsingEntity(ParsingEntities.ParsingEntity):
        def __init__(self):
            super(InstanceParsingEntity, self).__init__()

        def __eq__(self, other):
            return True

        def __contains__(self, item):
            return True

        def __str__(self):
            return ''

        def __repr__(self):
            return self.__str__()

        def __copy__(self):
            return InstanceParsingEntity()

        def __deepcopy__(self, memodict={}):
            return InstanceParsingEntity()

        def check(self, element, ref_position):
            return True

        def get_max_position(self):
            return True

        def get_min_position(self):
            return True

########################################################################################################################

    parsing_entity = InstanceParsingEntity()

    assert parsing_entity.isNot is False
    assert (parsing_entity == 0) is True
    assert (parsing_entity != 0) is False
    assert (0 in parsing_entity) is True
    assert str(parsing_entity) is ''
    assert repr(parsing_entity) is ''
    assert isinstance(copy.copy(parsing_entity), InstanceParsingEntity) is True
    assert isinstance(copy.deepcopy(parsing_entity), InstanceParsingEntity) is True
    assert parsing_entity.check(None, None) is True
    assert parsing_entity.get_max_position() is True
    assert parsing_entity.get_min_position() is True

########################################################################################################################

    and_parsing_entity = parsing_entity & InstanceParsingEntity()

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

########################################################################################################################

    or_parsing_entity = parsing_entity | InstanceParsingEntity()

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

########################################################################################################################

    xor_parsing_entity = parsing_entity ^ InstanceParsingEntity()

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

########################################################################################################################

    rshift_parsing_block = parsing_entity >> InstanceParsingEntity()

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

########################################################################################################################

    invert_parsing_entity = ~parsing_entity

    assert isinstance(invert_parsing_entity, ParsingEntities.ParsingEntity) is True
    assert invert_parsing_entity.isNot is True
