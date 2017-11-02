# coding: utf8

from core import ParsingEntities

import copy


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


class InstanceParsingStructure(ParsingEntities.ParsingStructure):
    def __init__(self, name, rel_position=0):
        super(InstanceParsingStructure, self).__init__()
        self.name = name
        self.relPosition = rel_position

    def check(self, element, ref_position):
        return True, True

    def get_max_position(self):
        return self.relPosition

    def get_min_position(self):
        return self.relPosition

########################################################################################################################


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


def test_parsing_condition():
    parsing_condition_1 = ParsingEntities.ParsingCondition('0')
    assert isinstance(parsing_condition_1, ParsingEntities.ParsingCondition) is True
    assert (parsing_condition_1.character == '0') is True

    parsing_condition_2 = ParsingEntities.ParsingCondition('012')
    assert isinstance(parsing_condition_2, ParsingEntities.ParsingOperator) is True
    assert (parsing_condition_2.operatorType is ParsingEntities.OperatorType.AND) is True
    assert isinstance(parsing_condition_2.operandA, ParsingEntities.ParsingOperator) is True
    assert (parsing_condition_2.operandA.operatorType is ParsingEntities.OperatorType.AND) is True
    assert (parsing_condition_2.operandA.operandA.character == '0') is True
    assert (parsing_condition_2.operandA.operandB.character == '1') is True
    assert isinstance(parsing_condition_2.operandB, ParsingEntities.ParsingCondition) is True
    assert (parsing_condition_2.operandB.character == '2') is True

    ###################################################################################################################

    assert (parsing_condition_1 == 0) is False
    assert (parsing_condition_1 == ParsingEntities.ParsingCondition('0')) is True
    assert (parsing_condition_1 == ParsingEntities.ParsingCondition('0', rel_position=1)) is False
    parsing_condition_3 = ParsingEntities.ParsingCondition('0')
    parsing_condition_3.isNot = True
    assert (parsing_condition_1 == parsing_condition_3) is False

    ###################################################################################################################

    assert (0 in parsing_condition_1) is False
    assert (ParsingEntities.ParsingCondition('0') in parsing_condition_1) is True
    assert (ParsingEntities.ParsingCondition('0', rel_position=1) in parsing_condition_1) is False
    assert (parsing_condition_3 in parsing_condition_1) is False

    ###################################################################################################################

    assert (str(parsing_condition_1) == 'ParsingCondition object') is True

    ###################################################################################################################

    assert (repr(parsing_condition_1) == 'ParsingCondition object') is True

    ###################################################################################################################

    copy_parsing_condition_1 = copy.copy(parsing_condition_1)
    assert (copy_parsing_condition_1.character == '0') is True
    assert (copy_parsing_condition_1.relPosition == 0) is True
    assert copy_parsing_condition_1.isNot is False

    ###################################################################################################################

    deep_copy_parsing_condition_1 = copy.deepcopy(parsing_condition_1)
    assert (deep_copy_parsing_condition_1.character == '0') is True
    assert (deep_copy_parsing_condition_1.relPosition == 0) is True
    assert deep_copy_parsing_condition_1.isNot is False

    ###################################################################################################################

    parsing_condition_4 = ParsingEntities.ParsingCondition('W', rel_position=2)
    assert parsing_condition_1.check('Hello0World !', ref_position=5) is True
    assert parsing_condition_1.check('Hello0World !', ref_position=6) is False
    assert parsing_condition_4.check('Hello0World !', ref_position=4) is True
    assert parsing_condition_4.check('Hello0World !', ref_position=5) is False

    try:
        parsing_condition_4.check('Hello0World !', ref_position=15)
        assert False
    except IndexError:
        assert True

    try:
        parsing_condition_4.check('Hello0World !', ref_position=11)
        assert False
    except IndexError:
        assert True

    ###################################################################################################################

    parsing_condition_5 = ParsingEntities.ParsingCondition('W', rel_position=-2)
    assert (parsing_condition_2.get_min_position() == 0) is True
    assert (parsing_condition_5.get_min_position() == -2) is True

    ###################################################################################################################

    assert (parsing_condition_2.get_max_position() == 2) is True
    assert (parsing_condition_5.get_max_position() == 0) is True


def test_parsing_structure():
    parsing_structure_1 = InstanceParsingStructure('structure 1')
    parsing_structure_2 = InstanceParsingStructure('structure 2')
    result_1 = parsing_structure_1 + parsing_structure_2
    assert isinstance(result_1, ParsingEntities.ParsingPipeline) is True
    assert (len(result_1.arParsingStructure) == 2) is True
    assert (result_1.arParsingStructure[0].name == 'structure 1') is True
    assert (result_1.arParsingStructure[1].name == 'structure 2') is True

    result_2 = parsing_structure_1 + None
    assert isinstance(result_2, ParsingEntities.ParsingPipeline) is True
    assert (len(result_2.arParsingStructure) == 1) is True
    assert (result_2.arParsingStructure[0].name == 'structure 1') is True

    ###################################################################################################################

    assert (InstanceParsingStructure('test').check(None, None) == (True, True)) is True

    ###################################################################################################################

    assert (InstanceParsingStructure('test').get_min_position() == 0) is True

    ###################################################################################################################

    assert (InstanceParsingStructure('test').get_max_position() == 0) is True


def test_parsing_pipeline():
    parsing_pipeline_1 = ParsingEntities.ParsingPipeline(InstanceParsingStructure('structure 1'))
    assert (len(parsing_pipeline_1.arParsingStructure) == 1) is True
    assert (parsing_pipeline_1.current_parsing_block_index == 0) is True
    assert parsing_pipeline_1.isEnded is False

    try:
        ParsingEntities.ParsingPipeline(None)
        assert False
    except TypeError:
        assert True

    ###################################################################################################################

    parsing_pipeline_1.add_structure(ParsingEntities.ParsingPipeline(InstanceParsingStructure('structure 2')))
    assert (parsing_pipeline_1.current_parsing_block_index == 0) is True
    assert parsing_pipeline_1.isEnded is False
    assert parsing_pipeline_1.check(None, None) == (True, True)
    assert (parsing_pipeline_1.current_parsing_block_index == 1) is True
    assert parsing_pipeline_1.isEnded is False
    assert parsing_pipeline_1.check(None, None) == (True, True)
    assert (parsing_pipeline_1.current_parsing_block_index == 1) is True
    assert parsing_pipeline_1.isEnded is True
    assert parsing_pipeline_1.check(None, None) is None

    ###################################################################################################################

    parsing_pipeline_2 = ParsingEntities.ParsingPipeline(InstanceParsingStructure('structure 1', -2))
    parsing_pipeline_2.add_structure(InstanceParsingStructure('structure 2', 3))
    parsing_pipeline_2.add_structure(InstanceParsingStructure('structure 3', 1))
    assert (parsing_pipeline_2.get_max_position() == 3) is True

    ###################################################################################################################

    assert (parsing_pipeline_2.get_min_position() == -2) is True

    ###################################################################################################################

    assert (len(parsing_pipeline_2.arParsingStructure) == 3) is True
    parsing_pipeline_2.add_structure(InstanceParsingStructure('structure 4'))
    assert (len(parsing_pipeline_2.arParsingStructure) == 4) is True

    ###################################################################################################################

    assert parsing_pipeline_1.isEnded is True
    assert (parsing_pipeline_1.current_parsing_block_index == 1) is True
    parsing_pipeline_1.reset()
    assert parsing_pipeline_1.isEnded is False
    assert (parsing_pipeline_1.current_parsing_block_index == 0) is True
