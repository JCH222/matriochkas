# coding: utf8

from matriochkas.core import ParsingEntities
from collections import Counter

import copy


########################################################################################################################

class InstanceParsingEntity(ParsingEntities.ParsingEntity):
    def __init__(self, name, check_return=True, rel_position=0, key_word=None):
        super(InstanceParsingEntity, self).__init__()
        self.name = name
        self.checkReturn = check_return
        self.keyWord = key_word
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


class MockStreamClass:
    pass

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
    assert isinstance(rshift_parsing_block_2.borderCondition, ParsingEntities.EmptyParsingCondition) is True

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
    assert (parsing_condition_1 == ParsingEntities.ParsingCondition('0', key_word='0')) is False
    parsing_condition_3 = ParsingEntities.ParsingCondition('0')
    parsing_condition_3.isNot = True
    assert (parsing_condition_1 == parsing_condition_3) is False

    ###################################################################################################################

    assert (0 in parsing_condition_1) is False
    assert (ParsingEntities.ParsingCondition('0') in parsing_condition_1) is True
    assert (ParsingEntities.ParsingCondition('0', rel_position=1) in parsing_condition_1) is False
    assert (ParsingEntities.ParsingCondition('0', key_word='0') in parsing_condition_1) is False
    assert (parsing_condition_3 in parsing_condition_1) is False

    ###################################################################################################################

    assert (str(parsing_condition_1) == 'ParsingCondition object') is True

    ###################################################################################################################

    assert (repr(parsing_condition_1) == 'ParsingCondition object') is True

    ###################################################################################################################

    copy_parsing_condition_1 = copy.copy(parsing_condition_1)
    assert (copy_parsing_condition_1.character == '0') is True
    assert (copy_parsing_condition_1.relPosition == 0) is True
    assert copy_parsing_condition_1.keyWord is None
    assert copy_parsing_condition_1.isNot is False

    ###################################################################################################################

    deep_copy_parsing_condition_1 = copy.deepcopy(parsing_condition_1)
    assert (deep_copy_parsing_condition_1.character == '0') is True
    assert (deep_copy_parsing_condition_1.relPosition == 0) is True
    assert copy_parsing_condition_1.keyWord is None
    assert deep_copy_parsing_condition_1.isNot is False

    ###################################################################################################################

    parsing_condition_4 = ParsingEntities.ParsingCondition('W', rel_position=2)
    assert (parsing_condition_1.check('Hello0World !', ref_position=5) == (True, Counter({None: 1}))) is True
    assert parsing_condition_1.check('Hello0World !', ref_position=6) is False
    assert (parsing_condition_4.check('Hello0World !', ref_position=4) == (True, Counter({None: 1}))) is True
    assert parsing_condition_4.check('Hello0World !', ref_position=5) is False

    parsing_condition_6 = ParsingEntities.ParsingCondition('')
    assert isinstance(parsing_condition_6, ParsingEntities.EmptyParsingCondition) is True

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

    copy_parsing_condition_1 = copy.copy(parsing_condition_1)
    assert (copy_parsing_condition_1.character == '0') is True
    assert (copy_parsing_condition_1.relPosition == 0) is True
    assert copy_parsing_condition_1.isNot is False

    ###################################################################################################################

    deep_copy_parsing_condition_1 = copy.deepcopy(parsing_condition_1)
    assert (deep_copy_parsing_condition_1.character == '0') is True
    assert (deep_copy_parsing_condition_1.relPosition == 0) is True
    assert deep_copy_parsing_condition_1.isNot is False


def test_empty_parsing_condition():
    empty_parsing_condition_1 = ParsingEntities.EmptyParsingCondition()
    assert isinstance(empty_parsing_condition_1, ParsingEntities.EmptyParsingCondition) is True

    ###################################################################################################################

    assert (empty_parsing_condition_1 == ParsingEntities.EmptyParsingCondition()) is True
    assert (empty_parsing_condition_1 == ParsingEntities.ParsingCondition('')) is True
    assert (empty_parsing_condition_1 == ParsingEntities.ParsingCondition('0')) is False
    assert (empty_parsing_condition_1 == ~ParsingEntities.EmptyParsingCondition()) is False

    ###################################################################################################################

    assert (empty_parsing_condition_1 in ParsingEntities.EmptyParsingCondition()) is True
    assert (empty_parsing_condition_1 in ParsingEntities.ParsingCondition('')) is True
    assert (empty_parsing_condition_1 in ParsingEntities.ParsingCondition('0')) is False
    assert (empty_parsing_condition_1 in ~ParsingEntities.EmptyParsingCondition()) is False

    ###################################################################################################################

    assert (str(empty_parsing_condition_1) == 'EmptyParsingCondition object') is True

    ###################################################################################################################

    assert (repr(empty_parsing_condition_1) == 'EmptyParsingCondition object') is True

    ###################################################################################################################

    copy_empty_parsing_condition_1 = copy.copy(empty_parsing_condition_1)
    isinstance(copy_empty_parsing_condition_1, ParsingEntities.EmptyParsingCondition)
    assert copy_empty_parsing_condition_1.isNot is False

    ###################################################################################################################

    deep_copy_empty_parsing_condition_1 = copy.deepcopy(empty_parsing_condition_1)
    isinstance(deep_copy_empty_parsing_condition_1, ParsingEntities.EmptyParsingCondition)
    assert deep_copy_empty_parsing_condition_1.isNot is False

    ###################################################################################################################

    assert empty_parsing_condition_1.check(None, None) is False

    ###################################################################################################################

    assert (empty_parsing_condition_1.get_min_position() == 0) is True

    ###################################################################################################################

    assert (empty_parsing_condition_1.get_max_position() == 0) is True


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


def test_parsing_block():
    parsing_block_1 = ParsingEntities.ParsingBlock(InstanceParsingEntity('structure 1', True, -1),
                                                   InstanceParsingEntity('structure 2', True, 1))
    assert isinstance(parsing_block_1.parser, ParsingEntities.ParsingEntity) is True
    assert (parsing_block_1.parser.name == 'structure 1') is True
    assert isinstance(parsing_block_1.borderCondition, ParsingEntities.ParsingEntity) is True
    assert (parsing_block_1.borderCondition.name == 'structure 2') is True

    try:
        ParsingEntities.ParsingBlock(0, None)
        assert False
    except TypeError:
        assert True

    try:
        ParsingEntities.ParsingBlock(InstanceParsingEntity('structure 1', True, -1), 0)
        assert False
    except TypeError:
        assert True

    ###################################################################################################################

    parsing_block_2 = ParsingEntities.ParsingBlock(InstanceParsingEntity('structure 1', True, -2), None)
    assert (parsing_block_1.check(None, None) == (True, True)) is True
    assert (parsing_block_2.check(None, None) == (True, False)) is True

    ###################################################################################################################

    assert (parsing_block_1.get_min_position() == -1) is True
    assert (parsing_block_2.get_min_position() == -2) is True

    ###################################################################################################################

    assert (parsing_block_1.get_max_position() == 1) is True
    assert (parsing_block_2.get_max_position() == 0) is True


def test_parsing_result():
    parsing_result_1 = ParsingEntities.ParsingResult(MockStreamClass, 'read method 1', 'write method 1',
                                                     'return method 1', 'close method 1', ['arg a', 'arg b'],
                                                     {'arg c': 'c', 'arg d': 'd'}, [(0, 'A'), (2, 'B')])
    assert (parsing_result_1.streamClass == MockStreamClass) is True
    assert (parsing_result_1.readMethod == 'read method 1') is True
    assert (parsing_result_1.writeMethod == 'write method 1') is True
    assert (parsing_result_1.returnMethod == 'return method 1') is True
    assert (parsing_result_1.arInput == {'args': ['arg a', 'arg b'], 'kwargs': {'arg c': 'c', 'arg d': 'd'}}) is True
    assert (parsing_result_1.arIndex == [(0, 'A'), (2, 'B')])

    try:
        ParsingEntities.ParsingResult(None, 'read method', 'write method', 'return method', 'close method',
                                      ['arg a', 'arg b'], {'arg c': 'c', 'arg d': 'd'}, [(0, 'A'), (2, 'B')])
        assert False
    except TypeError:
        assert True

    try:
        ParsingEntities.ParsingResult(MockStreamClass, 'read method', 'write method', 'return method', 'close method',
                                      None, {'arg c': 'c', 'arg d': 'd'}, [(0, 'A'), (2, 'B')])
        assert False
    except TypeError:
        assert True

    try:
        ParsingEntities.ParsingResult(MockStreamClass, 'read method', 'write method', 'return method', 'close method',
                                      ['arg a', 'arg b'], None, [(0, 'A'), (2, 'B')])
        assert False
    except TypeError:
        assert True

    try:
        ParsingEntities.ParsingResult(MockStreamClass, 'read method', 'write method', 'return method', 'close method',
                                      ['arg a', 'arg b'], {'arg c': 'c', 'arg d': 'd'}, None)
        assert False
    except TypeError:
        assert True

    try:
        ParsingEntities.ParsingResult(MockStreamClass, 0, 'write method', 'return method', ['arg a', 'arg b'],
                                      'close method', {'arg c': 'c', 'arg d': 'd'}, [(0, 'A'), (2, 'B')])
        assert False
    except TypeError:
        assert True

    try:
        ParsingEntities.ParsingResult(MockStreamClass, 'read method', 0, 'return method', 'close method',
                                      ['arg a', 'arg b'], {'arg c': 'c', 'arg d': 'd'}, [(0, 'A'), (2, 'B')])
        assert False
    except TypeError:
        assert True

    try:
        ParsingEntities.ParsingResult(MockStreamClass, 'read method', 'write method', 0, 'close method',
                                      ['arg a', 'arg b'], {'arg c': 'c', 'arg d': 'd'}, [(0, 'A'), (2, 'B')])
        assert False
    except TypeError:
        assert True

    try:
        ParsingEntities.ParsingResult(MockStreamClass, 'read method', 'write method', 'return method', 0,
                                      ['arg a', 'arg b'], {'arg c': 'c', 'arg d': 'd'}, [(0, 'A'), (2, 'B')])
        assert False
    except TypeError:
        assert True

    parsing_result_2 = ParsingEntities.ParsingResult(MockStreamClass, None, 'write method 2', 'return method 2',
                                                     'close method 2', ['arg a', 'arg b'], {'arg c': 'c', 'arg d': 'd'},
                                                     [(0, 'A'), (2, 'B')])
    assert (parsing_result_2.streamClass == MockStreamClass) is True
    assert parsing_result_2.readMethod is None
    assert (parsing_result_2.writeMethod == 'write method 2') is True
    assert (parsing_result_2.returnMethod == 'return method 2') is True
    assert (parsing_result_2.closeMethod == 'close method 2') is True
    assert (parsing_result_2.arInput == {'args': ['arg a', 'arg b'], 'kwargs': {'arg c': 'c', 'arg d': 'd'}}) is True
    assert (parsing_result_2.arIndex == [(0, 'A'), (2, 'B')])

    parsing_result_3 = ParsingEntities.ParsingResult(MockStreamClass, 'read method 3', None, 'return method 3',
                                                     'close method 3', ['arg a', 'arg b'], {'arg c': 'c', 'arg d': 'd'},
                                                     [(0, 'A'), (2, 'B')])
    assert (parsing_result_3.streamClass == MockStreamClass) is True
    assert (parsing_result_3.readMethod == 'read method 3') is True
    assert parsing_result_3.writeMethod is None
    assert (parsing_result_3.returnMethod == 'return method 3') is True
    assert (parsing_result_3.closeMethod == 'close method 3') is True
    assert (parsing_result_3.arInput == {'args': ['arg a', 'arg b'], 'kwargs': {'arg c': 'c', 'arg d': 'd'}}) is True
    assert (parsing_result_3.arIndex == [(0, 'A'), (2, 'B')])

    parsing_result_4 = ParsingEntities.ParsingResult(MockStreamClass, 'read method 4', 'write method 4', None,
                                                     'close method 4', ['arg a', 'arg b'], {'arg c': 'c', 'arg d': 'd'},
                                                     [(0, 'A'), (2, 'B')])
    assert (parsing_result_4.streamClass == MockStreamClass) is True
    assert (parsing_result_4.readMethod == 'read method 4') is True
    assert (parsing_result_4.writeMethod == 'write method 4') is True
    assert parsing_result_4.returnMethod is None
    assert (parsing_result_4.closeMethod == 'close method 4') is True
    assert (parsing_result_4.arInput == {'args': ['arg a', 'arg b'], 'kwargs': {'arg c': 'c', 'arg d': 'd'}}) is True
    assert (parsing_result_4.arIndex == [(0, 'A'), (2, 'B')])

    parsing_result_8 = ParsingEntities.ParsingResult(MockStreamClass, 'read method 8', 'write method 8',
                                                     'return method 8', None, ['arg a', 'arg b'],
                                                     {'arg c': 'c', 'arg d': 'd'}, [(0, 'A'), (2, 'B')])
    assert (parsing_result_8.streamClass == MockStreamClass) is True
    assert (parsing_result_8.readMethod == 'read method 8') is True
    assert (parsing_result_8.writeMethod == 'write method 8') is True
    assert (parsing_result_8.returnMethod == 'return method 8') is True
    assert parsing_result_8.closeMethod is None
    assert (parsing_result_8.arInput == {'args': ['arg a', 'arg b'], 'kwargs': {'arg c': 'c', 'arg d': 'd'}}) is True
    assert (parsing_result_8.arIndex == [(0, 'A'), (2, 'B')])

    ###################################################################################################################

    parsing_result_5a = ParsingEntities.ParsingResult(MockStreamClass, 'read method 1', 'write method 1',
                                                      'return method 1', 'close method 1', ['arg a', 'arg b'],
                                                      {'arg c': 'c', 'arg d': 'd'}, [(0, 'A'), (2, 'B')])
    parsing_result_5b = ParsingEntities.ParsingResult(MockStreamClass, 'read method 1', 'write method 1',
                                                      'return method 1', 'close method 1', ['arg a', 'arg b'],
                                                      {'arg c': 'c', 'arg d': 'd'}, [(0, 'C'), (4, 'D')])
    parsing_result_5 = parsing_result_5a + parsing_result_5b
    assert (parsing_result_5.streamClass == MockStreamClass) is True
    assert (parsing_result_5.readMethod == 'read method 1') is True
    assert (parsing_result_5.writeMethod == 'write method 1') is True
    assert (parsing_result_5.returnMethod == 'return method 1') is True
    assert (parsing_result_5.closeMethod == 'close method 1') is True
    assert (parsing_result_5.arInput == {'args': ['arg a', 'arg b'], 'kwargs': {'arg c': 'c', 'arg d': 'd'}}) is True
    assert (parsing_result_5.arIndex == [(0, 'A'), (2, 'B'), (4, 'D')])

    parsing_result_5c = ParsingEntities.ParsingResult(MockStreamClass, 'read method 1', 'write method 1',
                                                      'return method 1', 'close method 1', ['arg e', 'arg f'],
                                                      {'arg c': 'c', 'arg d': 'd'}, [(0, 'C'), (4, 'D')])
    try:
        parsing_result_5a + parsing_result_5c
        assert False
    except ValueError:
        assert True

    parsing_result_5d = ParsingEntities.ParsingResult(MockStreamClass, 'read method 2', 'write method 1',
                                                      'return method 1', 'close method 1', ['arg a', 'arg b'],
                                                      {'arg c': 'c', 'arg d': 'd'}, [(0, 'C'), (4, 'D')])
    try:
        parsing_result_5a + parsing_result_5d
        assert False
    except ValueError:
        assert True

    parsing_result_5e = ParsingEntities.ParsingResult(MockStreamClass, 'read method 1', 'write method 2',
                                                      'return method 1', 'close method 1', ['arg a', 'arg b'],
                                                      {'arg c': 'c', 'arg d': 'd'}, [(0, 'C'), (4, 'D')])
    try:
        parsing_result_5a + parsing_result_5e
        assert False
    except ValueError:
        assert True

    parsing_result_5f = ParsingEntities.ParsingResult(MockStreamClass, 'read method 1', 'write method 1',
                                                      'return method 2', 'close method 1', ['arg a', 'arg b'],
                                                      {'arg c': 'c', 'arg d': 'd'}, [(0, 'C'), (4, 'D')])
    try:
        parsing_result_5a + parsing_result_5f
        assert False
    except ValueError:
        assert True

    parsing_result_5h = ParsingEntities.ParsingResult(MockStreamClass, 'read method 1', 'write method 1',
                                                      'return method 1', 'close method 2', ['arg a', 'arg b'],
                                                      {'arg c': 'c', 'arg d': 'd'}, [(0, 'C'), (4, 'D')])
    try:
        parsing_result_5a + parsing_result_5h
        assert False
    except ValueError:
        assert True

    parsing_result_5g = ParsingEntities.ParsingResult(MockStreamClass, 'read method 1', 'write method 1',
                                                      'return method 1', 'close method 1', ['arg a', 'arg b'],
                                                      {'arg d': 'd', 'arg e': 'e'}, [(0, 'C'), (4, 'D')])
    try:
        parsing_result_5a + parsing_result_5g
        assert False
    except ValueError:
        assert True

    try:
        parsing_result_5a + None
        assert False
    except TypeError:
        assert True


    ###################################################################################################################

    parsing_result_6 = ParsingEntities.ParsingResult(MockStreamClass, 'read method 1', 'write method 1',
                                                     'return method 1', 'close method 1', ['arg a', 'arg b'],
                                                     {'arg c': 'c', 'arg d': 'd'},
                                                     [(0, 'A'), (2, 'B', 'Left')])
    assert (0 in parsing_result_6) is True
    assert ((0, 'A') in parsing_result_6) is True
    assert ((2, 'B', 'Left') in parsing_result_6) is True
    assert ((2, None, 'Left') in parsing_result_6) is True
    assert (5 in parsing_result_6) is False
    assert ((0, 'B') in parsing_result_6) is False
    assert ((1, 'B', 'Right') in parsing_result_6) is False
    assert ((0, None, 'Left') in parsing_result_6) is False
    assert ('0' in parsing_result_6) is False

    ###################################################################################################################

    assert repr(parsing_result_1) is not None

    ###################################################################################################################

    assert str(parsing_result_1) is not None

    ###################################################################################################################

    copy_parsing_result_1 = copy.copy(parsing_result_1)
    assert (copy_parsing_result_1.streamClass == MockStreamClass) is True
    assert (copy_parsing_result_1.readMethod == 'read method 1') is True
    assert (copy_parsing_result_1.writeMethod == 'write method 1') is True
    assert (copy_parsing_result_1.returnMethod == 'return method 1') is True
    assert (copy_parsing_result_1.closeMethod == 'close method 1') is True
    assert (copy_parsing_result_1.arInput ==
            {'args': ['arg a', 'arg b'], 'kwargs': {'arg c': 'c', 'arg d': 'd'}}) is True
    assert (copy_parsing_result_1.arIndex == [(0, 'A'), (2, 'B')])
    copy_parsing_result_1.arInput = {'args': ['arg c', 'arg d'], 'kwargs': {'arg e': 'e', 'arg f': 'f'}}
    assert (copy_parsing_result_1.streamClass == MockStreamClass) is True
    assert (copy_parsing_result_1.readMethod == 'read method 1') is True
    assert (copy_parsing_result_1.writeMethod == 'write method 1') is True
    assert (copy_parsing_result_1.returnMethod == 'return method 1') is True
    assert (copy_parsing_result_1.closeMethod == 'close method 1') is True
    assert (copy_parsing_result_1.arInput ==
            {'args': ['arg c', 'arg d'], 'kwargs': {'arg e': 'e', 'arg f': 'f'}}) is True
    assert (copy_parsing_result_1.arIndex == [(0, 'A'), (2, 'B')])
    assert (parsing_result_1.streamClass == MockStreamClass) is True
    assert (parsing_result_1.readMethod == 'read method 1') is True
    assert (parsing_result_1.writeMethod == 'write method 1') is True
    assert (parsing_result_1.returnMethod == 'return method 1') is True
    assert (parsing_result_1.closeMethod == 'close method 1') is True
    assert (parsing_result_1.arInput ==
            {'args': ['arg a', 'arg b'], 'kwargs': {'arg c': 'c', 'arg d': 'd'}}) is True
    assert (parsing_result_1.arIndex == [(0, 'A'), (2, 'B')])

    ###################################################################################################################

    parsing_result_7 = ParsingEntities.ParsingResult(MockStreamClass, 'read method 1', 'write method 1',
                                                     'return method 1', 'close method 1', ['arg a', 'arg b'],
                                                     {'arg c': 'c', 'arg d': 'd'}, [(0, 'A'), (2, 'B')])
    assert ParsingEntities.ParsingResult.are_from_the_same_parsing(parsing_result_1, parsing_result_7) is True
    assert ParsingEntities.ParsingResult.are_from_the_same_parsing(parsing_result_1, copy_parsing_result_1) is False

    try:
        ParsingEntities.ParsingResult.are_from_the_same_parsing(None, None)
        assert False
    except TypeError:
        assert True

    ###################################################################################################################

    assert parsing_result_7.check_indexes() is True

    parsing_result_7.arIndex = [(5, 'A'), (2, 'B')]
    try:
        parsing_result_7.check_indexes()
        assert False
    except ValueError:
        assert True

    parsing_result_7.arIndex = [(0, 'A'), (2, 10)]
    try:
        parsing_result_7.check_indexes()
        assert False
    except TypeError:
        assert True
