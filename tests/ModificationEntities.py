# coding: utf8

from core import ModificationEntities
import core.ParsingEntities
import tests.ParsingEntities


########################################################################################################################


class InstanceModificationEntity(ModificationEntities.ModificationEntity):
    def __init__(self, name):
        super(InstanceModificationEntity, self).__init__()
        self.name = name

    def generate_parsing_result(self, initial_parsing_result):
        return True

########################################################################################################################


class InstanceModificationOperation(ModificationEntities.ModificationOperation):
    def __init__(self, name):
        super(InstanceModificationOperation, self).__init__()
        self.name = name

    def generate_parsing_result(self, initial_parsing_result):
        return True

########################################################################################################################


class MockParsingResult(core.ParsingEntities.ParsingResult):
    pass

########################################################################################################################


def test_modification_side():

    assert len(ModificationEntities.ModificationSide) == 2

    assert ModificationEntities.ModificationSide.LEFT.name == 'LEFT'
    assert ModificationEntities.ModificationSide.LEFT.value == -1

    assert ModificationEntities.ModificationSide.RIGHT.name == 'RIGHT'
    assert ModificationEntities.ModificationSide.RIGHT.value == 1


def test_modification_entity():
    modification_entity_1 = InstanceModificationEntity('entity 1')
    modification_entity_2 = InstanceModificationEntity('entity 2')
    result = modification_entity_1 + modification_entity_2
    assert isinstance(result, ModificationEntities.ModificationOperator) is True
    assert isinstance(result.operandA, ModificationEntities.ModificationEntity) is True
    assert (result.operandA.name == 'entity 1') is True
    assert isinstance(result.operandB, ModificationEntities.ModificationEntity) is True
    assert (result.operandB.name == 'entity 2') is True

    ###################################################################################################################

    assert modification_entity_1.generate_parsing_result(None) is True


def test_modification_operation():
    modification_operation = InstanceModificationOperation('operation 1')
    assert isinstance(modification_operation, ModificationEntities.ModificationOperation) is True
    assert (modification_operation.name == 'operation 1') is True
    assert (modification_operation.relPosition == 0) is True
    assert (modification_operation.parsing_result == []) is True


def test_modification_add():
    modification_add_1 = ModificationEntities.ModificationAdd('0', 1, ModificationEntities.ModificationSide.RIGHT)
    parsing_result_1 = core.ParsingEntities.ParsingResult(tests.ParsingEntities.MockStreamClass, 'read method 1',
                                                          'write method 1', 'return method 1', ['arg a', 'arg b'],
                                                          {'arg c': 'c', 'arg d': 'd'}, [(0, 'A'), (2, 'B')])
    result = modification_add_1.generate_parsing_result(parsing_result_1)
    assert isinstance(result, core.ParsingEntities.ParsingResult) is True
    assert (result.streamClass == tests.ParsingEntities.MockStreamClass) is True
    assert (result.readMethod == 'read method 1') is True
    assert (result.writeMethod == 'write method 1') is True
    assert (result.returnMethod == 'return method 1') is True
    assert (result.arInput == {'args': ['arg a', 'arg b'], 'kwargs': {'arg c': 'c', 'arg d': 'd'}}) is True
    assert (result.arIndex == [(1, '0', ModificationEntities.ModificationSide.RIGHT),
                               (3, '0', ModificationEntities.ModificationSide.RIGHT)]) is True

    try:
        modification_add_1.generate_parsing_result(None)
        assert False
    except TypeError:
        assert True
