# coding: utf8

from core import ModificationEntities


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
