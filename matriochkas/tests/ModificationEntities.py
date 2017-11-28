# coding: utf8

from matriochkas.core import ModificationEntities
from matriochkas.tests.ParsingEntities import MockThread
from collections import Counter

import matriochkas.core.ParsingEntities
import matriochkas.tests.ParsingEntities


########################################################################################################################


class InstanceModificationEntity(ModificationEntities.ModificationEntity):
    def __init__(self, name):
        super(InstanceModificationEntity, self).__init__()
        self.name = name

    def create_indexes_generator(self, initial_parsing_result, thread_ref=None, sleep_time=0.5):
        return True

########################################################################################################################


class InstanceModificationOperation(ModificationEntities.ModificationOperation):
    def __new__(cls, name, rel_position=0):
        return super(InstanceModificationOperation, cls).__new__(cls, rel_position, None)

    def __init__(self, name, rel_position=0):
        super(InstanceModificationOperation, self).__init__(rel_position=rel_position)
        self.id = name

########################################################################################################################


class MockParsingResult(matriochkas.core.ParsingEntities.ParsingResult):
    def __init__(self):
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

    assert modification_entity_1.create_indexes_generator(None) is True


def test_modification_operation():
    modification_operation = InstanceModificationOperation('operation 1')
    assert isinstance(modification_operation, ModificationEntities.ModificationOperation) is True
    assert (modification_operation.id == 'operation 1') is True
    assert (modification_operation.relPosition == 0) is True

    modification_operation_2 = InstanceModificationOperation('operation 2', rel_position=2)
    assert isinstance(modification_operation_2, ModificationEntities.ModificationOperation) is True
    assert (modification_operation_2.id == 'operation 2') is True
    assert (modification_operation_2.relPosition == 2) is True

    modification_operation_3 = InstanceModificationOperation('operation 3', rel_position=[i for i in range(0, 3)])
    assert isinstance(modification_operation_3, ModificationEntities.ModificationOperator) is True
    assert isinstance(modification_operation_3.operandA, ModificationEntities.ModificationOperator) is True
    assert isinstance(modification_operation_3.operandA.operandA, ModificationEntities.ModificationOperation) is True
    assert (modification_operation_3.operandA.operandA.relPosition == 0) is True
    assert isinstance(modification_operation_3.operandA.operandB, ModificationEntities.ModificationOperation) is True
    assert (modification_operation_3.operandA.operandB.relPosition == 1) is True
    assert isinstance(modification_operation_3.operandB, ModificationEntities.ModificationOperation) is True
    assert (modification_operation_3.operandB.relPosition == 2) is True

    try:
        InstanceModificationOperation('invalid', rel_position=None)
        assert False
    except TypeError:
        assert True

    try:
        InstanceModificationOperation('invalid', rel_position=[])
        assert False
    except ValueError:
        assert True

    ###################################################################################################################

    try:
        modification_operation.create_indexes_generator(None)
        assert False
    except NotImplementedError:
        assert True


def test_modification_add():
    modification_add_1 = ModificationEntities.ModificationAdd('0', 1, ModificationEntities.ModificationSide.RIGHT)
    parsing_result_1 = matriochkas.core.ParsingEntities.ParsingResult(matriochkas.tests.ParsingEntities.MockStreamClass,
                                                                      matriochkas.core.ParsingEntities.ParsingResultOrigin.READING,
                                                                      matriochkas.core.ParsingEntities.ParsingResultType.VALUE,
                                                                      'read method 1', 'write method 1',
                                                                      'return method 1', 'close method 1',
                                                                      'seek method 1',
                                                                      ['arg a', 'arg b'], {'arg c': 'c', 'arg d': 'd'},
                                                                      [(0, 'A', Counter({None: 1})),
                                                                       (2, 'B', Counter({None: 1}))])
    result = modification_add_1.generate_parsing_result(parsing_result_1)
    assert isinstance(result, matriochkas.core.ParsingEntities.ParsingResult) is True
    assert (result.streamClass == matriochkas.tests.ParsingEntities.MockStreamClass) is True
    assert (result.origin == matriochkas.core.ParsingEntities.ParsingResultOrigin.MODIFICATION) is True
    assert (result.resultType == matriochkas.core.ParsingEntities.ParsingResultType.VALUE) is True
    assert (result.readMethod == 'read method 1') is True
    assert (result.writeMethod == 'write method 1') is True
    assert (result.returnMethod == 'return method 1') is True
    assert (result.closeMethod == 'close method 1') is True
    assert (result.seekMethod == 'seek method 1') is True
    assert (result.arInput == {'args': ['arg a', 'arg b'], 'kwargs': {'arg c': 'c', 'arg d': 'd'}}) is True
    assert (result.arIndex == [(1, '0', ModificationEntities.ModificationSide.RIGHT),
                               (3, '0', ModificationEntities.ModificationSide.RIGHT)]) is True

    try:
        modification_add_1.generate_parsing_result(None)
        assert False
    except AttributeError:
        assert True

    ###################################################################################################################

    generator_1 = modification_add_1.create_indexes_generator(parsing_result_1, None)
    for i, index in enumerate(generator_1):
        if i == 0:
            assert index == [(1, '0', ModificationEntities.ModificationSide.RIGHT)]
        elif i == 1:
            assert index == [(3, '0', ModificationEntities.ModificationSide.RIGHT)]
        else:
            assert False

    thread = MockThread()
    thread.start()
    generator_2 = modification_add_1.create_indexes_generator(parsing_result_1, thread)
    for i, index in enumerate(generator_2):
        if i == 0:
            assert index == [(1, '0', ModificationEntities.ModificationSide.RIGHT)]
        elif i == 1:
            assert index == [(3, '0', ModificationEntities.ModificationSide.RIGHT)]
        else:
            assert False


def test_modification_remove():
    modification_remove_1 = ModificationEntities.ModificationRemove(1)
    parsing_result_1 = matriochkas.core.ParsingEntities.ParsingResult(matriochkas.tests.ParsingEntities.MockStreamClass,
                                                                      matriochkas.core.ParsingEntities.ParsingResultOrigin.READING,
                                                                      matriochkas.core.ParsingEntities.ParsingResultType.VALUE,
                                                                      'read method 1', 'write method 1',
                                                                      'return method 1', 'close method 1',
                                                                      'seek method 1',
                                                                      ['arg a', 'arg b'], {'arg c': 'c', 'arg d': 'd'},
                                                                      [(0, 'A', Counter({None: 1})),
                                                                       (2, 'B', Counter({None: 1}))])
    result = modification_remove_1.generate_parsing_result(parsing_result_1)
    assert isinstance(result, matriochkas.core.ParsingEntities.ParsingResult) is True
    assert (result.streamClass == matriochkas.tests.ParsingEntities.MockStreamClass) is True
    assert (result.origin == matriochkas.core.ParsingEntities.ParsingResultOrigin.MODIFICATION) is True
    assert (result.resultType == matriochkas.core.ParsingEntities.ParsingResultType.VALUE) is True
    assert (result.readMethod == 'read method 1') is True
    assert (result.writeMethod == 'write method 1') is True
    assert (result.returnMethod == 'return method 1') is True
    assert (result.closeMethod == 'close method 1') is True
    assert (result.seekMethod == 'seek method 1') is True
    assert (result.arInput == {'args': ['arg a', 'arg b'], 'kwargs': {'arg c': 'c', 'arg d': 'd'}}) is True
    assert (result.arIndex == [(1, ''), (3, '')]) is True

    try:
        modification_remove_1.generate_parsing_result(None)
        assert False
    except AttributeError:
        assert True

    ###################################################################################################################

    generator_1 = modification_remove_1.create_indexes_generator(parsing_result_1, None)
    for i, index in enumerate(generator_1):
        if i == 0:
            assert index == [(1, '')]
        elif i == 1:
            assert index == [(3, '')]
        else:
            assert False

    thread = MockThread()
    thread.start()
    generator_2 = modification_remove_1.create_indexes_generator(parsing_result_1, thread)
    for i, index in enumerate(generator_2):
        if i == 0:
            assert index == [(1, '')]
        elif i == 1:
            assert index == [(3, '')]
        else:
            assert False
