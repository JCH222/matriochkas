# coding: utf8

from core import ModificationEntities


########################################################################################################################


class InstanceModificationEntity(ModificationEntities.ModificationEntity):
    def generate_parsing_result(self, initial_parsing_result):
        return True

########################################################################################################################


def test_modification_side():

    assert len(ModificationEntities.ModificationSide) == 2

    assert ModificationEntities.ModificationSide.LEFT.name == 'LEFT'
    assert ModificationEntities.ModificationSide.LEFT.value == -1

    assert ModificationEntities.ModificationSide.RIGHT.name == 'RIGHT'
    assert ModificationEntities.ModificationSide.RIGHT.value == 1
