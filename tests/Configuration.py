# coding: utf8

from core import Configuration


def test_stream_class_configuration():

    assert len(Configuration.StreamClassConfiguration) == 2

    assert Configuration.StreamClassConfiguration.StringIO.name == 'StringIO'
    assert Configuration.StreamClassConfiguration.StringIO.value == {'read_method': 'read', 'write_method': 'write',
                                                                     'return_method': 'getvalue'}

    assert Configuration.StreamClassConfiguration.TextIOWrapper.name == 'TextIOWrapper'
    assert Configuration.StreamClassConfiguration.TextIOWrapper.value == {'read_method': 'read',
                                                                          'write_method': 'write',
                                                                          'return_method': 'None'}
