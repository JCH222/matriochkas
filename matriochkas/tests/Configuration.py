# coding: utf8

from matriochkas.core import Configuration


def test_stream_class_configuration():

    assert len(Configuration.StreamClassConfiguration) == 2

    assert Configuration.StreamClassConfiguration.StringIO.name == 'StringIO'
    assert Configuration.StreamClassConfiguration.StringIO.value == {'read_method': 'read', 'write_method': 'write',
                                                                     'return_method': 'getvalue',
                                                                     'close_method': 'close', 'seek_method': 'seek'}

    assert Configuration.StreamClassConfiguration.TextIOWrapper.name == 'TextIOWrapper'
    assert Configuration.StreamClassConfiguration.TextIOWrapper.value == {'read_method': 'read',
                                                                          'write_method': 'write',
                                                                          'return_method': 'None',
                                                                          'close_method': 'close',
                                                                          'seek_method': 'seek'}
