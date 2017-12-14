# coding: utf8

import pytest

from matriochkas.core import StreamEntities
from collections import deque


def test_stream_two_dim_deque():
    row_separator = '\n'
    column_separator = ';'

    stream_two_dim_deque_1 = StreamEntities.StreamTwoDimDeque(row_separator=row_separator,
                                                              column_separator=column_separator)
    assert isinstance(stream_two_dim_deque_1, StreamEntities.StreamTwoDimDeque)
    assert stream_two_dim_deque_1.currentRow == 0
    assert stream_two_dim_deque_1.currentColumn == 0
    assert stream_two_dim_deque_1.currentPosition == 0
    assert stream_two_dim_deque_1.rowSeparator == row_separator
    assert stream_two_dim_deque_1.columnSeparator == column_separator
    assert stream_two_dim_deque_1.buffer == str()
    assert stream_two_dim_deque_1.currentDeque == deque([deque()])

    stream_two_dim_deque_2 = StreamEntities.StreamTwoDimDeque(row_separator=row_separator, column_separator=column_separator,
                                                              initial_deque=deque())
    assert isinstance(stream_two_dim_deque_2, StreamEntities.StreamTwoDimDeque)
    assert stream_two_dim_deque_2.currentRow == 0
    assert stream_two_dim_deque_2.currentColumn == 0
    assert stream_two_dim_deque_2.currentPosition == 0
    assert stream_two_dim_deque_2.rowSeparator == row_separator
    assert stream_two_dim_deque_2.columnSeparator == column_separator
    assert stream_two_dim_deque_2.buffer == str()
    assert stream_two_dim_deque_2.currentDeque == deque([deque()])

    deque_to_use_1 = deque([deque(['abc', 'def', 'ghi', 'jkl']), deque(['123', '456', '789', '101112', '131415'])])
    stream_two_dim_deque_3 = StreamEntities.StreamTwoDimDeque(row_separator=row_separator,
                                                              column_separator=column_separator,
                                                              initial_deque=deque_to_use_1)
    assert isinstance(stream_two_dim_deque_3, StreamEntities.StreamTwoDimDeque)
    assert stream_two_dim_deque_3.currentRow == 0
    assert stream_two_dim_deque_3.currentColumn == 0
    assert stream_two_dim_deque_3.currentPosition == 0
    assert stream_two_dim_deque_3.rowSeparator == row_separator
    assert stream_two_dim_deque_3.columnSeparator == column_separator
    assert stream_two_dim_deque_3.buffer == str()
    assert stream_two_dim_deque_3.currentDeque == deque_to_use_1

    ###################################################################################################################

    assert stream_two_dim_deque_3.read(1) == deque_to_use_1[0][0][0:1]
    assert stream_two_dim_deque_3.read(4) == deque_to_use_1[0][0][1:] + column_separator + deque_to_use_1[0][1][0:1]

    assert stream_two_dim_deque_3.read(34) == (deque_to_use_1[0][1][1:] + column_separator + deque_to_use_1[0][2] +
                                               column_separator + deque_to_use_1[0][3] +
                                               row_separator + deque_to_use_1[1][0] + column_separator +
                                               deque_to_use_1[1][1] + column_separator + deque_to_use_1[1][2] +
                                               column_separator + deque_to_use_1[1][3] + column_separator +
                                               deque_to_use_1[1][4][0:4])
    assert stream_two_dim_deque_3.read(100) == deque_to_use_1[1][4][4:]
    assert stream_two_dim_deque_3.read(2) is None

    with pytest.raises(ValueError):
        stream_two_dim_deque_3.read(-1)

    ###################################################################################################################

    stream_two_dim_deque_3.seek(5)
    assert stream_two_dim_deque_3.read(3) == deque_to_use_1[0][1][1:] + column_separator

    stream_two_dim_deque_3.seek(0)
    assert stream_two_dim_deque_3.read(5) == deque_to_use_1[0][0] + column_separator + deque_to_use_1[0][1][0]

    with pytest.raises(ValueError):
        stream_two_dim_deque_3.seek(-1)

    ###################################################################################################################

    stream_two_dim_deque_3.close()
    assert stream_two_dim_deque_3.read(5) == deque_to_use_1[0][0] + column_separator + deque_to_use_1[0][1][0]

    ###################################################################################################################

    stream_two_dim_deque_1.write(deque_to_use_1[0][0] + column_separator + deque_to_use_1[0][1] + row_separator +
                                 deque_to_use_1[0][2])
    assert stream_two_dim_deque_1.currentDeque == deque([deque([deque_to_use_1[0][0], deque_to_use_1[0][1]]),
                                                         deque([deque_to_use_1[0][2]])])

    stream_two_dim_deque_1.write(deque_to_use_1[0][3])
    assert stream_two_dim_deque_1.currentDeque == deque([deque([deque_to_use_1[0][0], deque_to_use_1[0][1]]),
                                                         deque([deque_to_use_1[0][2] + deque_to_use_1[0][3]])])

    ###################################################################################################################

    assert stream_two_dim_deque_1.get_value() == deque([deque([deque_to_use_1[0][0], deque_to_use_1[0][1]]),
                                                        deque([deque_to_use_1[0][2] + deque_to_use_1[0][3]])])
