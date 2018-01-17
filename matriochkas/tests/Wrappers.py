# coding: utf8

from matriochkas.core.Wrappers import ReadingWrapper
from io import StringIO

from threading import Thread
from time import sleep


class ThreadRoot(Thread):
    def __init__(self, method, name):
        super(ThreadRoot, self).__init__()
        self.arCharacter = dict()
        self.method = method
        self.name = name


class ThreadA(ThreadRoot):
    def __init__(self, method, name):
        super(ThreadA, self).__init__(method, name)

    def run(self):
        self.arCharacter[1] = self.method(1, self.name)
        sleep(1)
        self.arCharacter[2] = self.method(3, self.name)
        sleep(1)
        self.arCharacter[3] = self.method(1, self.name)
        sleep(1)


class ThreadB(ThreadRoot):
    def __init__(self, method, name):
        super(ThreadB, self).__init__(method, name)

    def run(self):
        self.arCharacter[1] = self.method(1, self.name)
        sleep(1)
        self.arCharacter[2] = self.method(1, self.name)
        sleep(1)
        self.arCharacter[3] = self.method(3, self.name)
        sleep(1)


class ThreadC(ThreadRoot):
    def __init__(self, method, name):
        super(ThreadC, self).__init__(method, name)

    def run(self):
        self.arCharacter[1] = self.method(1, self.name)
        sleep(1)
        self.arCharacter[2] = self.method(1, self.name)
        sleep(1)


class ThreadD(ThreadRoot):
    def __init__(self, method, name, wrapper):
        super(ThreadD, self).__init__(method, name)
        self.wrapper = wrapper

    def run(self):
        self.arCharacter[1] = self.method(1, self.name)
        sleep(1)
        self.wrapper.remove(self.name)
        raise RuntimeError("Unit test error")


def test_reading_wrapper():
    initial_text = 'abcd'

    reader_1 = StringIO(initial_text)
    wrapper_1 = ReadingWrapper(reader_1.read)

    assert wrapper_1.arReadingEvent == dict()
    assert wrapper_1.arTriggerEvent == dict()
    method_1_a = wrapper_1.get_method("stream_reader_A")
    assert method_1_a == wrapper_1.read
    assert list(wrapper_1.arReadingEvent.keys()) == ["stream_reader_A"]
    assert list(wrapper_1.arTriggerEvent.keys()) == ["stream_reader_A"]

    method_1_b = wrapper_1.get_method("stream_reader_B")
    assert method_1_b == wrapper_1.read
    assert list(wrapper_1.arReadingEvent.keys()) == ["stream_reader_A", "stream_reader_B"]
    assert list(wrapper_1.arTriggerEvent.keys()) == ["stream_reader_A", "stream_reader_B"]

    method_1_a_2 = wrapper_1.get_method("stream_reader_A")
    assert method_1_a_2 == wrapper_1.read
    assert list(wrapper_1.arReadingEvent.keys()) == ["stream_reader_A", "stream_reader_B"]
    assert list(wrapper_1.arTriggerEvent.keys()) == ["stream_reader_A", "stream_reader_B"]

    ###################################################################################################################

    reader_2 = StringIO(initial_text)
    wrapper_2 = ReadingWrapper(reader_2.read)

    thread_a = ThreadA(wrapper_2.get_method("stream_reader_A"), "stream_reader_A")
    thread_b = ThreadB(wrapper_2.get_method("stream_reader_B"), "stream_reader_B")

    thread_a.start()
    thread_b.start()
    wrapper_2.start()
    sleep(10)

    assert thread_a.arCharacter == {1: 'a', 2: 'bcd', 3: ''}
    assert thread_b.arCharacter == {1: 'a', 2: 'b', 3: 'cd'}

    ###################################################################################################################

    reader_3 = StringIO(initial_text)
    wrapper_3 = ReadingWrapper(reader_3.read)

    thread_a = ThreadA(wrapper_3.get_method("stream_reader_A"), "stream_reader_A")
    thread_c = ThreadC(wrapper_3.get_method("stream_reader_C"), "stream_reader_C")

    thread_a.start()
    thread_c.start()
    wrapper_3.start()
    sleep(2)

    wrapper_3.remove("stream_reader_C")
    sleep(8)

    assert thread_a.arCharacter == {1: 'a', 2: 'bcd', 3: ''}
    assert thread_c.arCharacter == {1: 'a', 2: 'b'}

    ###################################################################################################################

    reader_4 = StringIO(initial_text)
    wrapper_4 = ReadingWrapper(reader_4.read)

    thread_a = ThreadA(wrapper_4.get_method("stream_reader_A"), "stream_reader_A")
    thread_d = ThreadD(wrapper_4.get_method("stream_reader_D"), "stream_reader_D", wrapper_4)

    try:
        thread_a.start()
        thread_d.start()
        wrapper_4.start()

        sleep(10)

        assert thread_a.arCharacter == {1: 'a', 2: 'bcd', 3: ''}
        assert thread_d.arCharacter == {1: 'a'}
    except RuntimeError:
        pass
