# coding: utf8

from matriochkas.core.Wrappers import ReadingWrapper
from matriochkas.core.Wrappers import ClosingWrapper
from matriochkas.core.Wrappers import ReadingCollector
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


class ThreadI(Thread):
    def __init__(self, method):
        super(ThreadI, self).__init__()
        self.arCharacter = dict()
        self.method = method

    def run(self):
        for i in range(1, 11):
            sleep(1)
            self.arCharacter[i] = self.method(1)


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

    reader_5 = StringIO(initial_text)
    wrapper_5 = ReadingWrapper(reader_5.read)

    assert wrapper_5.arCollector == dict()
    method_5_a = wrapper_5.get_collector_method("stream_writer_A")
    assert isinstance(wrapper_5.arCollector["stream_writer_A"], ReadingCollector)
    assert list(wrapper_5.arCollector.keys()) == ["stream_writer_A"]

    wrapper_5.get_collector_method("stream_writer_B")
    assert isinstance(wrapper_5.arCollector["stream_writer_B"], ReadingCollector)
    assert list(wrapper_5.arCollector.keys()) == ["stream_writer_A", "stream_writer_B"]

    method_5_a_2 = wrapper_5.get_collector_method("stream_writer_A")
    assert isinstance(wrapper_5.arCollector["stream_writer_A"], ReadingCollector)
    assert method_5_a == method_5_a_2
    assert list(wrapper_5.arCollector.keys()) == ["stream_writer_A", "stream_writer_B"]

    ###################################################################################################################

    reader_2 = StringIO(initial_text)
    wrapper_2 = ReadingWrapper(reader_2.read)

    thread_a = ThreadA(wrapper_2.get_method("stream_reader_A"), "stream_reader_A")
    thread_b = ThreadB(wrapper_2.get_method("stream_reader_B"), "stream_reader_B")
    thread_i = ThreadI(wrapper_2.get_collector_method("stream_writer_A"))

    thread_a.start()
    thread_b.start()
    thread_i.start()
    wrapper_2.start()
    sleep(10)

    assert thread_a.arCharacter == {1: 'a', 2: 'bcd', 3: ''}
    assert thread_b.arCharacter == {1: 'a', 2: 'b', 3: 'cd'}
    assert thread_i.arCharacter == {1: 'a', 2: 'b', 3: 'c', 4: 'd', 5: '', 6: '', 7: '', 8: '', 9: ''}

    ###################################################################################################################

    reader_3 = StringIO(initial_text)
    wrapper_3 = ReadingWrapper(reader_3.read)

    thread_a = ThreadA(wrapper_3.get_method("stream_reader_A"), "stream_reader_A")
    thread_c = ThreadC(wrapper_3.get_method("stream_reader_C"), "stream_reader_C")
    thread_i = ThreadI(wrapper_3.get_collector_method("stream_writer_A"))

    thread_a.start()
    thread_c.start()
    thread_i.start()
    wrapper_3.start()
    sleep(2)

    wrapper_3.remove("stream_reader_C")
    sleep(8)

    assert thread_a.arCharacter == {1: 'a', 2: 'bcd', 3: ''}
    assert thread_c.arCharacter == {1: 'a', 2: 'b'}
    assert thread_i.arCharacter == {1: 'a', 2: 'b', 3: 'c', 4: 'd', 5: '', 6: '', 7: '', 8: '', 9: ''}

    ###################################################################################################################

    reader_4 = StringIO(initial_text)
    wrapper_4 = ReadingWrapper(reader_4.read)

    thread_a = ThreadA(wrapper_4.get_method("stream_reader_A"), "stream_reader_A")
    thread_d = ThreadD(wrapper_4.get_method("stream_reader_D"), "stream_reader_D", wrapper_4)
    thread_i = ThreadI(wrapper_4.get_collector_method("stream_writer_A"))

    try:
        thread_a.start()
        thread_d.start()
        thread_i.start()
        wrapper_4.start()

        sleep(10)

        assert thread_a.arCharacter == {1: 'a', 2: 'bcd', 3: ''}
        assert thread_d.arCharacter == {1: 'a'}
        assert thread_i.arCharacter == {1: 'a', 2: 'b', 3: 'c', 4: 'd', 5: '', 6: '', 7: '', 8: '', 9: ''}
    except RuntimeError:
        pass

    ###################################################################################################################

    reader_6 = StringIO(initial_text)
    wrapper_6 = ReadingWrapper(reader_6.read)

    thread_d = ThreadD(wrapper_6.get_method("stream_reader_D"), "stream_reader_D", wrapper_6)
    thread_i = ThreadI(wrapper_6.get_collector_method("stream_writer_A"))

    try:
        thread_d.start()
        thread_i.start()
        wrapper_6.start()

        sleep(10)

        assert thread_d.arCharacter == {1: 'a'}
        assert thread_i.arCharacter == {1: 'a', 2: 'b', 3: 'c', 4: 'd', 5: '', 6: '', 7: '', 8: '', 9: ''}
    except RuntimeError:
        pass

    ###################################################################################################################


class ThreadH(Thread):
    def __init__(self, initial_text, reading_collector):
        super(ThreadH, self).__init__()
        self.initial_text = initial_text
        self.reading_collector = reading_collector

    def run(self):
        try:
            self.reading_collector.add_character(self.initial_text[0])
            sleep(1)
            self.reading_collector.add_character(self.initial_text[1])
            sleep(1)
            self.reading_collector.add_character(self.initial_text[2])
            sleep(1)
            self.reading_collector.add_character(self.initial_text[3])
            sleep(1)
            self.reading_collector.add_character(self.initial_text[4])
            sleep(1)
            self.reading_collector.add_character(self.initial_text[5])
            sleep(1)
            self.reading_collector.add_character(self.initial_text[6])
            sleep(1)
            self.reading_collector.add_character(self.initial_text[7])
            sleep(1)
            self.reading_collector.add_character(self.initial_text[8])
            sleep(1)
            self.reading_collector.add_character(self.initial_text[9])
            sleep(1)
        except Exception as error:
            raise error
        finally:
            self.reading_collector.close()


def test_reading_collector():
    initial_text = 'abcdefghijklmnop'

    collector = ReadingCollector()
    ThreadH(initial_text, collector).start()

    assert collector.read(1) == 'a'
    assert collector.read(5) == 'bcdef'
    assert collector.read(100) == 'ghij'
    assert collector.read(200) == ''

    ###################################################################################################################

    initial_text_2 = 'abcd'

    collector_2 = ReadingCollector()
    ThreadH(initial_text_2, collector_2).start()

    assert collector_2.read(1) == 'a'
    assert collector_2.read(2) == 'bc'
    assert collector_2.read(3) == 'd'
    assert collector_2.read(10) == ''

    ###################################################################################################################


class ThreadE(ThreadRoot):
    def __init__(self, method, name):
        super(ThreadE, self).__init__(method, name)

    def run(self):
        sleep(4)
        self.method(self.name)


class ThreadF(ThreadRoot):
    def __init__(self, method, name):
        super(ThreadF, self).__init__(method, name)

    def run(self):
        sleep(8)
        self.method(self.name)


class ThreadG(ThreadRoot):
    def __init__(self, method, name, wrapper):
        super(ThreadG, self).__init__(method, name)
        self.wrapper = wrapper

    def run(self):
        sleep(8)
        self.wrapper.remove(self.name)


def test_closing_wrapper():
    initial_text = 'abcd'

    closer_1 = StringIO(initial_text)
    wrapper_1 = ClosingWrapper(closer_1.close)

    assert wrapper_1.arClosingEvent == dict()
    method_1_a = wrapper_1.get_method("stream_reader_A")
    assert method_1_a == wrapper_1.close
    assert list(wrapper_1.arClosingEvent.keys()) == ["stream_reader_A"]

    method_1_b = wrapper_1.get_method("stream_reader_B")
    assert method_1_b == wrapper_1.close
    assert list(wrapper_1.arClosingEvent.keys()) == ["stream_reader_A", "stream_reader_B"]

    method_1_a_2 = wrapper_1.get_method("stream_reader_A")
    assert method_1_a_2 == wrapper_1.close
    assert list(wrapper_1.arClosingEvent.keys()) == ["stream_reader_A", "stream_reader_B"]

    ###################################################################################################################

    closer_2 = StringIO(initial_text)
    wrapper_2 = ClosingWrapper(closer_2.close)

    thread_e = ThreadE(wrapper_2.get_method("stream_reader_A"), "stream_reader_A")
    thread_f = ThreadF(wrapper_2.get_method("stream_reader_B"), "stream_reader_B")

    thread_e.start()
    thread_f.start()
    wrapper_2.start()
    assert closer_2.closed is False
    sleep(5)
    assert closer_2.closed is False
    sleep(5)
    assert closer_2.closed is True

    ###################################################################################################################

    closer_3 = StringIO(initial_text)
    wrapper_3 = ClosingWrapper(closer_3.close)

    thread_e = ThreadE(wrapper_3.get_method("stream_reader_A"), "stream_reader_A")
    thread_g = ThreadG(wrapper_3.get_method("stream_reader_B"), "stream_reader_B", wrapper_3)

    thread_e.start()
    thread_g.start()
    wrapper_3.start()
    assert closer_3.closed is False
    sleep(2)
    assert closer_3.closed is False
    sleep(3)
    assert closer_3.closed is False
    sleep(4)
    assert closer_3.closed is True

    ###################################################################################################################
