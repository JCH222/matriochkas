# coding: utf8

import abc


class Entity(metaclass=abc.ABCMeta):
    @abc.abstractmethod
    def check(self, element, ref_position):
        pass

    @abc.abstractmethod
    def get_max_position(self):
        pass

    @abc.abstractmethod
    def get_min_position(self):
        pass
