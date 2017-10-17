# coding: utf8

from core.ParsingEntities import ParsingOperator
from core.ParsingEntities import ParsingCondition
from core.ParsingEntities import OperatorType


text = 'hello world !-HELLO WORLD !-Hello World !-Another Sentence !'


# ParsingCondition creation
condition = ParsingCondition('-')
# ParsingCondition manipulation
for i in range(0, len(text)):
    if condition.check(text, i):
        print("Character", str(i), "is '-'", sep=" ")


# ParsingOperator creation
conditionA = ParsingCondition('h')
conditionB = ParsingCondition('H')
operator = ParsingOperator(OperatorType.OR, conditionA, conditionB)
# ParsingOperator manipulation
for i in range(0, len(text)):
    if operator.check(text, i):
        print("Character", str(i), "is 'H' or 'h'", sep=" ")


# Operator overloading manipulation
conditionA = ParsingCondition('h')
conditionB = ParsingCondition('H')
conditionC = ParsingCondition('!', rel_position=12)
operator = (conditionA | conditionB) & conditionC
for i in range(0, len(text)):
    if operator.check(text, i):
        print("'hello world !' written between character ", str(i), " and character ", str(i+12), sep=" ")
