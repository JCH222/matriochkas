# coding: utf8

from core.ParsingEntities import ParsingOperator
from core.ParsingEntities import ParsingCondition
from core.ParsingEntities import OperatorType


text = '-hello world !-HELLO WORLD !-Hello World !-Another Sentence !%Another Sentence !-HELLO World !-'


# ParsingCondition creation
condition = ParsingCondition('-')
# ParsingCondition manipulation
for i in range(0, len(text) - condition.get_max_position()):
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
# Manipulation 1
conditionA = ParsingCondition('h')
conditionB = ParsingCondition('H')
conditionC = ParsingCondition('!', rel_position=12)
operator = (conditionA | conditionB) & conditionC
for i in range(0, len(text) - operator.get_max_position()):
    if operator.check(text, i):
        print("'hello world !' (not case sensitive) written between character ", str(i), " and character ", str(i+12), sep=" ")
# Manipulation 2
conditionD = ParsingCondition('O', rel_position=4)
conditionE = ParsingCondition('d', rel_position=10)
operator2 = conditionD & conditionE
operator3 = operator & operator2
for i in range(0, len(text) - operator3.get_max_position()):
    if operator3.check(text, i):
        print("'HELLO world !' written between character ", str(i), " and character ", str(i+12), sep=" ")


# ParsingEntity equality
if operator3 == (((conditionA | conditionB) & conditionC) & (conditionD & conditionE)):
    print("operator3 is equal to '((conditionA | conditionB) & conditionC) & (conditionD & conditionE)'")


# ParsingEntity inequality
if operator3 != conditionA | conditionB & conditionC & conditionD & conditionE:
    print("but operator3 is not equal to 'conditionA | conditionB & conditionC & conditionD & conditionE'")


# ParsingBlock creation
condition = ParsingCondition('%')
block = ((conditionA | conditionB) & conditionC) >> condition
# ParsingBlock manipulation
for i in range(0, len(text) - block.get_max_position()):
    result = block.check(text, i)
    if result[0]:
        print(text[i:i+12+1])
    if result[1]:
        break
