# coding: utf8

from core.ParsingEntities import ParsingCondition
from core.IO import StreamReader, StreamWriter
from core.ModificationEntities import ModificationAdd, ModificationRemove

import pandas


tabulation_condition = (~ParsingCondition('\t', rel_position=-1) & ParsingCondition('\t')) | \
                       (~ParsingCondition('\t', rel_position=1) & ParsingCondition('\t'))

space_condition = (~ParsingCondition(' ', rel_position=-1) & ParsingCondition(' ')) | \
                  (~ParsingCondition(' ', rel_position=1) & ParsingCondition(' '))

pipeline = ((tabulation_condition | space_condition) >> None) + None

parsing_result = StreamReader('chemical_elements.txt', stream_class=open).read(pipeline)
modification = (ModificationAdd(';') + ModificationRemove()).generate_parsing_result(parsing_result)

result = StreamWriter().write(modification)

ar_line = result.split('\n')
for i, line in enumerate(ar_line):
    ar_element = line.split(';')
    for j, element in enumerate(ar_element):
        if ' ' in element or '\t' in element or element == '':
            del ar_element[j]
    ar_line[i] = ar_element

result = pandas.DataFrame(ar_line)[[6, 7, 8]]
result.columns = ['Numéro atommique', 'Sigle', 'Nom']
print(result)
