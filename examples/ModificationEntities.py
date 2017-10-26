# coding: utf8

from core.ModificationEntities import ModificationAdd
from core.ModificationEntities import ModificationRemove
from core.ModificationEntities import ModificationSide

from core.ParsingEntities import ParsingCondition
from core.IO import StreamReader
from core.IO import StreamWriter

text = 'Lorem-ipsum-dolor-sit-amet, consectetur-adipiscing-elit, sed-do-eiusmod-tempor-incididunt-ut-labore-et-' \
       'dolore-magna-aliqua. Ut-enim-ad-minim-veniam, quis-nostrud-exercitation-ullamco-laboris-nisi-ut-aliquip-' \
       'ex-ea-commodo-consequat. Duis-aute-irure-dolor-in-reprehenderit-in-voluptate-velit-esse-cillum-dolore-eu-' \
       'fugiat-nulla-pariatur. Excepteur-sint-occaecat-cupidatat-non-proident, sunt-in-culpa-qui-officia-deserunt-' \
       'mollit-anim-id-est-laborum. -hello world !-HELLO WORLD !-Hello World !-Another Sentence !%Another Sentence !' \
       '-HELLO World !-'

blocA = (ParsingCondition(',') | (ParsingCondition('.')) & ParsingCondition(' ', rel_position=1)) >> \
        (ParsingCondition('.') & ParsingCondition(' ', rel_position=1) & ParsingCondition('-', rel_position=2))
blocB = (ParsingCondition('-') | ParsingCondition('%')) >> None
pipeline = blocA + blocB
streamReader = StreamReader(text)
parsing_result = streamReader.read(pipeline)

'''modification_replace = ModificationAdd('@') + ModificationRemove()
modification_add = ModificationAdd('2', modification_side=ModificationSide.LEFT)
modification = modification_replace + modification_add
print('Parsing result before modifications : ' + str(parsing_result.arIndex))
print('Parsing result after modifications : ' + str(modification.generate_parsing_result(parsing_result).arIndex))'''

streamWriter = StreamWriter()
print(streamWriter.write(ModificationRemove().generate_parsing_result(parsing_result)))