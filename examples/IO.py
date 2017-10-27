# coding: utf8

from core.ParsingEntities import ParsingCondition
from core.ModificationEntities import ModificationAdd, ModificationRemove
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

# StreamReader creation
streamReader = StreamReader(text)
streamReader2 = StreamReader("Lorem Ipsum.txt", stream_class=open)
# StreamReader manipulation
parsing_result = streamReader.read(pipeline)
parsing_result2 = streamReader2.read(pipeline)
print(parsing_result)
print(parsing_result2)

modification_replace = ModificationAdd('@') + ModificationRemove()
# StreamWriter creation
streamWriter = StreamWriter()
streamWriter2 = StreamWriter("Lorem Ipsum 2.txt", 'w', stream_class=open)
# StreamWriter manipulation
print(streamWriter.write(modification_replace.generate_parsing_result(parsing_result)))
print(streamWriter2.write(modification_replace.generate_parsing_result(parsing_result2)))

