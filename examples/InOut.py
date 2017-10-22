# coding: utf8

from core.ParsingEntities import ParsingCondition
from core.InOut import StreamReader

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
print(streamReader.read(pipeline))
print(streamReader2.read(pipeline))
