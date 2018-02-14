<img align="left" src="MatriochkasLogo.png" alt="Matriochkas logo" width="75" height="75">
 
 Matriochkas
===========

Qu'est ce que c'est ?
--------------------

Matriochkas est une petite bibliothèque de parsage de texte simple et rapide à prendre en main.

Elle permet actuellement de récupérer les positions des caractères correspondant à un schéma de parsage défini par l'utilisateur puis d'y effectuer une modification selon un schéma de modification défini lui aussi par l'utilisateur.

Comment ça marche ?
-------------------

Le parsage ou la modification d'un texte s'effectue caractère par caractère à partir du début du texte. On peut représenter le caractère actuellement analysé par un curseur. 

Matriochkas permet de conçevoir des schémas de parsage ou de modification sur une chaine de caractères. C'est donc plusieurs caractères qui sont lus ou écrits à chaque incrémentation de la position du curseur. Ceux-ci sont représentés par leurs positions relatives au curseur. Ainsi par exemple, si l'on souhaite analyser la valeur du caractère précédant le curseur, on le représentera par la valeur *-1*. Les valeurs choisies par l'utilisateur définissent donc la longueur de la chaine de caractères (que l'on appellera *fenêtre du curseur*) dans laquelle certains caractères sont à analyser.  

Exemple
-------

L'objectif est triple:  
- Détecter les mots du texte et les séparer par un point-virgule :
- Détecter les phrases du texte et les séparer par une étoile :
- Obtenir un tableau de dimension 2 contenant les mots de chaque phrase :

Ci-dessous le texte à parser:

    # Texte à parser
    text = 'Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et
    dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip
    ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu
    fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt
    mollit anim id est laborum'
    
Création du schéma de parsage:
    
    # Importation de la librairie
    from matriochkas import *
    
    # Création de la condition de récupération d'un caractère ('|' correspond à l'opérateur 'OR')
    # D'autres opérateurs sont disponibles ('&' -> 'AND' / '^' -> 'XOR' / '~' -> 'NOT')

    # Condition pour la détection des espaces seuls
    space_parsing_pattern = ParsingCondition(' ') & (~ParsingCondition('.', rel_position=-1)) & (~ParsingCondition(',', rel_position=-1))

    # Condition pour la détection des ponctuations
    punctuation_parsing_pattern = ParsingCondition(', ', key_word='key 1') | ParsingCondition('. ', key_word='key 1')

    # Schéma de parsage final pour les détections de mots
    word_parsing_pattern = space_parsing_pattern | punctuation_parsing_pattern
    
    # Schéma de parsage final pour les détections de phrases
    sentence_parsing_pattern = ParsingCondition('. ')
    
Le paramètre *key_word* permet de définir un mot clé afin de savoir si les caractères dans le résultat de parsage ont été sélectionnés par l'une des conditions qui contient ce mot clé.

    # Création d'un bloc de parsage qui définit la limite de parsage avec cette condition
    # Dans ce cas on souhaite parser le texte entier avec cette condition d'où None
    word_parsing_block = word_parsing_pattern >> None
    sentence_parsing_block = sentence_parsing_pattern >> None
 
    # Création du pipeline de parsage qui définit l'ordre d'utilisation des blocs
    # Dans ce cas il n'y a qu'un seul bloc d'où None
    word_parsing_pipeline = word_parsing_block + None
    sentence_parsing_pipeline = sentence_parsing_block + None
    
 
Parsage du texte pour la détection des mots:

    # Création du reader pour analyser le texte pour la détection des mots
    word_reader = StreamReader(text, result_type=ParsingResultType.REFERENCE)

    # Parsage du texte avec le pipeline de parsage des mots précédemment créé
    word_parsing_result = word_reader.read(word_parsing_pipeline, close_stream=False)
    
On obtient en resultat (la variable *word_parsing_result*) un objet qui contient entre autres une liste de tuples contenant la position du curseur au moment du parsage, la valeur du caratère et l'ensemble des mots clés des conditions qui ont validées ce caractère.

    Parsing result :
        Stream class : StringIO
        Origin : ParsingResultOrigin.READING
        Result type : ParsingResultType.REFERENCE
        Inputs : {'args': (), 'kwargs': {'reference': <_io.StringIO object at 0x00468D50>}}
        Index result : [(5, ' ', Counter({None: 3})), (11, ' ', Counter({None: 3})), (17, ' ', Counter({None: 3})), 
        (21, ' ', Counter({None: 3})), (26, ',', Counter({'key 1': 2})), ... (432, ' ', Counter({None: 3})), 
        (436, ' ', Counter({None: 3}))]
        
Le paramètre *result_type* du constructeur permet de définir si l'on souhaite récupérer la référence de l'objet de stream créé lors de l'appel de la méthode *read* dans le résultat de parsage ou uniquement récupérer ses paramètres d'entrées. Dans cette exemple, on souhaite effectuer un deuxième parsage afin de récupérer un résultat pour détecter les phrases. Il est donc préférable de choisir *result_type=ParsingResultType.REFERENCE* afin d'utiliser le même stream pour le deuxième parsage et ainsi éviter la création d'un deuxième stream.  
De plus, le stream ne doit pas être fermé à la fin du premier parsage afin de pouvoir être utilisé au second d'où le paramètre *close_stream=False* à l'appel de la méthode *read*.  

Parsage du texte pour la détection des phrases:

    # Création du reader pour analyser le texte pour la détection des phrases
    # On utilise une instance de la LinkedStreamReader car elle prend en paramètre un résultat de parsage
    sentence_reader = LinkedStreamReader(word_parsing_result)

    # Parsage du texte avec le pipeline de parsage des phrases précédemment créé
    sentence_parsing_result = sentence_reader.read(sentence_parsing_pipeline, close_stream=False)

Création du résulat de parsage modifié pour la détection des mots:

    #Création du schéma de modification
    word_modification_pattern = ModificationRemove() + ModificationAdd(';') + ModificationRemove(rel_position=1, key_word='key 1')

    #Application de la modification au résultat de parsage précédent
    word_final_parsing_result = word_modification_pattern.generate_parsing_result(word_parsing_result)

Le paramère *key_word* dans les schémas de modifition permet d'éffectuer certaines modifications uniquement lorsque le tuple du résultat de parsage possède ce mot clé. Dans cet exemple, on souhaite supprimer le caractère suivant le curseur uniquement dans le cas où une ponctuation a été détectée (un espace).

Création du résultat de parsage modifié pour la détection des phrases:

    #Création du schéma de modification
    sentence_modification_pattern = ModificationRemove() + ModificationAdd('*') + ModificationRemove(rel_position=1)

    #Application de la modification au résultat de parsage précédent
    sentence_final_parsing_result = sentence_modification_pattern.generate_parsing_result(sentence_parsing_result)

On obtient deux nouveaux résultats de parsage (la variable *word_final_parsing_result* et *sentence_final_parsing_result*) qui contiennent entre autres les positions et les nouvelles valeurs des caractères à modifier:

    #Résultat de parsage final pour la détection des mots
    Parsing result :
        Stream class : StringIO
        Origin : ParsingResultOrigin.MODIFICATION
        Result type : ParsingResultType.REFERENCE
        Inputs : {'args': (), 'kwargs': {'reference': <_io.StringIO object at 0x00468D50>}}
        Index result : [(5, ''), (5, ';', <ModificationSide.RIGHT: 1>), ..., (436, ';', <ModificationSide.RIGHT: 1>)]
        
    #Résultat de parsage final pour la détection des phrases
    Parsing result :
        Stream class : StringIO
        Origin : ParsingResultOrigin.MODIFICATION
        Result type : ParsingResultType.REFERENCE
        Inputs : {'args': (), 'kwargs': {'reference': <_io.StringIO object at 0x00468D50>}}
        Index result : [(123, ''), (123, '*', <ModificationSide.RIGHT: 1>), ...]
        
Création des textes modifiés:

    #Création des writer pour créer des textes modifiés
    word_writer = StreamWriter()
    sentence_writer = StreamWriter()
    
    #Création des textes modifiés
    new_text_1 = word_writer.write(word_final_parsing_result, close_reading_stream=False)
    new_text_2 = sentence_writer.write(sentence_final_parsing_result, close_reading_stream=False)
    
Création de la convergence des deux resultats de parsages modifiés précédents (*sentence_final_parsing_result* et *word_final_parsing_result*).

    merged_modification_result = sentence_final_parsing_result + word_final_parsing_result
    
Dans le cas où deux positions sont identiques dans les deux résultats de parsages, la position dans le premier opérande (dans ce cas *sentence_final_parsing_result*) sera gardée. Dans ce cas, cette condition permet de supprimer les séparations des mots de fin de phrase (;) et de les remplacer par des séparations de phrases (*):
    
    Parsing result :
        Stream class : StringIO
        Origin : ParsingResultOrigin.MODIFICATION
        Result type : ParsingResultType.REFERENCE
        Inputs : {'args': (), 'kwargs': {'reference': <_io.StringIO object at 0x051F6080>}}
        Index result : [(5, ''), (5, ';', <ModificationSide.RIGHT: 1>), (11, ''), (11, ';', <ModificationSide.RIGHT: 1>), ..., (122, ''), (122, '*', <ModificationSide.RIGHT: 1>), ...]

Création du tableau:

    array = StreamWriter(stream_class=StreamTwoDimDeque, column_separator=';', row_separator='*').write(merged_modification_result, close_reading_stream=True)
    
La classe *StreamWriter* génère par défaut une chaine de caractères (*str*) à l'appel de la méthode *write*. Il est donc nécessaire d'utiliser le paramètre *stream_class* lors de la création d'une instance avec comme valeur la classe *StreamTwoDimDeque* (classe qui nécessite à l'instanciation deux paramètres : *column_separator*, le caractère de séparation des colonnes] et *row_separator*, le caractère de séparation des lignes.
    
On obtient ces textes et ce tableau:

    Lorem;ipsum;dolor;sit;amet;consectetur;adipiscing;elit;sed;do;eiusmod;tempor;incididunt;ut;labore;et;dolore;magna;aliqua;
    Ut;enim;ad;minim;veniam;quis;nostrud;exercitation;ullamco;laboris;nisi;ut;aliquip;ex;ea;commodo;consequat;Duis;aute;irure;
    dolor;in;reprehenderit;in;voluptate;velit;esse;cillum;dolore;eu;fugiat;nulla;pariatur;Excepteur;sint;occaecat;cupidatat;non;
    proident;sunt;in;culpa;qui;officia;deserunt;mollit;anim;id;est;laborum
    
    Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua*
    Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat*
    Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur*
    Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum
    
    deque([ deque(['Lorem', 'ipsum', 'dolor', 'sit', 'amet', 'consectetur', 'adipiscing', 'elit', 'sed', 'do', 'eiusmod', 'tempor', 'incididunt', 'ut', 'labore', 'et', 'dolore', 'magna', 'aliqua']), 
            deque(['Ut', 'enim', 'ad', 'minim', 'veniam', 'quis', 'nostrud', 'exercitation', 'ullamco', 'laboris', 'nisi', 'ut', 'aliquipex', 'ea', 'commodo', 'consequat']), 
            deque(['Duis', 'aute', 'irure', 'dolor', 'in', 'reprehenderit', 'in', 'voluptate', 'velit', 'esse', 'cillum', 'dolore', 'eufugiat', 'nulla', 'pariatur']), 
            deque(['Excepteur', 'sint', 'occaecat', 'cupidatat', 'non', 'proident', 'sunt', 'in', 'culpa', 'qui', 'officia', 'deseruntmollit', 'anim', 'id', 'est', 'laborum'])])
    
Ci-dessous l'exemple entier et condensé:

    from matriochkas import *
    
    text = 'Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et ' \
           'dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip' \
           'ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu' \
           'fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt' \
           'mollit anim id est laborum'

    space_parsing_pattern = ParsingCondition(' ') & (~ParsingCondition('.', rel_position=-1)) & (~ParsingCondition(',', rel_position=-1))
    punctuation_parsing_pattern = ParsingCondition(', ', key_word='key 1') | ParsingCondition('. ', key_word='key 1')

    word_parsing_pipeline = ((space_parsing_pattern | punctuation_parsing_pattern) >> None) + None
    sentence_parsing_pipeline = (ParsingCondition('. ') >> None) + None

    word_parsing_result = StreamReader(text, result_type=ParsingResultType.REFERENCE).read(word_parsing_pipeline, close_stream=False)
    sentence_parsing_result = LinkedStreamReader(word_parsing_result).read(sentence_parsing_pipeline, close_stream=False)

    word_modification_pattern = ModificationRemove() + ModificationAdd(';') + ModificationRemove(rel_position=1, key_word='key 1')
    word_final_parsing_result = word_modification_pattern.generate_parsing_result(word_parsing_result)

    sentence_modification_pattern = ModificationRemove() + ModificationAdd('*') + ModificationRemove(rel_position=1)
    sentence_final_parsing_result = sentence_modification_pattern.generate_parsing_result(sentence_parsing_result)
    merged_modification_result = sentence_final_parsing_result + word_final_parsing_result

    new_text_1 = StreamWriter().write(word_final_parsing_result, close_reading_stream=False)
    new_text_2 = StreamWriter().write(sentence_final_parsing_result, close_reading_stream=False)
    array = StreamWriter(stream_class=StreamTwoDimDeque, column_separator=';', row_separator='*').write(merged_modification_result, close_reading_stream=True)

    print(new_text_1)
    print(new_text_2)
    print(array)
