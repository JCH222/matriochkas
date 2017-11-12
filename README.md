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

L'objectif est de détecter les mots du texte et de les séparer par un point-virgule :

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

    # Schéma de parsage final
    parsing_pattern = space_parsing_pattern | punctuation_parsing_pattern
    
Le paramètre *key_word* permet de définir un mot clé afin de savoir si les caractères dans le résultat de parsage ont été sélectionnés par l'une des conditions qui contient ce mot clé.

    # Création d'un bloc de parsage qui définit la limite de parsage avec cette condition
    # Dans ce cas on souhaite parser le texte entier avec cette condition d'où None
    parsing_block = parsing_pattern >> None
 
    # Création du pipeline de parsage qui définit l'ordre d'utilisation des blocs
    # Dans ce cas il n'y a qu'un seul bloc d'où None
    parsing_pipeline = parsing_block + None
 
Parsage du texte:

    # Création du reader pour analyser le texte
    reader = StreamReader(text)

    # Parsage du texte avec le pipeline de parsage précédemment créé
    parsing_result = reader.read(parsing_pipeline)
    
On obtient en resultat (la variable *parsing_result*) un objet qui contient entre autres une liste de tuples contenant la position du curseur au moment du parsage, la valeur du caratère et l'ensemble des mots clés des conditions qui ont validées ce caractère.

    Parsing result :
        Stream class : StringIO
        Inputs : {'args': ('Lorem ... laborum',), 'kwargs': {}}
        Index result : [(5, ' ', Counter({None: 3})), (11, ' ', Counter({None: 3})), (17, ' ', Counter({None: 3})), 
        (21, ' ', Counter({None: 3})), (26, ',', Counter({'key 1': 2})), ... (432, ' ', Counter({None: 3})), 
        (436, ' ', Counter({None: 3}))]
        
Création du résulat de parsage modifié:

    #Création du schéma de modification
    modification_pattern = ModificationRemove() + ModificationAdd(';') + ModificationRemove(rel_position=1, key_word='key 1')

    #Application de la modification au résultat de parsage précédent
    final_parsing_result = modification_pattern.generate_parsing_result(parsing_result)

Le paramère *key_word* dans les schémas de modifition permet d'éffectuer certaines modifications uniquement lorsque le tuple du résultat de parsage possède ce mot clé. Dans cet exemple, on souhaite supprimer le caractère suivant le curseur uniquement dans le cas où une ponctuation a été détectée (un espace).

On obtient un nouveau résultat de parsage (la variable *final_parsing_result*) qui contient entre autres les positions et les nouvelles valeurs des caractères à modifier:

    Parsing result :
        Stream class : StringIO
        Inputs : {'args': ('Lorem ... laborum',), 'kwargs': {}}
        Index result : [(5, ''), (5, ';', <ModificationSide.RIGHT: 1>), ..., (436, ';', <ModificationSide.RIGHT: 1>)]
        
Création du texte modifié:

    #Création du writer pour créer un texte modifié
    writer = StreamWriter()
    
    #Création du texte modifié
    new_text = writer.write(final_parsing_result)
    
On obtient ce texte:

    Lorem;ipsum;dolor;sit;amet;consectetur;adipiscing;elit;sed;do;eiusmod;tempor;incididunt;ut;labore;et;dolore;magna;aliqua;
    Ut;enim;ad;minim;veniam;quis;nostrud;exercitation;ullamco;laboris;nisi;ut;aliquip;ex;ea;commodo;consequat;Duis;aute;irure;
    dolor;in;reprehenderit;in;voluptate;velit;esse;cillum;dolore;eu;fugiat;nulla;pariatur;Excepteur;sint;occaecat;cupidatat;non;
    proident;sunt;in;culpa;qui;officia;deserunt;mollit;anim;id;est;laborum
