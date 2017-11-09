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
    parsing_pattern = ParsingCondition(' ') | ParsingCondition(', ') | ParsingCondition('. ')
    
    # Création d'un bloc de parsage qui défini la limite de parsage avec cette condition
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
    
On obtient en resultat (la variable *parsing_result*) un objet qui contient entre autres une liste de tuples contenant la position du curseur au moment du parsage et la valeur du caratère.

    Parsing result :
        Stream class : StringIO
        Inputs : {'args': ('Lorem ... laborum',), 'kwargs': {}}
        Index result : [(5, ' '), (11, ' '), (17, ' '), (21, ' '), (26, ','), (27, ' '), (39, ' '), (50, ' '), (55, ','), (56, ' '), (60, ' '), (63, ' '), (71, ' '), (78, ' '), (89, ' '), (92, ' '), (99, ' '), (102, ' '), (109, ' '), (115, ' '), (122, '.'), (123, ' '), (126, ' '), (131, ' '), (134, ' '), (140, ' '), (147, ','), (148, ' '), (153, ' '), (161, ' '), (174, ' '), (182, ' '), (190, ' '), (195, ' '), (198, ' '), (206, ' '), (209, ' '), (212, ' '), (220, ' '), (230, '.'), (231, ' '), (236, ' '), (241, ' '), (247, ' '), (253, ' '), (256, ' '), (270, ' '), (273, ' '), (283, ' '), (289, ' '), (294, ' '), (301, ' '), (308, ' '), (311, ' '), (318, ' '), (324, ' '), (333, '.'), (334, ' '), (344, ' '), (349, ' '), (358, ' '), (368, ' '), (372, ' '), (381, ','), (382, ' '), (387, ' '), (390, ' '), (396, ' '), (400, ' '), (408, ' '), (417, ' '), (424, ' '), (429, ' '), (432, ' '), (436, ' ')]
        
Création du résulat de parsage modifié:

    #Création du schéma de modification
    modification_pattern = ModificationRemove() + ModificationAdd(';')

    #Application de la modification au résultat de parcage précédent
    final_parsing_result = modification_pattern.generate_parsing_result(parsing_result)

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

    Lorem;ipsum;dolor;sit;amet;;consectetur;adipiscing;elit;;sed;do;eiusmod;tempor;incididunt;ut;labore;et;dolore;magna;aliqua;;
    Ut;enim;ad;minim;veniam;;quis;nostrud;exercitation;ullamco;laboris;nisi;ut;aliquip;ex;ea;commodo;consequat;;Duis;aute;irure;
    dolor;in;reprehenderit;in;voluptate;velit;esse;cillum;dolore;eu;fugiat;nulla;pariatur;;Excepteur;sint;occaecat;cupidatat;non;
    proident;;sunt;in;culpa;qui;officia;deserunt;mollit;anim;id;est;laborum
    
Limitation
----------

Dans le résultat de l'exemple précédent, on peut remarquer des champs vides qui correspondent aux détections de conditions à plusieurs caractères (*' .'* ou *' ,'*). La cause de cette particularité vient de la méthode de création du résultat de parsage final qui applique la même opération à tous les caractères du résultat de parsage intermédiaire. Or dans l'idéal la modification devrait être différente pour une condition avec le caractère *' '* et pour une condition avec la chaine de caractères *' ,'* ou *' .'*.

Cette amélioration ne devrait pas tarder à arriver dans une prochaine release ;)
