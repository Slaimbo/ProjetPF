##################### Généralité #####################

Nous avons implémenté deux fonctions qui permettent de
générer la table initial utilisé pour les algorithmes. 
Les deux fonctions ci-dessous utilisent insert de la 
classe Table.

initDico :: String -> Dico
initArbre :: String -> Arbre

initDico :: String8 -> Dico
initArbre :: String8 -> Arbre

------------------------------------------------------

testDico :: String -> Bool
testArbre :: String -> Bool

testDico :: String8 -> Bool
testArbre :: String8 -> Bool

Les deux fonctions ci-dessus permettent de tester
facilement les fonctions d'encodage et de désencodage.
La fonction retourne True SSI si la string donné
en paramète est la même que la string issue du 
désencodage.

------------------------------------------------------

La structure de l'arbre que avons créer comporte un 
neud racine qui est l'ancêtre de chaque prefixe ce qui 
permet d'avoir qu'un seul arbre et non une liste 
d'arbres.


################ Problèmes rencontrés ################

# Lenteur de l'Arbre #

On remarque que l'algorithme avec l'arbre est plus 
lent que celui avec le dictionnaire. Or cela ne 
devrait pas être le cas car un parcours dans l'arbre
pour rechercher un prefix est plus efficace qu'une
recherche dans un dictionnaire. Cependant, cela peut 
peu etre s'expliquer par le fait que la construction 
d'un arbre est plus longue que celle d'un 
dictionnaire.

# Implémentation de Int10 #

Nous n'avons pas pu implémenter l'algorithme avec un 
type int10 car nous n'avons pas trouver de librairie 
permettant de le faire. Cependant, nous avons trouver 
les types Word8 et Word16 ce qui permet quand même de 
réduire la taille des variables.

#################### Les fichiers ####################

Instantiation Int & String    : 'parti1-String-Int.hs'
Instantiation Int16 & [Word8] : 'parti2-Byte-Int16.hs'


#Fichiers des executables :

Compression Dico  : 'lzwCompressDico.hs'
Compression Arbre : 'lzwCompressArbre.hs'

Décompression Dico  : 'lzwDecompressDico.hs'
Décompression Arbre : 'lzwDecompressArbre.hs'


#################### Utilisation #####################

Compilation des executable : make

De manière général pour les executable : 

./lzwCompress____.exe <source> <cible>

Compress ou déconpresse à partir de la source dans 
la cible.

Tester les executables : ./bench.bash

Ce scripte compresse et décompresse puis compare la
cible à la version décompréssé et ce pour l'arbre et 
le dictionnaire.





