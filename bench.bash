#!/bin/bash
make
rm -f res.comp res.decomp res2.comp res2.decomp

echo "################### Test du Dictionnaire ###################"
taille=`du -h test.txt`
temp=`echo "Encodage du fichier : $taille dans res.comp ..." | tr '\t' ' '` 
echo $temp

./lzwCompressDico.exe test.txt res.comp

taille=`du -h res.comp`
temp=`echo "Fichier compressé : $taille" | tr '\t' ' '`
echo $temp



echo "Décompression de res.comp dans res.decomp..."

./lzwDecompressDico.exe res.comp res.decomp

taille=`du -h res.decomp`
temp=`echo "Nouvelle taille : $taille" | tr '\t' ' '`
echo $temp

echo "Comparaison entre le fichier d'origine et la version décodé ..."

origin=`cat test.txt | echo`
new=`cat res.decomp | echo`


if [ $origine -eq $new ]
then
	echo "	Succes : Les fichiers sont identiques."
else
	echo "	Fail : les fichiers sont differents."
fi

echo
echo


echo "################### Test de l'arbre ###################"
taille=`du -h test2.txt`
temp=`echo "Encodage du fichier : $taille dans res2.comp ..." | tr '\t' ' '` 
echo $temp

./lzwCompressArbre.exe test2.txt res2.comp

taille=`du -h res2.comp`
temp=`echo "Fichier compressé : $taille" | tr '\t' ' '`
echo $temp



echo "Décompression de res2.comp dans res2.decomp..."

./lzwDecompressArbre.exe res2.comp res2.decomp

taille=`du -h res2.decomp`
temp=`echo "Nouvelle taille : $taille" | tr '\t' ' '`
echo $temp

echo "Comparaison entre le fichier d'origine et la version décodé ..."

origin=`cat test2.txt | echo`
new=`cat res2.decomp | echo`


if [ $origine -eq $new ]
then
	echo "	Succes : Les fichiers sont identiques."
else
	echo "	Fail : les fichiers sont differents."
fi

