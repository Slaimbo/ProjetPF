type Code = Int

--class (Eq a) => Table a where
class Table a where
    empty :: a
    insert :: a -> String -> a
    
    codeOf :: a -> String -> Maybe Code
    stringOf :: a -> Code -> Maybe String
    isIn :: a -> String -> Bool
    split :: a -> String -> (String, Maybe Code, String)

-------------------------- Question 1 ----------------------------------------

-- Translate string into code with table, untranslated string as parameter --

lzwEncode :: Table a => a -> String -> [Code]
lzwEncode table [] = []
lzwEncode table str = 
  case split table str of
    (_, Nothing, _) -> []
    (prefix, Just code, (head:tail)) -> 
        code : lzwEncode (insert table (prefix ++ [head])) (head:tail)
    (prefix, Just code, []) -> [code]

--translate code into string with table, oldtrans and untranslated code as parameter-- 

lzw_Decode :: Table a => a -> String -> [Code] -> String
lzw_Decode table oldtrans [] = oldtrans
lzw_Decode table oldtrans (x:xs) = case stringOf table x of
        Just newtrans -> oldtrans ++ lzw_Decode (insert table (oldtrans ++ [head newtrans])) newtrans xs
        Nothing -> oldtrans ++ lzw_Decode (insert table (oldtrans ++ [head newtrans])) newtrans xs
            where newtrans = oldtrans ++ [head oldtrans]

--main decode function to decode the first code and call lzw_Decode with table, translating string and rest of code--

lzwDecode :: Table a => a -> [Code] -> String
lzwDecode table [] = []
lzwDecode table (x:xs) = case stringOf table x of
        Just string -> lzw_Decode table string xs
        Nothing -> []

-------------------------- Question 2 ----------------------------------------

data Dico = Dico [(Code, String)]


 --search the biggest code in Dico and add this with new string give in parameter--

max_dico :: Code -> [(Code, String)] -> Code
max_dico max [] = max+1
max_dico max (x:xs) = if (fst x) > max
                      then max_dico (fst x) xs
                      else max_dico max xs


split_2 :: Dico -> String -> String -> Maybe Code -> (String, Maybe Code, String)

split_2 (Dico dic) startstr [] lastcode = (startstr, lastcode, [])
split_2 (Dico dic) startstr (x:xs) lastcode = if codeOf (Dico dic) (startstr ++ [x]) == Nothing
                                                then (startstr, lastcode, x:xs)
                                                else split_2 (Dico dic) (startstr ++ [x]) xs (codeOf (Dico dic) (startstr ++ [x]))


instance Table Dico where

 --Return an empty dico--

    empty = Dico []                                                          

 --insert a new code and associated word in the Dico--

    insert (Dico []) str = Dico ( [(0, str)] )                              
    insert (Dico (x:xs)) str = Dico ( (max_dico (fst x) xs, str) : (x:xs) )    

 --return True if string parameter is in Dico, else false--

    isIn (Dico []) str = False
    isIn (Dico (x:xs)) str = if (snd x) == str
                        then True
                        else isIn (Dico xs) str

 --return associated code of word or nothing-- sera utiliser pas split

    codeOf (Dico []) str = Nothing
    codeOf (Dico (x:xs)) str = if (snd x) == str
                                then Just (fst x)
                                else codeOf (Dico xs) str

--return associated string of code or nothing--

    stringOf (Dico []) code = Nothing
    stringOf (Dico (x:xs)) code = if (fst x) == code
                                then Just (snd x)
                                else stringOf (Dico xs) code


    split (Dico dic) (x:xs) = if codeOf (Dico dic) [x] == Nothing
                                then ([], Nothing, (x:xs))
                                else split_2 (Dico dic) [x] xs (codeOf (Dico dic) [x])


-- Test de la partie 1

--lzwDecode (Dico [ (0, "a"), (1, "b"), (2, "c") ] ) (lzwEncode (Dico [ (0, "a"), (1, "b"), (2, "c") ] ) "abccba")
initDico :: String -> Dico

initDico (x:[]) = Dico [(0, [x])]
initDico (x:xs) = insert (initDico xs) [x] 

testDico str = lzwDecode dico (lzwEncode dico str)
            where dico = initDico "abcdefghijklmnopqrstuvwxyz "


-------------------------- Question 4 ----------------------------------------

-- L'arbre de prefixe.
data Arbre = Noeud (Maybe Char, Maybe Code, [Arbre])


--retourne les arbres qui n'ont pas matché, l'arbre qui match, le reste des arbres.
inChildren :: [Arbre] -> [Arbre] -> Char -> [[Arbre]]
inChildren [] poubelle char = []
inChildren ((Noeud (car, code, children)):xs) poubelle char = if car == Just char
                                        then [poubelle, [x], xs]
                                        else inChildren xs (x:poubelle) char
                                            where x = Noeud (car, code, children)

-- Retourne le plus grand élément de la liste donné en entré
maxi :: [Maybe Code] -> Code
maxi ((Just code):[]) = code 
maxi ((Just code):xs) = if code > (maxi xs)
                         then code
                         else maxi xs

getcodeFrom (Noeud (_, _, fils)) = getcode fils

-- Retourne la liste de tout les code contenu dans l'arbre
getcode :: [Arbre] -> [Maybe Code]
getcode [] = []
getcode ((Noeud (Just char, Nothing, [])):[]) = []
getcode ((Noeud (Just char, Just code, [])):[]) = [Just code]

getcode ((Noeud (Just char, Just code, children_f)):[]) = [Just code] ++ (getcode children_f)                                              
getcode ((Noeud (Just char, Nothing, children_f)):[]) = getcode children_f

getcode ((Noeud (Just char, Just code, children_f)):ys) = [Just code] ++ (getcode ys) ++ (getcode children_f)
getcode ((Noeud (Just char, Nothing, children_f)):ys) = (getcode ys) ++ (getcode children_f)


--Creation des branches quand plus aucun chemin ne correspond a mon inclusion
insert2 (Noeud (Nothing, Nothing, [])) str newcode = if (length str == 1)
                                                       then Noeud (Just (head str), Just (newcode+1), [])
                                                       else Noeud (Just (head str), Nothing, [insert2 (Noeud (Nothing, Nothing, [])) (tail str) newcode ] )

--parcour de l'arbre tant que cela correspond a mon insertion
--Eventuellement gerer le cas insert aaaa puis insert aa donc le cas ou code = Nothing si bug sinon osef
insert2 (Noeud (char, code, children) ) (y:ys) newcode = if (length mathch_child) == 0
                                                             then Noeud (char, code, children ++ [insert2 (Noeud (Nothing, Nothing, [])) (y:ys) newcode ] )
                                                             else Noeud (char, code, ( (mathch_child !! 0) ++ [(insert2 ( (mathch_child !! 1) !! 0) ys newcode)] ++ (mathch_child !! 2) ) )
                                                                where father = Noeud (char, code, children)
                                                                      mathch_child = inChildren children [] y   --(nomatchbefore, match, nomatchafter)

stringOf2 :: Arbre -> Code -> String -> String
stringOf2 (Noeud (Just p_char, p_code, []) ) code str = if (Just code) == p_code
                                                       then str ++ [p_char]
                                                       else []
stringOf2 (Noeud (Nothing, _, []) ) code str = []

stringOf2 (Noeud (Just p_char, p_code, (Noeud (Just f_char, f_code , f_children)):xs) ) code str =      --Je recherche dans le reste de mes fils et les fils de mon fils ou je retourne le code si trouvé
            if f_code == (Just code)
                then str ++ [p_char] ++ [f_char]
                else case stringOf2 (Noeud (Just p_char, p_code, xs) ) code str of
                        []  -> stringOf2 (Noeud (Just f_char, f_code, f_children)) code (str ++ [p_char])
                        any -> any

--si str est dans l'arbre meme s'il n'as pas de code.
isIn_sa (Noeud (Just p_char, _, _) ) [] = True
isIn_sa (Noeud (_, _, []) ) (x:xs) = False
isIn_sa (Noeud (p_char, p_code, (Noeud (Just f_char, f_code , f_children)):xs) ) str = 
                if f_char == (head str)
                    then isIn_sa (Noeud (Just f_char, f_code , f_children)) (tail str)
                    else isIn_sa (Noeud (p_char, p_code, xs) ) str


split2 :: Arbre -> String -> String -> String -> String -> (String, Maybe Code, String)

split2 arbre str_start [] match_str end_match = (match_str, codeOf arbre match_str, end_match)
split2 arbre str_start (x:xs) match_str end_str = 
                                    if (isIn_sa arbre (str_start ++ [x])) --si le caractere x est dans un des fils
                                    then 
                                        if ( isIn arbre (str_start ++ [x]) ) --si ce caractere x posséde un code
                                            then split2 arbre (str_start ++ [x]) xs (str_start ++ [x]) xs 
                                            else split2 arbre (str_start ++ [x]) xs match_str end_str
                                    else (match_str, (codeOf arbre match_str), end_str)

   -- if (isIn arbre (str_start ++ [x])) == False)
    



-- Instanciation
instance Table Arbre where
    empty = Noeud (Nothing, Nothing, []) -- Racine de tout les sous arbre
    
    -- à iniaitliser avec un code à -1 pour l'arbre racine
    insert (Noeud (char, code, children) ) str = if getcode children == [] --Aucun code n'as ete trouvé
                                                    then insert2 (Noeud (char, code, children)) str (-1)
                                                    else insert2 (Noeud (char, code, children)) str (maxi (getcode children))
    --getcodeFrom (insert (insert(Noeud (Nothing, Just (-1), [] )) "oooo") "oo")

    codeOf (Noeud (Just p_char, Just p_code, _) ) [] = Just p_code -- si str vide et code -> OK
    codeOf (Noeud (Just p_char, Nothing, _) ) [] = Nothing         -- si str vide mais pas code
    codeOf (Noeud (_, _, []) ) _ = Nothing                         -- si str non vide et plus de fils

    codeOf (Noeud (p_char, p_code, (Noeud (Just f_char, f_code , f_children)):xs) ) str = 
            if f_char == (head str)
                then codeOf (Noeud (Just f_char, f_code , f_children)) (tail str)
                else codeOf (Noeud (p_char, p_code, xs) ) str

    
    stringOf (Noeud (Nothing, p_code, [] )) code = Nothing

    stringOf (Noeud (Nothing, p_code, x:xs ) ) code = 
                case stringOf2 x code [] of
                    []  -> stringOf (Noeud (Nothing, p_code, xs ) ) code
                    any -> Just any

    
    isIn (Noeud (Just p_char, Just p_code, _) ) [] = True
    isIn (Noeud (_, Nothing, _) ) [] = False
    isIn (Noeud (_, _, []) ) (x:xs) = False

    isIn (Noeud (p_char, p_code, (Noeud (Just f_char, f_code , f_children)):xs) ) str = 
            if f_char == (head str)
                then isIn (Noeud (Just f_char, f_code , f_children)) (tail str)
                else isIn (Noeud (p_char, p_code, xs) ) str


    split arbre str = split2 arbre [] str [] []



--lzwDecode (insert (insert (insert (insert (insert (insert (insert (insert (insert (insert (insert (insert (insert (insert (insert (insert (insert (insert (insert (insert (insert (insert (insert (insert (insert (insert ( Noeud (Nothing, Just (-1), [])) "a") "b") "c") "d") "e") "f") "g") "h") "i") "j") "k") "l") "m") "n") "o") "p") "q") "r") "s") "t") "u") "v") "w") "x") "y") "z") (lzwEncode (insert (insert (insert (insert (insert (insert (insert (insert (insert (insert (insert (insert (insert (insert (insert (insert (insert (insert (insert (insert (insert (insert (insert (insert (insert (insert ( Noeud (Nothing, Just (-1), [])) "a") "b") "c") "d") "e") "f") "g") "h") "i") "j") "k") "l") "m") "n") "o") "p") "q") "r") "s") "t") "u") "v") "w") "x") "y") "z") "abccba")


lzwDecode (insert (insert (insert (insert (insert (insert (insert (insert (insert (insert (insert (insert (insert (insert (insert (insert (insert (insert (insert (insert (insert (insert (insert (insert (insert (insert ( Noeud (Nothing, Just (-1), [])) "a") "b") "c") "d") "e") "f") "g") "h") "i") "j") "k") "l") "m") "n") "o") "p") "q") "r") "s") "t") "u") "v") "w") "x") "y") "z") [0,1,2,2,1,0]

initArbre :: String -> Arbre

initArbre (x:[]) = insert (Noeud (Nothing, Just (-1), [])) [x]
initArbre str = insert (initArbre (take ((length str) - 1) str)) [car]
                    where car = head (reverse str)



testArbre str = getcodeFrom arbre
            where arbre = initArbre " abcdefghijklmnopqrstuvwxyz"


