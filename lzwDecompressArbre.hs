import qualified Data.ByteString.Lazy as BS
import Data.Bits
import System.Environment as SE

import Data.Word
import Data.Int
import Data.Char (ord)

type Byte = Word8
type String8 = [Byte]
type Int10 = Word16
type Code = Int10


--class (Eq a) => Table a where
class Table a where
    empty :: a
    insert :: a -> String8 -> a
    
    codeOf :: a -> String8 -> Maybe Code
    stringOf :: a -> Code -> Maybe String8
    isIn :: a -> String8 -> Bool
    split :: a -> String8 -> (String8, Maybe Code, String8)


--translate code into string with table, oldtrans and untranslated code as parameter-- 
lzw_Decode :: Table a => a -> String8 -> [Code] -> String8
lzw_Decode table oldtrans [] = oldtrans
lzw_Decode table oldtrans (x:xs) = case stringOf table x of
        Just newtrans -> oldtrans ++ lzw_Decode (insert table (oldtrans ++ [head newtrans])) newtrans xs
        Nothing -> oldtrans ++ lzw_Decode (insert table (oldtrans ++ [head newtrans])) newtrans xs
            where newtrans = oldtrans ++ [head oldtrans]

--main decode function to decode the first code and call lzw_Decode with table, translating string and rest of code--
lzwDecode :: Table a => a -> [Code] -> String8
lzwDecode table [] = []
lzwDecode table (x:xs) = case stringOf table x of
        Just string -> lzw_Decode table string xs
        Nothing -> []



-- L'arbre de prefixe.
newtype Arbre = Noeud (Maybe Byte, Maybe Code, [Arbre]) deriving (Show)


--retourne les arbres qui n'ont pas matché, l'arbre qui match, le reste des arbres.
inChildren :: [Arbre] -> [Arbre] -> Byte -> [[Arbre]]
inChildren [] poubelle char = []
inChildren ((Noeud (car, code, children)):xs) poubelle char =
                                        if car == (Just char)
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

insert2 (Noeud (Nothing, Nothing, [])) (x:xs) newcode = if (xs == [])
                                                       then Noeud (Just x, Just (newcode+1), [])
                                                       else Noeud (Just x, Nothing, [ insert2 (Noeud (Nothing, Nothing, [])) xs (newcode) ] )

--parcour de l'arbre tant que cela correspond a mon insertion
--Eventuellement gerer le cas insert aaaa puis insert aa donc le cas ou code = Nothing si bug sinon osef
insert2 (Noeud (char, code, children) ) (y:ys) newcode = 
                    if (length mathch_child) == 0
                         then Noeud (char, code, children ++ [insert2 (Noeud (Nothing, Nothing, [])) (y:ys) newcode ] )
                         else Noeud (char, code, ( (mathch_child !! 0) ++ [(insert2 ( (mathch_child !! 1) !! 0) ys newcode)] ++ (mathch_child !! 2) ) )
                    where father = Noeud (char, code, children)
                          mathch_child = inChildren children [] y   --(nomatchbefore, match, nomatchafter)


stringOf2 :: Arbre -> Code -> String8 -> String8
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


split2 :: Arbre -> String8 -> String8 -> String8 -> String8-> (String8, Maybe Code, String8)

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


initArbre :: String8 -> Arbre
initArbre (x:[]) = insert (Noeud (Nothing, Just (-1), [])) [x]
initArbre str = insert (initArbre (take ((length str) - 1) str)) [car]
                    where car = head (reverse str)



-- Compresse le fichier src dans le fichier dest en utilisant la table table.
-- La table (initDico [0..255] ou initArbre [0..255])- fichier source - fichier ciblef
decompFile :: Table a => a -> FilePath -> FilePath -> IO ()
decompFile table src dest = do
    contents <- BS.readFile src
    BS.writeFile dest (BS.pack (lzwDecode table (byte2code (BS.unpack contents))))


byte2code :: [Byte] -> [Code]
byte2code [] = []
byte2code (x:xs:xss) = ((shiftL (fromInteger (toInteger x)) 8) + (fromInteger (toInteger xs))) : (byte2code xss)



main = do
        args <- SE.getArgs
        decompFile (initArbre [0..255]) (args !! 0) (args !! 1)









