type Code = Int
type Test = [(String, Code)]

--class (Eq a) => Table a where
class Table a where
    empty :: a
    insert :: a -> String -> a
    codeOf :: a -> String -> Maybe Code
    stringOf :: a -> Code -> Maybe String
    isIn :: a -> String -> Bool
    split :: a -> String -> (String, Maybe Code, String)

--lzwEncode :: Table a => a -> String -> [Code]
--lzwEncode table [] = []
--lzwEncode table str = code : lzwEncode newTable suffix
--	where
--	  (prefix, Just code, suffix) = split str
--	  newTable = case suffix of
--   	  head:tail -> insert table (prefix ++ [head])
--   	  [] -> table


-------------------------- Question 1 ----------------------------------------

lzwEncode :: Table a => a -> String -> [Code]
lzwEncode table [] = []
lzwEncode table str = 
  case split table str of
    (_, Nothing, _) -> []
    (prefix, Just code, (head:tail)) -> 
        code : lzwEncode (insert table (prefix ++ [head])) (head:tail)
    (prefix, Just code, []) -> [code]


lzw_Decode :: Table a => a -> String -> [Code] -> String
lzw_Decode table str [] = []
lzw_Decode table str (x:xs) = case stringOf table x of
        Just string -> str ++ lzw_Decode (insert table (str ++ [head string])) string xs
        Nothing -> str ++ lzw_Decode (insert table (str ++ [head str])) (str ++ [head str]) xs


lzwDecode :: Table a => a -> [Code] -> String
lzwDecode table [] = []
lzwDecode table (x:xs) = case stringOf table x of
        Just string -> lzw_Decode table string xs
        Nothing -> []

-------------------------- Question 2 ----------------------------------------

data Dico_lol = Dico [(Code, String)]

max_dico :: Code -> [(Code, String)] -> Code
max_dico max [] = max+1
max_dico max (x:xs) = if (fst x) > max
                      then max_dico (fst x) xs
                      else max_dico max xs

instance Table Dico_lol where
    empty = Dico [] 

    insert (Dico []) str = Dico ( [(0, str)] )
    insert (Dico (x:xs)) str = Dico ( (max_dico (fst x) xs, str) : (x:xs) )    

    isIn (Dico []) str = False
    isIn (Dico (x:xs)) str = if (snd x) == str
                        then True
                        else isIn (Dico xs) str



