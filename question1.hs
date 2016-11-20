type Code = Int
type Test = [(String, Code)]

--class (Eq a) => Table a where
class Table a where
    empty :: a
    insert :: a -> String -> a
    codeOf :: a -> String -> Maybe Code
    stringOf :: a -> Code -> Maybe String
    isIn :: a -> String -> Bool
    split_2 :: a -> String -> String -> Maybe Code -> (String, Maybe Code, String)
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

--translate string into code with table untranslate string as parameter--

lzwEncode :: Table a => a -> String -> [Code]
lzwEncode table [] = []
lzwEncode table str = 
  case split table str of
    (_, Nothing, _) -> []
    (prefix, Just code, (head:tail)) -> 
        code : lzwEncode (insert table (prefix ++ [head])) (head:tail)
    (prefix, Just code, []) -> [code]

--translate code into string with table, oldtrans ans untranslate code as parameter-- 

lzw_Decode :: Table a => a -> String -> [Code] -> String
lzw_Decode table oldtrans [] = []
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

data Dico_lol = Dico [(Code, String)]


 --search the biggest code in Dico and add this with new string give in parameter--

max_dico :: Code -> [(Code, String)] -> Code
max_dico max [] = max+1
max_dico max (x:xs) = if (fst x) > max
                      then max_dico (fst x) xs
                      else max_dico max xs


instance Table Dico_lol where

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

--split :: a -> String -> (String, Maybe Code, String)--

    split_2 (Dico dic) startstr [] lastcode = (startstr, lastcode, [])
    split_2 (Dico dic) startstr (x:xs) lastcode = if codeOf (Dico dic) (startstr ++ [x]) == Nothing
                                                then (startstr, lastcode, xs)
                                                else split_2 (Dico dic) (startstr ++ [x]) xs (codeOf (Dico dic) startstr)

    split (Dico dic) (x:xs) = if codeOf (Dico dic) [x] == Nothing
                                then ([], Nothing, (x:xs))
                                else split_2 (Dico dic) [x] xs (codeOf (Dico dic) [x])









