import System.Environment as SE
import qualified Data.ByteString.Lazy as BS
import Data.Bits
import Data.Word
import Data.Int
import Data.Char (ord)
--import Data.Char8
--import qualified Data.ByteString.Char8

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

-------------------------- Question 2 ----------------------------------------

data Dico = Dico [(Code, String8)] deriving (Show)


 --search the biggest code in Dico and add this with new string give in parameter--

max_dico :: Code -> [(Code, String8)] -> Code
max_dico max [] = max+1
max_dico max (x:xs) = if (fst x) > max
                      then max_dico (fst x) xs
                      else max_dico max xs


split_2 :: Dico -> String8 -> String8 -> Maybe Code -> (String8, Maybe Code, String8)

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



initDico :: String8 -> Dico

initDico (x:[]) = Dico [(0, [x])]
initDico str = insert (initDico (take ((length str) - 1) str)) [car]
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
        decompFile (initDico [0..255]) (args !! 0) (args !! 1)



