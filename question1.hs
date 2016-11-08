type Code = Int
type Test = [(String, Code)]

class Table a where
	empty :: a
   	insert :: a -> String -> a
	codeOf :: a -> String -> Maybe String
	stringOf :: a -> Code -> Maybe Code
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



lzwEncode :: Table a => a -> String -> [Code]
lzwEncode table [] = []
lzwEncode table str = 
  case split table str of
    (_, Nothing, _) -> []
    (prefix, Just code, (head:tail)) -> 
        code : lzwEncode (insert table (prefix ++ [head])) (head:tail)
    (prefix, Just code, []) -> [code]
