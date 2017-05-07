{-# LANGUAGE OverloadedStrings #-}
module Main where
import Data.Char
import Data.Int
import Control.Monad.State.Strict
import System.Environment
import System.IO
import qualified Text.Parsec as P
import qualified Data.ByteString.Char8 as BS
import qualified Data.Set as S

data Expression
    = AddressOp [Char] [Char]
    | IntLiteral [Char]
    | WordLiteral [Char]
    | CA65Literal [Char]
    | Word [Char]
    | Quote [Expression]
    deriving (Show)

-- Parsing code

p_programFile :: P.Parsec BS.ByteString u [([Char], [Expression])]
p_programFile = p_wordDef `P.sepEndBy` p_spaces1

p_wordDef :: P.Parsec BS.ByteString u ([Char], [Expression])
p_wordDef = (,) <$> (p_word <* p_spaces <* P.char ':' <* p_spaces) <*> p_quote

p_quote :: P.Parsec BS.ByteString u [Expression]
p_quote = P.between (P.char '[') (P.char ']')
                    (p_spaces *> (p_expression `P.sepEndBy` p_spaces1))

p_expression :: P.Parsec BS.ByteString u Expression
p_expression = P.choice
    [ P.try (AddressOp <$> (p_ca65 P.<|> p_addr) <*> (P.char '.' *> P.many1 P.letter))
    , P.try (IntLiteral <$> p_num)
    , WordLiteral <$> (P.char '\'' *> p_addr)
    , CA65Literal <$> p_ca65
    , Word <$> p_word 
    , Quote <$> p_quote ]

p_num :: P.Parsec BS.ByteString u [Char]
p_num = (:) <$> P.option ' ' (P.char '-') <*> P.choice
    [ P.many1 P.digit
    , P.char '$' *> P.many1 P.hexDigit
    , P.char '%' *> P.many1 (P.oneOf "01") ]

p_addr :: P.Parsec BS.ByteString u [Char]
p_addr = ca65expr P.<|> p_num P.<|> p_word 
    where ca65char = P.alphaNum P.<|> P.oneOf "~!@#$%^&*-_+={}|\\\"'<>,./?"
          ca65expr = foldl (\a b -> a ++ "(" ++ b ++ ")") "" <$> 
                     P.between (P.char '(') (P.char ')') 
                     (P.many (P.choice [ P.many1 ca65char, ca65expr, p_spaces1 *> return " "]))

p_ca65 :: P.Parsec BS.ByteString u [Char]
p_ca65 = (\a -> "(" ++ a ++ ")") <$> 
         P.between (P.char '(') (P.char ')') 
         (concat <$> (P.many (P.choice [ P.many1 ca65char, p_ca65, p_spaces1 *> return " "])))
    where ca65char = P.alphaNum P.<|> P.oneOf "~!@#$%^&*-_+={}|\\\"'<>,./?"

p_word :: P.Parsec BS.ByteString u [Char]
p_word = (:) <$> (P.letter P.<|> specialChar) <*> P.many (P.alphaNum P.<|> specialChar)
    where specialChar = P.oneOf "!@#^&*-_+={}|/?<>"

p_spaces :: P.Parsec BS.ByteString u ()
p_spaces = P.skipMany ((P.space *> return ()) P.<|> p_comment)

p_spaces1 :: P.Parsec BS.ByteString u ()
p_spaces1 = P.skipMany1 ((P.space *> return ()) P.<|> p_comment)

p_comment :: P.Parsec BS.ByteString u ()
p_comment = P.char ';' *> P.manyTill P.anyChar P.newline *> return ()

-- Compiling code

type QuotDict = ([([Char], [Expression])], Int)

mangle :: [Char] -> BS.ByteString
mangle = foldl (\a b -> BS.append a (mchr b)) ""
    where mchr c | isAlphaNum c = BS.singleton c 
                 | c == '_' = BS.singleton c 
                 | otherwise = "__" `BS.append` (BS.pack . show . ord) c `BS.append` "__"

compileExpression :: Expression -> State QuotDict BS.ByteString
compileExpression (IntLiteral i) = return $ "    __push " `BS.append` BS.pack i `BS.snoc` '\n'
compileExpression (WordLiteral w) = return $ "    __push " `BS.append` mangle w `BS.snoc` '\n'
compileExpression (CA65Literal w) = return $ "    __push " `BS.append` BS.pack w `BS.snoc` '\n'
compileExpression (Word w) = return $ "    __call sub," `BS.append` mangle w `BS.snoc` '\n'
compileExpression (Quote e) = state $ \(qs, i) ->
    ("    __push __quot" `BS.append` (BS.pack . show) i `BS.snoc` '\n',
     (("__quot" ++ show i, e) : qs, i + 1))
compileExpression (AddressOp a op) = 
    return $ "    __addrOp " 
    `BS.append` BS.pack op `BS.append` "," 
    `BS.append` BS.pack a `BS.snoc` '\n'

compileLastExpression :: Expression -> State QuotDict BS.ByteString
compileLastExpression (Word w) = return $ "    __call tail," `BS.append` mangle w `BS.snoc` '\n'
compileLastExpression e = flip BS.append "    rts\n" <$> compileExpression e

compileWord :: ([Char], [Expression]) -> State QuotDict BS.ByteString
compileWord (w, []) = return $ (BS.pack w `BS.append` ":\n    rts\n")
compileWord (w, es) = do 
    allButLastLine <- mapM compileExpression $ init es
    lastLine <- compileLastExpression $ last es
    return $ (mangle w `BS.append` ":\n") 
             `BS.append` BS.concat allButLastLine
             `BS.append` lastLine

compile :: [([Char], [Expression])] -> Int -> BS.ByteString
compile ws i = case runState (BS.concat <$> mapM compileWord ws) ([], i) of
    (str, ([], _)) -> str
    (str, (qs, i')) -> str `BS.append` compile qs i'

compileFile :: [Char] -> IO ()
compileFile filename = do
    bs <- BS.readFile filename
    let parseResult = P.parse p_programFile filename bs
    case parseResult of
        Left error -> hPutStrLn stderr $ show error
        Right ws -> BS.putStr (compile ws 0)

main :: IO ()
main = getArgs >>= mapM_ compileFile
