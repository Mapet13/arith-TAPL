import           Control.Applicative (Alternative (..))
import           Data.Bifunctor      (first)
import           Data.Maybe          (catMaybes)

data Term
  = TrueTerm
  | FalseTerm
  | IfTerm Term Term Term
  | ZeroTerm
  | SuccTerm Term
  | PredTerm Term
  | IsZeroTerm Term
  deriving (Show)

data Token
  = TrueToken
  | FalseToken
  | IfToken
  | ThenToken
  | ElseToken
  | ZeroToken
  | SuccToken
  | PrevToken
  | IsZeroToken

isNumericValue :: Term -> Bool
isNumericValue ZeroTerm     = True
isNumericValue (SuccTerm t) = isNumericValue t
isNumericValue _            = False

isValue :: Term -> Bool
isValue TrueTerm  = True
isValue FalseTerm = True
isValue t         = isNumericValue t

boolToTerm :: Bool -> Term
boolToTerm True  = TrueTerm
boolToTerm False = FalseTerm

singleEval :: Term -> Maybe Term
singleEval (IfTerm TrueTerm t2 _)     = Just t2
singleEval (IfTerm FalseTerm _ t3)    = Just t3
singleEval (IfTerm t1 t2 t3)          = singleEval t1 >>= \t -> singleEval (IfTerm t t2 t3)
singleEval (SuccTerm t)               = singleEval t >>= Just . SuccTerm
singleEval (PredTerm (SuccTerm t))    = if isNumericValue t then Just t else Nothing
singleEval (PredTerm ZeroTerm)        = Just ZeroTerm
singleEval (PredTerm t)               = singleEval t >>= Just . PredTerm
singleEval (IsZeroTerm ZeroTerm)      = Just TrueTerm
singleEval (IsZeroTerm (SuccTerm t))  = if isNumericValue t then Just FalseTerm else Nothing
singleEval (IsZeroTerm t)             = singleEval t >>= Just . IsZeroTerm
singleEval _                          = Nothing

eval :: Term -> Maybe Term
eval t = singleEval t >>= \x -> if isValue x then Just x else eval x

lexToken :: String -> Maybe Token
lexToken "if"     = Just IfToken
lexToken "then"   = Just ThenToken
lexToken "else"   = Just ElseToken
lexToken "true"   = Just TrueToken
lexToken "false"  = Just FalseToken
lexToken "0"      = Just ZeroToken
lexToken "succ"   = Just SuccToken
lexToken "prev"   = Just PrevToken
lexToken "iszero" = Just IsZeroToken
lexToken _        = Nothing

lexer :: String -> Maybe [Token]
lexer = mapM lexToken . words

parseZero :: Token -> [Token] -> Maybe (Term, [Token])
parseZero ZeroToken rest = Just (ZeroTerm, rest)
parseZero _ _            = Nothing

parseTrue :: Token -> [Token] -> Maybe (Term, [Token])
parseTrue TrueToken rest = Just (TrueTerm, rest)
parseTrue _ _            = Nothing

parseFalse :: Token -> [Token] -> Maybe (Term, [Token])
parseFalse FalseToken rest = Just (FalseTerm, rest)
parseFalse _ _             = Nothing

parseSucc :: Token -> [Token] -> Maybe (Term, [Token])
parseSucc SuccToken rest = first SuccTerm <$> parseTokens rest
parseSucc _ _            = Nothing

parsePrev :: Token -> [Token] -> Maybe (Term, [Token])
parsePrev PrevToken rest = first PredTerm <$> parseTokens rest
parsePrev _ _            = Nothing

parseIsZero :: Token -> [Token] -> Maybe (Term, [Token])
parseIsZero IsZeroToken rest = first IsZeroTerm <$> parseTokens rest
parseIsZero _ _              = Nothing

parseIf :: Token -> [Token] -> Maybe (Term, [Token])
parseIf IfToken ts = do
    (t1, ts1) <- parseTokens ts
    (t2, ts2) <- tryParseThen ts1
    (t3, ts3) <- tryParseElse ts2
    return (IfTerm t1 t2 t3, ts3)
  where
    tryParseThen (ThenToken : ts) = parseTokens ts
    tryParseThen _                = Nothing
    tryParseElse (ElseToken : ts) = parseTokens ts
    tryParseElse _                = Nothing
parseIf _ _ = Nothing

parseTokens :: [Token] -> Maybe (Term, [Token])
parseTokens (t:ts) = parseZero t ts
    <|> parseTrue t ts
    <|> parseFalse t ts
    <|> parseSucc t ts
    <|> parsePrev t ts
    <|> parseIsZero t ts
    <|> parseIf t ts
parseTokens [] = Nothing

testInput :: String
testInput = "if iszero prev succ succ 0 then succ succ prev succ 0 else prev prev 0"

main :: IO ()
main = case lexer testInput >>= parseTokens >>= eval . fst of
    Just t  -> print t
    Nothing -> putStrLn "Parse error"
