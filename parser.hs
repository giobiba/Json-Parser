{-# LANGUAGE DeriveGeneric, TupleSections, LambdaCase #-}
module JSONParser where

import Control.Applicative (Alternative(..), optional)
import Control.Monad (replicateM)
import Data.Bits (shiftL)
import Data.Char (isDigit, isHexDigit, isSpace, chr, ord, digitToInt)
import Data.Functor(($>))
import Data.List (intercalate)
import GHC.Generics (Generic)
import Numeric (showHex)
import Test.QuickCheck hiding (Positive, Negative)

data JValue = JNull
            | JBool Bool
            | JString String
            | JNumber { int :: Integer, frac :: [Int], exponent :: Integer }
            | JArray [JValue]
            | JObject [(String, JValue)]
            deriving (Eq, Generic)

-- instance of show for the JSON file

instance Show JValue where
    show value = case value of
        JNull           -> "null"
        JBool True      -> "true"
        JBool False     -> "false"
        JString s       -> showJSONString s
        JNumber s [] 0  -> show s
        JNumber s f  0  -> show s ++ "." ++ concatMap show f
        JNumber s [] e  -> show s ++ "e" ++ show e
        JNumber s f  e  -> show s ++ "." ++ concatMap show f ++ "e" ++ show e
        JArray a        -> "[" ++ intercalate ", " (show <$> a) ++ "]"
        JObject o       -> "{" ++ intercalate ", " (showPair <$> o) ++ "}"
        where
            showPair (k, v) = showJSONString k ++ ": " ++ show v

showJSONString :: String -> String
showJSONString s = "\"" ++ concatMap showJSONChar s ++ "\""

isControl :: Char -> Bool
isControl c = c `elem` ['\0'..'\31']

showJSONChar :: Char -> String
showJSONChar c = case c of
        '\'' -> "'"
        '\"' -> "\\\""
        '\\' -> "\\\\"
        '/'  -> "\\/"
        '\b' -> "\\b"
        '\f' -> "\\f"
        '\n' -> "\\n"
        '\r' -> "\\r"
        '\t' -> "\\t"
        _ | isControl c -> "\\u" ++ showJSONNonASCIIChar c
        _ -> [c]
        where
            showJSONNonASCIIChar c =
                let a = "0000" ++ showHex (ord c) "" in drop (length a - 4) a

-- creating generators for the JSON

jNullGen :: Gen JValue
jNullGen = pure JNull

jBoolGen :: Gen JValue
jBoolGen = JBool <$> arbitrary

jNumberGen :: Gen JValue
jNumberGen = JNumber <$> arbitrary <*> listOf (choose (0, 9)) <*> arbitrary


-- for the string generator we need to include both unescaped and escaped characters
stringGen :: Gen String
stringGen =
    concat <$> listOf (oneof [vectorOf 1 arbitraryUnicodeChar
                              , escapedUnicodeChar])
    where
        escapedUnicodeChar = ("\\u" ++) <$> vectorOf 4 (elements hexDigitLetters)
        hexDigitLetters    = ['0'..'9'] ++ ['a'..'f'] ++ ['A'..'F']

jStringGen :: Gen JValue
jStringGen = JString <$> stringGen

-- generating the complex types
jArrayGen :: Int -> Gen JValue
jArrayGen = fmap JArray . scale (`div` 2) . listOf . jValueGen . (`div` 2)

jObjectGen :: Int -> Gen JValue
jObjectGen = fmap JObject . scale (`div` 2) . listOf . objKV . (`div` 2)
    where
        objKV n = (,) <$> stringGen <*> jValueGen n

jValueGen :: Int -> Gen JValue
jValueGen n =
    frequency [(4, frequency scalarGens), (1, oneof (compositeGens n))]
    where
        scalarGens      = [(1,jNullGen), (1,jBoolGen), (2, jNumberGen), (2, jStringGen)]
        compositeGens n =  [jArrayGen n, jObjectGen n]

instance Arbitrary JValue where
    arbitrary = sized jValueGen
    shrink    = genericShrink

-- not yet implemented
jsonWhitespaceGen :: Gen String
jsonWhitespaceGen =
    scale (round . sqrt . fromIntegral)
    . listOf
    . elements
    $ [' ' , '\n' , '\r' , '\t']

-- parser

newtype Parser i o = Parser  {runParser :: i -> Maybe (i, o)}

instance Functor (Parser i) where
    fmap f (Parser p) = Parser (fmap (fmap f) . p)

instance Applicative (Parser i) where
    pure x    = Parser $ \input -> pure (input, x)
    (Parser pf) <*> (Parser po) = Parser $ \input -> case pf input of
        Nothing        -> Nothing
        Just (rest, f) -> fmap f <$> po rest

instance Alternative (Parser i) where
    empty = Parser $ const empty
    (Parser p1) <|> (Parser p2) = Parser $ \input -> p1 input <|> p2 input

instance Monad (Parser i) where
    return = pure
    p >>= f = Parser $ \input -> case runParser p input of
        Nothing        -> Nothing
        Just (rest, o) -> runParser (f o) rest

-- first version
-- charParse :: Char -> Parser String Char
-- charParse c = Parse $ \case
--    (x:xs) | x == c -> Just (xs, x)
--    _               -> Nothing

-- a generalization of the first version
satisfy :: (a -> Bool) -> Parser [a] a
satisfy predicate = Parser $ \case
    (x:xs) | predicate x  -> Just (xs, x)
    _                     -> Nothing

charParse :: Char -> Parser String Char
charParse c  = satisfy (== c)

digitParse :: Parser String Int
digitParse = digitToInt <$> satisfy isDigit
--         = Parser $ \i -> fmap digitToInt <$> runParser (satisfy isDigit) i
--           old

stringParse :: String -> Parser String String
stringParse ""     = pure ""
stringParse (c:cs) = (:) <$> charParse c <*> stringParse cs

jNullParse :: Parser String JValue
jNullParse = stringParse "null" $> JNull

jBoolParse :: Parser String JValue
jBoolParse = stringParse "true" $> JBool True
         <|> stringParse "false" $> JBool False

jsonCharParse :: Parser String Char
jsonCharParse =   stringParse "\\\"" $> '"'
        <|> stringParse "\\\\" $> '\\'
        <|> stringParse "\\/"  $> '/'
        <|> stringParse "\\b"  $> '\b'
        <|> stringParse "\\f"  $> '\f'
        <|> stringParse "\\n"  $> '\n'
        <|> stringParse "\\r"  $> '\r'
        <|> stringParse "\\t"  $> '\t'
        <|> unicodeChar
        <|> satisfy (\c -> not (c == '\"' || c == '\\' || isControl c))
    where
        unicodeChar = chr . fromInteger . digitsToNumber 16 0
                      <$> (string "\\u" *> replicateM 4 hexDigit)  -- for a given string "*>" discards the "\\u" part and runs the parser 4 times
        hexDigit = digitToInt <$> satisfy Data.Char.isHexDigit

digitsToNumber :: Int -> Integer -> [Int] -> Integer
digitsToNumber base = foldl (\num acc -> num * fromIntegral base + fromIntegral acc)

isHighSurrogate :: Char -> Bool
isHighSurrogate c = ord c >= 0xDB00 && ord c <= 0xDBFF

isLowSurrogate :: Char -> Bool
isLowSurrogate c = ord c >= 0xDC00 && ord c <= 0XDFFF

isSurroagate :: Char -> Bool
isSurroagate c = isLowSurrogate c || isHighSurrogate c

combineSurrogates :: Char -> Char -> Char
combineSurrogates a b = chr $
    ((ord a - 0xD800) `shiftL` 10) + (ord b - 0xDC00) + 0x10000

jStringParse :: Parser String JValue
jStringParse = JString <$> (charParse '"' *> jString') -- we test for the
    where
        jString' = do
            optFirst <- optional jsonCharParse
            case optFirst of
                Nothing -> "" <$ charParse '"'
                Just first | not $ isSurroagate first ->
                    (first:) <$> jString'
                Just first -> do
                    second <- jsonCharParse
                    if isHighSurrogate first && isLowSurrogate second
                    then (combineSurrogates first second:) <$> jString'
                    else empty
