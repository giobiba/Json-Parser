{-# LANGUAGE DeriveGeneric, LambdaCase #-}
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

-- message from error branch
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

stringify :: JValue -> Gen String
stringify = pad . go
  where
    surround l r j = l ++ j ++ r
    pad gen = surround <$> jsonWhitespaceGen <*> jsonWhitespaceGen <*> gen
    commaSeparated = pad . pure . intercalate ","

    go value = case value of
      JArray elements ->
        mapM (pad . stringify) elements
          >>= fmap (surround "[" "]") . commaSeparated
      JObject kvs ->
        mapM stringifyKV kvs >>= fmap (surround "{" "}") . commaSeparated
      _           -> return $ show value

    stringifyKV (k, v) =
      surround <$> pad (pure $ showJSONString k) <*> stringify v <*> pure ":"

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
-- char :: Char -> Parser String Char
-- char c = Parse $ \case
--    (x:xs) | x == c -> Just (xs, x)
--    _               -> Nothing

-- a generalization of the first version
satisfy :: (a -> Bool) -> Parser [a] a
satisfy predicate = Parser $ \case
    (x:xs) | predicate x  -> Just (xs, x)
    _                     -> Nothing

char :: Char -> Parser String Char
char c  = satisfy (== c)

digitParse :: Parser String Int
digitParse = digitToInt <$> satisfy isDigit
--         = Parser $ \i -> fmap digitToInt <$> runParser (satisfy isDigit) i
--           old

stringParse :: String -> Parser String String
stringParse ""     = pure ""
stringParse (c:cs) = (:) <$> char c <*> stringParse cs

jNull :: Parser String JValue
jNull = stringParse "null" $> JNull

jBool :: Parser String JValue
jBool = stringParse "true" $> JBool True
     <|> stringParse "false" $> JBool False

jsonchar :: Parser String Char
jsonchar =   stringParse "\\\"" $> '"'
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
                      <$> (stringParse "\\u" *> replicateM 4 hexDigit)  -- for a given string "*>" discards the "\\u" part and runs the parser 4 times
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

jString :: Parser String JValue
jString = JString <$> (char '"' *> jString') -- we test for the
    where
        jString' = do
            optFirst <- optional jsonchar -- we get the first character, using optional because it may not exist
            case optFirst of
                Nothing -> "" <$ char '"' -- if there is no first character the input is empty
                Just first | not $ isSurroagate first ->
                    (first:) <$> jString' -- if the first character is not a surrogate then we treat it as a normal character and call jString' recursively
                Just first -> do
                    second <- jsonchar -- otherwise we parse the second character
                    if isHighSurrogate first && isLowSurrogate second -- if there is a valid surrogate pair
                    then (combineSurrogates first second:) <$> jString' -- we combine the surrogates and continue reccursively
                    else empty -- otherwise we fail because the surrogate pair was invalid


prop_genParseJString :: Property
prop_genParseJString =
    forAllShrink jStringGen shrink $ \js ->
        case runParser jString (show js) of
            Nothing     -> False
            Just (_,o)  -> o == js


-- JNumber parser

jUInt :: Parser String Integer
jUInt = (\d ds -> digitsToNumber 10 0 (d:ds) ) <$> digit19 <*> digits
        <|> fromIntegral <$> digitParse

-- digit parser excluding 0
digit19 :: Parser String Int
digit19 = digitToInt <$> satisfy (\x -> isDigit x && x /= '0')

digits :: Parser String [Int]
digits = some digitParse

jInt :: Parser String Integer
jInt = signInt <$> optional (char '-') <*> jUInt

signInt :: Maybe Char -> Integer -> Integer
signInt (Just '-') i = negate i
signInt _          i = i

jFrac :: Parser String [Int]
jFrac = char '.' *> digits

jExp :: Parser String Integer
jExp = (char 'e' <|> char 'E')
    *> (signInt <$> optional (char '+' <|> char '-') <*> jUInt)


-- solution provided by the site

-- jInt :: Parser String JValue
-- jInt = JNumber <$> jInt' <*> pure [] <*> pure 0
--
-- jIntExp :: Parser String JValue
-- jIntExp = JNumber <$> jInt' <*> pure [] <*> jExp
--
-- jIntFrac :: Parser String JValue
-- jIntFrac = JNumber <$> jInt' <*> jFrac <*> pure 0
--
-- jIntFracExp :: Parser String JValue
-- jIntFracExp = JNumber <$> jInt' <*> jFrac <*> jExp
--
-- jNumber :: Parser String JValue
-- jNumber = jIntFracExp <|> jIntExp <|> jIntFrac <|> jInt

-- a more clean solution written by me
jNumber :: Parser String JValue
jNumber = JNumber <$> jInt
                  <*> (jFrac <|> pure [])
                  <*> (jExp <|> pure 0)

prop_genParseJNumber :: Property
prop_genParseJNumber =
    forAllShrink jNumberGen shrink $ \jn ->
        case runParser jNumber (show jn) of
            Nothing    -> False
            Just (_,o) -> o == jn

-- JArray Parser
surroundedBy :: Parser String a -> Parser String b -> Parser String a
surroundedBy p1 p2 = p2 *> p1 <* p2

-- a way to see if there are separators between every value
separatedBy :: Parser i v -> Parser i s -> Parser i [v]
separatedBy v s = (:) <$> v <*> many (s *> v)
               <|> pure []

spaces :: Parser String String
spaces = many (char ' ' <|> char '\n' <|> char '\r' <|> char '\t')

jArray :: Parser String JValue
jArray = JArray <$>
    (char '[' *>
        (jValue `separatedBy` char ',' `surroundedBy` spaces)
    <* char ']')

jObject :: Parser String JValue
jObject = JObject <$>
    (char '{' *> pair `separatedBy` char ',' `surroundedBy` spaces <* char '}')
    where
        pair = (\(JString k) v -> (k, v))
           <$> (jString `surroundedBy` spaces)
           <* char ':'
           <*> jValue

-- properties for jArray and jObject

prop_genParseJArray :: Property
prop_genParseJArray =
    forAllShrink (sized jArrayGen) shrink $ \ja -> do
        jas <- dropWhile isSpace <$> stringify ja
        return . counterexample (show jas) $ case runParser jArray jas of
            Nothing     -> False
            Just (_, o) -> o == ja

prop_genParseJObject :: Property
prop_genParseJObject =
    forAllShrink (sized jObjectGen) shrink $ \ja -> do
        jas <- dropWhile isSpace <$> stringify ja
        return . counterexample (show jas) $ case runParser jObject jas of
            Nothing     -> False
            Just (_, o) -> o == ja


jValue :: Parser String JValue
jValue = jValue' `surroundedBy` spaces
  where
    jValue' =  jNull
           <|> jBool
           <|> jString
           <|> jNumber
           <|> jArray
           <|> jObject

parseJSON :: String -> Maybe JValue
parseJSON s = case runParser jValue s of
    Just ("", json) -> Just json
    _               -> Nothing

prop_genParseJSON :: Property
prop_genParseJSON = forAllShrink (sized jValueGen) shrink $ \value -> do
  json <- stringify value
  return . counterexample (show json) . (== Just value) . parseJSON $ json

runTests :: IO ()
runTests = do
    putStrLn "~~ verifying JString ~~"
    quickCheck prop_genParseJString

    putStrLn "~~ verifying JNumber ~~"
    quickCheck prop_genParseJNumber

    putStrLn "~~ verifying JArray ~~"
    quickCheck prop_genParseJArray

    putStrLn "~~ verifying JObject ~~"
    quickCheck prop_genParseJObject

    putStrLn "~~ verifying JSON ~~"
    quickCheck prop_genParseJSON
