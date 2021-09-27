module Text.ValveVKV.Internal where
import Text.Parsec
import Text.Read (readMaybe)

alphabet :: [Char]
alphabet = "'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_-+=.,#@!$%¨&*\\"

alphabetSpace :: [Char]
alphabetSpace = ' ':alphabet

data ValveKeyValueEntry
   = KVObject (Pair [ValveKeyValueEntry])
   | KVString (Pair String)
   | KVInt (Pair Int)
   deriving (Show)

liftVKV :: (String -> String) -> ValveKeyValueEntry -> ValveKeyValueEntry
liftVKV f (KVObject (Pair name s)) = KVObject (Pair (f name) s)
liftVKV f (KVInt (Pair name s)) = KVInt (Pair (f name) s)
liftVKV f (KVString (Pair name s)) = KVString (Pair (f name) s)


data Pair a = Pair String a
    deriving (Show)

unpair :: Pair a -> (String, a)
unpair (Pair str a) = (str,a)

entryParser :: Parsec String () ValveKeyValueEntry
entryParser =
    try objParse <|> try kvIntParse <|> try kvStringParse

whitespaceCommon :: Parsec String () Char
whitespaceCommon = choice $ fmap char "\t "

nameGrab :: Parsec String () String
nameGrab = many1 $ choice $ fmap char alphabet

nameGrabSpace :: Parsec String () String
nameGrabSpace = many1 $ choice $ fmap char alphabetSpace

objParse :: Parsec String () ValveKeyValueEntry
objParse = do
    many extendWhitespace
    many commentParser
    name <- nameGrab
    many (try extendWhitespace)
    char '{'
    many (try extendWhitespace)
    things <- vkvParser
    many (try extendWhitespace)
    char '}'
    many (try commentParser)
    many (try extendWhitespace)
    return $ KVObject $ Pair name things

extendWhitespace :: Parsec String () Char
extendWhitespace = choice $ try commentParser:fmap char "\t\n " :: Parsec String () Char

commentParser :: Parsec String () Char
commentParser = do
    many whitespaceCommon
    char '/'
    char '/'
    anyLoop
    where
        anyLoop :: Parsec String () Char
        anyLoop = do
            thisChar <- anyChar
            if thisChar == '\n' then
                return '\n'
            else do
                anyLoop
                return thisChar

kvStringParse :: Parsec String () ValveKeyValueEntry
kvStringParse = do
    many extendWhitespace
    many commentParser
    let stringGrab = many1 $ choice $ fmap char alphabet :: Parsec String () String
    let names = choice [nameGrab, between (char '"') (char '"') nameGrabSpace]
    many whitespaceCommon
    name <- names
    many1 whitespaceCommon
    value <- try (between (char '"') (char '"') nameGrabSpace) <|> try stringGrab
    many (try extendWhitespace)
    return $ KVString $ Pair name value

kvIntParse :: Parsec String () ValveKeyValueEntry
kvIntParse = do
    many extendWhitespace
    many commentParser
    let numberGrab = many1 $ choice $ fmap char "0123456789" :: Parsec String () String
    many (try whitespaceCommon)
    name <- nameGrab
    many1 whitespaceCommon
    valueRaw <- numberGrab
    many (try extendWhitespace)
    let value = readMaybe valueRaw :: Maybe Int
    case value of
        Just x -> return $ KVInt $ Pair name x
        Nothing -> fail ""

-- | The Parsec parser itself.
vkvParser :: Parsec String () [ValveKeyValueEntry]
vkvParser =
    many1 entryParser
