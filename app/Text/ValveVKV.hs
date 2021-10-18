module Text.ValveVKV(parseValveVKV, parseToVKV, fromValveVKV, (.:), (^:), ValveVKV,
    ValveKeyValueEntry(KVObject, KVInt, KVString), Pair (Pair), unpair, Context, vkvParser) where
-- Library for processing Valve's value keyvalue format. The main function you will wish to use is parseValveVKV. To convert it into your own type, you
-- will need to write a 'ValveVKV' instance for it.

import Text.Parsec
import Text.ValveVKV.Internal
import Text.ValveVKV.Class
import Data.Maybe (mapMaybe)

-- | The main function you will be using. Turns the ValveVKV string into a type that has the 'ValveVKV' typeclass.
parseValveVKV :: ValveVKV a => String -> Either String a
parseValveVKV input =
    let parseRes = parse vkvParser "" input in
    case parseRes of
        Left s -> Left (show s)
        Right a ->
            let topObj = KVObject (Pair "top" a) in
            fromValveVKV topObj topObj

-- | Parses it directly to a list of entries. Most of the times, 'parseValveVKV' will be better to directly turn it into a Haskell type of your choice
--
-- @since 1.0.1.0
parseToVKV :: String -> Either ParseError [ValveKeyValueEntry]
parseToVKV = parse vkvParser ""
