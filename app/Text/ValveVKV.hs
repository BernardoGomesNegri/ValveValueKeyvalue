module Text.ValveVKV(vkvParser, parseValveVKV, ValveKeyValueEntry, Pair, fromValveVKV, (.:), (^:), unpair) where
-- Library for processing Valve's value keyvalue format. The main function you will wish to use is parseValveVKV. To convert it into your own type, you
-- will need to write a 'ValveVKV' instance for it.

import Text.Parsec
import Text.ValveVKV.Internal
import Text.ValveVKV.Class
import Data.Maybe (mapMaybe)


parseValveVKV :: ValveVKV a => String -> Maybe a
parseValveVKV input =
    let parseRes = parse vkvParser "" input in
    case parseRes of
        Left _ -> Nothing
        Right a ->
            let topObj = KVObject (Pair "top" a) in
            fromValveVKV topObj topObj
