module Text.ValveVKV.Class where

import Text.ValveVKV.Internal
import Control.Monad (forM)
import Data.Maybe (mapMaybe)
import GHC.Base (NonEmpty)
import Data.List.NonEmpty (NonEmpty, nonEmpty)

findFromName :: ValveKeyValueEntry -> String -> [ValveKeyValueEntry]
findFromName (KVObject (Pair _ stuff)) name =
    mapMaybe finder stuff
    where
        finder :: ValveKeyValueEntry -> Maybe ValveKeyValueEntry
        finder this@(KVObject (Pair thisname s)) = if thisname == name then Just this else Nothing
        finder this@(KVString (Pair thisname s)) = if thisname == name then Just this else Nothing
        finder this@(KVInt (Pair thisname s)) = if thisname == name then Just this else Nothing
findFromName _ _ = []

-- | This operator receives an entry on the left side and a string on the right side. It tries to find the string subentry named the string inside the entry you gave in on the left.
(.:) :: ValveVKV a => ValveKeyValueEntry -> String -> Maybe a 
context .: name =
    let results = findFromName context name in
    case results of
        [] -> Nothing
        x:_ -> fromValveVKV x context
infixl 5 .:

-- | This operator receives an entry on the left side and a string on the right side. It tries to find the subentry named the string inside the entry you gave in on the left.
(^:) :: ValveKeyValueEntry -> String -> Maybe String
context ^: name =
    let results = findFromName context name in
    case results of
        (KVString (Pair _ s)):_ -> Just s
        _ -> Nothing
infixl 5 ^:

-- | A type synonim for ValveKeyValueEntry
type Context = ValveKeyValueEntry


-- | The class that lets a value to be made from a Valve value-keyvalue format.
-- For example, if you have
-- @
-- data My = My {name :: String, count :: Int}
-- @
-- You write your instance as
-- @
-- instance ValveVKV My where
--     fromValveVKV this _ =
--         My \<$\> this ^: "name" \<*\> this .: "count"
-- @
class ValveVKV a where
    -- | The first argument is the entry that should be turned into the type. The second argument is the entry just above that.
    fromValveVKV :: ValveKeyValueEntry -> Context -> Maybe a

instance ValveVKV Int where
    fromValveVKV (KVInt (Pair _ num)) _ = Just num
    fromValveVKV _ _ = Nothing

instance ValveVKV a => ValveVKV (Maybe a) where
    fromValveVKV entry con = Just (fromValveVKV entry con)

instance ValveVKV Bool where
    fromValveVKV (KVInt (Pair _ 0)) _ = Just False
    fromValveVKV (KVInt (Pair _ 1)) _ = Just True
    fromValveVKV _ _ = Nothing

instance ValveVKV a => ValveVKV [a] where
    fromValveVKV (KVString (Pair name _)) context =
        let results = mapMaybe (`fromValveVKV` context) (findFromName context name) in
        case results of
            [] -> Nothing
            _ -> Just results
    fromValveVKV (KVObject (Pair name _)) context =
        let results = mapMaybe (`fromValveVKV` context) (findFromName context name) in
        case results of
            [] -> Nothing
            _ -> Just results
    fromValveVKV (KVInt (Pair name _)) context =
        let results = mapMaybe (`fromValveVKV` context) (findFromName context name) in
        case results of
            [] -> Nothing
            _ -> Just results

instance ValveVKV a => ValveVKV (NonEmpty a) where
    fromValveVKV entry context =
        list >>= nonEmpty
        where
            list :: ValveVKV a => Maybe [a]
            list = fromValveVKV entry context

