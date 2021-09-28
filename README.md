# ValveValueKeyvalue
A Haskell parser for Valve's value-keyvalue format. The main function is "parseValveVKV" in the module Text.ValveVKV. You can see it only parses to types that are part of the typeclass ValveVKV. For example, if you have this type:
` data My = My {name :: String, count :: Int} `
Then the instance looks like this:
```
instance ValveVKV My where
    fromValveVKV this _ =
        My <$> this ^: "name" <*> this .: "count"
```
The first parameter is the entry that should be turned into your type, and the 2nd one is the parent of that entry. The ^: operator receives an entry on the left side and a string on the right side. It tries to find the string subentry named the string inside the entry you gave in on the left. The .: operator is similar, but can return any type, not just string.

The entry type ValveKeyValueEntry has 3 constructors. KVObject, which has a Pair of a KeyValueEntry list. KVInt, which has a Pair of Int and KVString, which has a pair of string. The Pair type itself is one constructor of a string and the type parameter.

So you can now run
```
a :: IO My
a = do
    contents <- readFile "file.txt"
    return $ parseValveVKV contents
```
This will open file "file.txt", read its contents and return the "My" type.