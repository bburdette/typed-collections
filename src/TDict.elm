module TDict exposing
    ( TDict
    , empty, clear, insert, update, remove
    , isEmpty, member, get, size
    , keys, values, toList, insertList
    , map, foldl, foldr, filter, partition
    , union, intersect, diff, merge
    )

{-| A dictionary mapping unique keys to values. The keys can be any type that can be converted to an elm 'comparable'.

This is helpful if you have keys that are really just a comparable underneath, but you want to keep them separate using the type system. For instance

    type TagId
        = TagId Int

    type CustomerId
        = CustomerId Int

    emptyTagDict =
        TDict.empty (\(TagId id) -> id) TagId String

    emptyCustomerDict =
        TDict.empty (\(CustomerId id) -> id) CustomerId String

So inserting a (tag,string) into an emptyTagDict works:

    TDict.insert ( TagId 1, "some string of interest" ) emptyTagDict

But inserting a (tag,string) into an emptyCustomerDict doesn't:

    TDict.insert ( TagId 1, "some string of interest" ) emptyCustomerDict


# Dictionaries

@docs TDict


# Build

@docs empty, clear, insert, update, remove


# Query

@docs isEmpty, member, get, size


# Lists

@docs keys, values, toList, insertList


# Transform

@docs map, foldl, foldr, filter, partition


# Combine

@docs union, intersect, diff, merge

-}

import Dict exposing (Dict)


{-| A dictionary of keys and values. Create a TDict with the `empty` function.
-}
type TDict k comparable val
    = TDict
        { keyToComparable : k -> comparable
        , comparableToKey : comparable -> k
        , dict : Dict comparable val
        }


{-| Create an empty dictionary of keys and values. Requires two conversion functions:
one from the key to comparable, and the other from comparable to key.
-}
empty : (k -> comparable) -> (comparable -> k) -> TDict k comparable val
empty k2c c2k =
    TDict
        { keyToComparable = k2c
        , comparableToKey = c2k
        , dict = Dict.empty
        }


{-| Get the value associated with a key. If the key is not found, return
`Nothing`. This is useful when you are not sure if a key will be in the
dictionary.
-}
get : k -> TDict k comparable v -> Maybe v
get key (TDict dict) =
    Dict.get (dict.keyToComparable key) dict.dict


{-| Determine if a key is in a dictionary.
-}
member : k -> TDict k comparable v -> Bool
member key (TDict dict) =
    Dict.member (dict.keyToComparable key) dict.dict


{-| Determine the number of key-value pairs in the dictionary.
-}
size : TDict k comparable v -> Int
size (TDict td) =
    Dict.size td.dict


{-| Determine if a dictionary is empty.
-}
isEmpty : TDict k comparable val -> Bool
isEmpty (TDict dict) =
    Dict.isEmpty dict.dict


{-| Remove all elements from the TDict
-}
clear : TDict k comparable val -> TDict k comparable val
clear (TDict td) =
    TDict
        { td
            | dict = Dict.empty
        }


{-| Insert a key-value pair into a dictionary. Replaces value when there is
a collision.
-}
insert : k -> v -> TDict k comparable v -> TDict k comparable v
insert key val (TDict dict) =
    TDict
        { dict
            | dict =
                Dict.insert (dict.keyToComparable key) val dict.dict
        }


{-| Merge an association list into a dictionary.
-}
insertList : TDict k comparable v -> List ( k, v ) -> TDict k comparable v
insertList (TDict dict) kvs =
    TDict
        { dict
            | dict =
                List.foldl
                    (\( k, v ) td ->
                        Dict.insert (dict.keyToComparable k) v td
                    )
                    dict.dict
                    kvs
        }


{-| Remove a key-value pair from a dictionary. If the key is not found,
no changes are made.
-}
remove : k -> TDict k comparable val -> TDict k comparable val
remove key (TDict dict) =
    TDict
        { dict
            | dict =
                Dict.remove (dict.keyToComparable key) dict.dict
        }


{-| Update the value of a dictionary for a specific key with a given function.
-}
update : k -> (Maybe v -> Maybe v) -> TDict k comparable v -> TDict k comparable v
update k alter (TDict td) =
    TDict { td | dict = Dict.update (td.keyToComparable k) alter td.dict }


{-| Combine two dictionaries. If there is a collision, preference is given
to the first dictionary.
-}
union : TDict k comparable v -> TDict k comparable v -> TDict k comparable v
union (TDict t1) (TDict t2) =
    TDict { t1 | dict = Dict.union t1.dict t2.dict }


{-| Keep a key-value pair when its key appears in the second dictionary.
Preference is given to values in the first dictionary.
-}
intersect : TDict k comparable v -> TDict k comparable v -> TDict k comparable v
intersect (TDict t1) (TDict t2) =
    TDict { t1 | dict = Dict.intersect t1.dict t2.dict }


{-| Keep a key-value pair when its key does not appear in the second dictionary.
-}
diff : TDict k comparable v -> TDict k comparable v -> TDict k comparable v
diff (TDict t1) (TDict t2) =
    TDict { t1 | dict = Dict.diff t1.dict t2.dict }


{-| The most general way of combining two dictionaries. You provide three
accumulators for when a given key appears:

1.  Only in the left dictionary.
2.  In both dictionaries.
3.  Only in the right dictionary.

You then traverse all the keys from lowest to highest, building up whatever
you want.

-}
merge :
    (k -> a -> result -> result)
    -> (k -> a -> b -> result -> result)
    -> (k -> b -> result -> result)
    -> TDict k comparable a
    -> TDict k comparable b
    -> result
    -> result
merge leftStep bothStep rightStep leftDict rightDict initialResult =
    let
        (TDict tdl) =
            leftDict

        (TDict tdr) =
            rightDict
    in
    Dict.merge
        (\c a result -> leftStep (tdl.comparableToKey c) a result)
        (\c a b result -> bothStep (tdl.comparableToKey c) a b result)
        (\c b result -> rightStep (tdl.comparableToKey c) b result)
        tdl.dict
        tdr.dict
        initialResult


{-| Apply a function to all values in a dictionary.
-}
map : (k -> a -> b) -> TDict k comparable a -> TDict k comparable b
map f (TDict td) =
    TDict
        { keyToComparable = td.keyToComparable
        , comparableToKey = td.comparableToKey
        , dict = Dict.map (\c v -> f (td.comparableToKey c) v) td.dict
        }


{-| Fold over the key-value pairs in a dictionary, in order from lowest
key to highest key.
-}
foldl : (k -> v -> b -> b) -> b -> TDict k comparable v -> b
foldl f acc (TDict td) =
    Dict.foldl (\c v a -> f (td.comparableToKey c) v a) acc td.dict


{-| Fold over the key-value pairs in a dictionary, in order from highest
key to lowest key.
-}
foldr : (k -> v -> b -> b) -> b -> TDict k comparable v -> b
foldr f acc (TDict td) =
    Dict.foldr (\c v a -> f (td.comparableToKey c) v a) acc td.dict


{-| Keep a key-value pair when it satisfies a predicate.
-}
filter : (k -> v -> Bool) -> TDict k comparable v -> TDict k comparable v
filter predicate (TDict td) =
    TDict { td | dict = Dict.filter (\c v -> predicate (td.comparableToKey c) v) td.dict }


{-| Partition a dictionary according to a predicate. The first dictionary
contains all key-value pairs which satisfy the predicate, and the second
contains the rest.
-}
partition : (k -> v -> Bool) -> TDict k comparable v -> ( TDict k comparable v, TDict k comparable v )
partition predicate (TDict td) =
    let
        ( l, r ) =
            Dict.partition (\c v -> predicate (td.comparableToKey c) v) td.dict
    in
    ( TDict { td | dict = l }
    , TDict { td | dict = r }
    )


{-| Get all of the keys in a dictionary, sorted from lowest to highest.
-}
keys : TDict k comparable v -> List k
keys (TDict td) =
    List.map td.comparableToKey (Dict.keys td.dict)


{-| Get all of the values in a dictionary, in the order of their keys.
-}
values : TDict k comparable v -> List v
values (TDict td) =
    Dict.values td.dict


{-| Convert a dictionary into an association list of key-value pairs, sorted by keys.
-}
toList : TDict k comparable v -> List ( k, v )
toList td =
    foldr (\k value list -> ( k, value ) :: list) [] td
