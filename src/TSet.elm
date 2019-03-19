module TSet exposing
    ( TSet
    , empty, clear, insert, remove
    , isEmpty, member, size
    , union, intersect, diff
    , toList, insertList
    , mapInto, foldl, foldr, filter, partition
    )

{-| A set of unique values. The elements can be any type that can be converted to an elm 'comparable'.

This is helpful if you have values that are really just a comparable underneath, but you want to keep them separate using the type system. For instance:

    type Kilos
        = Kilos Float

    type Pounds
        = Pounds Float

Create a TSet with the `empty` function, which takes two converter functions as arguments.
It can be convenient to create a canonical empty TSet for a certain type:

    emptyKiloSet =
        TSet.empty
            (\(Kilos n) -> n)
            Kilos

Then to do the equivalent of Set.fromList:

    TSet.insertList emptyKiloSet mykilolist

Insert, remove, and query operations all take _O(log n)_ time, just like Set.


# Sets

@docs TSet


# Build

@docs empty, clear, insert, remove


# Query

@docs isEmpty, member, size


# Combine

@docs union, intersect, diff


# Lists

@docs toList, insertList


# Transform

@docs mapInto, foldl, foldr, filter, partition

-}

import Set exposing (Set)


{-| Represents a set of unique values. Create with the `empty` function.
-}
type TSet k comparable
    = TSet
        { keyToComparable : k -> comparable
        , comparableToKey : comparable -> k
        , set : Set comparable
        }


{-| Create an empty TSet. Requires two conversion functions:
one from the key to comparable, and the other from comparable to key.
-}
empty : (k -> comparable) -> (comparable -> k) -> TSet k comparable
empty ktc ctk =
    TSet
        { keyToComparable = ktc
        , comparableToKey = ctk
        , set = Set.empty
        }


{-| remove all values from the set.
-}
clear : TSet k comparable -> TSet k comparable
clear (TSet ts) =
    TSet
        { ts
            | set = Set.empty
        }


{-| Insert a value into a set.
-}
insert : k -> TSet k comparable -> TSet k comparable
insert key (TSet set) =
    TSet
        { set
            | set =
                Set.insert (set.keyToComparable key) set.set
        }


{-| Insert a list of values into a set.
-}
insertList : TSet k comparable -> List k -> TSet k comparable
insertList ts items =
    List.foldl
        (\k tset ->
            insert k tset
        )
        ts
        items


{-| Remove a value from a set. If the value is not found, no changes are made.
-}
remove : k -> TSet k comparable -> TSet k comparable
remove key (TSet set) =
    TSet
        { set
            | set =
                Set.remove (set.keyToComparable key) set.set
        }


{-| Get the intersection of two sets. Keeps values that appear in both sets.
-}
intersect : TSet k comparable -> TSet k comparable -> TSet k comparable
intersect (TSet set1) (TSet set2) =
    TSet
        { set1
            | set =
                Set.intersect set1.set set2.set
        }


{-| Get the union of two sets. Keep all values.
-}
union : TSet k comparable -> TSet k comparable -> TSet k comparable
union (TSet set1) (TSet set2) =
    TSet
        { set1
            | set =
                Set.union set1.set set2.set
        }


{-| Get the difference between the first set and the second. Keeps values
that do not appear in the second set.
-}
diff : TSet k comparable -> TSet k comparable -> TSet k comparable
diff (TSet set1) (TSet set2) =
    TSet
        { set1
            | set =
                Set.diff set1.set set2.set
        }


{-| Determine if a value is in a set.
-}
member : k -> TSet k comparable -> Bool
member key (TSet set) =
    Set.member (set.keyToComparable key) set.set


{-| Determine if a set is empty.
-}
isEmpty : TSet k comparable -> Bool
isEmpty (TSet set) =
    Set.isEmpty set.set


{-| Determine the number of elements in a set.
-}
size : TSet k comparable -> Int
size (TSet ts) =
    Set.size ts.set


{-| Convert a set into a list, sorted from lowest to highest.
-}
toList : TSet k comparable -> List k
toList (TSet set) =
    List.map set.comparableToKey <| Set.toList set.set


{-| Fold over the values in a set, in order from lowest to highest.
-}
foldl : (k -> b -> b) -> b -> TSet k comparable -> b
foldl fn val (TSet ts) =
    Set.foldl (fn << ts.comparableToKey) val ts.set


{-| Fold over the values in a set, in order from highest to lowest.
-}
foldr : (k -> b -> b) -> b -> TSet k comparable -> b
foldr fn val (TSet ts) =
    Set.foldr (fn << ts.comparableToKey) val ts.set


{-| Map a function onto a set, inserting into a second set.
-}
mapInto : (k -> k2) -> TSet k comparable -> TSet k2 comparable2 -> TSet k2 comparable2
mapInto f s s2 =
    insertList s2 (List.map f (toList s))


{-| Create a new set consisting only of elements which satisfy a predicate.
-}
filter : (k -> Bool) -> TSet k comparable -> TSet k comparable
filter fn (TSet ts) =
    TSet { ts | set = Set.filter (fn << ts.comparableToKey) ts.set }


{-| Create two new sets; the first consisting of elements which satisfy a
predicate, the second consisting of elements which do not.
-}
partition : (k -> Bool) -> TSet k comparable -> ( TSet k comparable, TSet k comparable )
partition p (TSet ts) =
    let
        ( p1, p2 ) =
            Set.partition (ts.comparableToKey >> p) ts.set
    in
    ( TSet { ts | set = p1 }
    , TSet { ts | set = p2 }
    )
