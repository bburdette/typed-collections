module TSet exposing (..)

import Set exposing (Set)


type TSet k comparable
    = TSet
        { keyToComparable : k -> comparable
        , comparableToKey : comparable -> k
        , set : Set comparable
        }


empty : (k -> comparable) -> (comparable -> k) -> TSet k comparable
empty ktc ctk =
    TSet
        { keyToComparable = ktc
        , comparableToKey = ctk
        , set = Set.empty
        }


clear : TSet k comparable -> TSet k comparable
clear (TSet ts) =
    TSet
        { ts
            | set = Set.empty
        }


insert : k -> TSet k comparable -> TSet k comparable
insert key (TSet set) =
    TSet
        { set
            | set =
                Set.insert (set.keyToComparable key) set.set
        }


insertList : TSet k comparable -> List k -> TSet k comparable
insertList ts items =
    List.foldl
        (\k set ->
            insert k set
        )
        ts
        items


remove : k -> TSet k comparable -> TSet k comparable
remove key (TSet set) =
    TSet
        { set
            | set =
                Set.remove (set.keyToComparable key) set.set
        }


intersect : TSet k comparable -> TSet k comparable -> TSet k comparable
intersect (TSet set1) (TSet set2) =
    TSet
        { set1
            | set =
                Set.intersect set1.set set2.set
        }


union : TSet k comparable -> TSet k comparable -> TSet k comparable
union (TSet set1) (TSet set2) =
    TSet
        { set1
            | set =
                Set.union set1.set set2.set
        }


diff : TSet k comparable -> TSet k comparable -> TSet k comparable
diff (TSet set1) (TSet set2) =
    TSet
        { set1
            | set =
                Set.diff set1.set set2.set
        }


member : k -> TSet k comparable -> Bool
member key (TSet set) =
    Set.member (set.keyToComparable key) set.set


isEmpty : TSet k comparable -> Bool
isEmpty (TSet set) =
    Set.isEmpty set.set


toList : TSet k comparable -> List k
toList (TSet set) =
    List.map set.comparableToKey <| Set.toList set.set


foldl : (k -> b -> b) -> b -> TSet k comparable -> b
foldl fn val (TSet ts) =
    Set.foldl (fn << ts.comparableToKey) val ts.set


foldr : (k -> b -> b) -> b -> TSet k comparable -> b
foldr fn val (TSet ts) =
    Set.foldr (fn << ts.comparableToKey) val ts.set


filter : (k -> Bool) -> TSet k comparable -> TSet k comparable
filter fn (TSet ts) =
    TSet { ts | set = Set.filter (fn << ts.comparableToKey) ts.set }
