module TDict exposing (..)

import Dict exposing (Dict)


type TDict k comparable val
    = TDict
        { keyToComparable : k -> comparable
        , dict : Dict comparable val
        }


empty : (k -> comparable) -> TDict k comparable val
empty kfn =
    TDict
        { keyToComparable = kfn
        , dict = Dict.empty
        }


clear : TDict k comparable val -> TDict k comparable val
clear (TDict td) =
    TDict
        { td
            | dict = Dict.empty
        }


insertList : List ( k, v ) -> TDict k comparable v -> TDict k comparable v
insertList kvs (TDict dict) =
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


insert : k -> v -> TDict k comparable v -> TDict k comparable v
insert key val (TDict dict) =
    TDict
        { dict
            | dict =
                Dict.insert (dict.keyToComparable key) val dict.dict
        }


remove : k -> TDict k comparable val -> TDict k comparable val
remove key (TDict dict) =
    TDict
        { dict
            | dict =
                Dict.remove (dict.keyToComparable key) dict.dict
        }


get : k -> TDict k comparable v -> Maybe v
get key (TDict dict) =
    Dict.get (dict.keyToComparable key) dict.dict


member : k -> TDict k comparable v -> Bool
member key (TDict dict) =
    Dict.member (dict.keyToComparable key) dict.dict


isEmpty : TDict k comparable val -> Bool
isEmpty (TDict dict) =
    Dict.isEmpty dict.dict
