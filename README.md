# Typed Containers

Simple layers over the standard Elm containers (Dict and Set), that allow 
any type of key that can be converted to comparable.  

This gives you a little extra type safety, at the cost of a little extra 
hassle - you must provide conversion functions to create a new container.

For example, if you have types that are really just comparables underneath, 
but you want to keep the segregated for safety:

```elm

type Kilos =
    Kilos Float
type Pounds =
    Pounds Float

-- Create a TSet with the empty function, which takes two converter functions as arguments.
-- It can be convenient to create a canonical empty TSet for a certain type:

emptyKiloSet = TSet.empty ((Kilos n) -> n) Kilos

mykilolist = [Kilo 1.0, Kilo 2.0]

-- Then to do the equivalent of fromList:
TSet.insertList emptyKiloSet mykilolist

-- but you can't put the kilolist into a pounds Set. 
emptyPoundSet = TSet.empty ((Pounds n) -> n) Pounds
TSet.insertList emptyPoundSet mykilolist

```
