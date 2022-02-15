(*Environ*)

exception SymbolNotFound
exception UnknownType
exception Impossible

type 'a env = (string * 'a) list

fun lookupT [] id = raise UnknownType
  | lookupT ((k:string, v)::t) id = if k = id then v else lookupT t id;

fun lookupE [] id = raise Impossible
  | lookupE ((k:string, v)::t) id = if k = id then v else lookupE t id;
