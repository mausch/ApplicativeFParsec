module FParsec.Applicative

open FParsec.Primitives

/// Sequential application
let inline (<*>) f a = 
    f >>= fun f' -> a >>= fun a' -> preturn (f' a')
    (*
    parse { 
        let! f' = f 
        let! a' = a 
        return f' a' }
    *)

/// A variant of (<*>) with the arguments reversed
let inline (<**>) a f = f <*> a

/// Lifts a value. Equivalent to Haskell's 'pure' for parsers
let inline puree a = preturn a

/// Lift a function. Equivalent to Haskell's 'liftA' or 'fmap' for parsers
let inline lift f a = a |>> f

/// Lift a function. 
let inline liftA x = lift x

/// Lift a function. Equivalent to Haskell's (<$>) for parsers
let inline (<!>) f a = lift f a // puree f <*> a

/// Lift a binary function. Equivalent to Haskell's 'liftA2' for parsers
let inline lift2 f a b = pipe2 a b f  //preturn f <*> a <*> b

/// Lift a binary function. 
let inline liftA2 x = lift2 x

/// Replace all locations in the input with the same value. Equivalent to Haskell's (<$) for parsers
let inline (<!) x y = y >>% x // (lift << (fun a b -> a)) x y

/// Sequence parsers, discarding the value of the first parser.
let inline ( *>) x y = x >>. y // lift2 (fun _ z -> z) x y

/// Sequence parsers, discarding the value of the second parser.
let inline ( <*) x y = x .>> y // lift2 (fun z _ -> z) x y
