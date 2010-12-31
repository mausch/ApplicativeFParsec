open System
open FParsec.CharParsers
open FParsec.Primitives
open FParsec.Applicative

let readHex a b = 
    char <| Byte.Parse(sprintf "%c%c" a b, System.Globalization.NumberStyles.HexNumber)

let p_hex : Parser<char, unit> = 
    parse { 
        do! skipChar '%' 
        let! a = hex 
        let! b = hex 
        return readHex a b 
    }

let a_hex : Parser<char, unit> = 
    puree (fun skipCharResult hex1result hex2result -> readHex hex1result hex2result) <*> skipChar '%' <*> hex <*> hex

let a_hex2 : Parser<char, unit> = 
    (fun skipCharResult hex1result hex2result -> readHex hex1result hex2result) <!> skipChar '%' <*> hex <*> hex

let a_hex3 : Parser<char, unit> = 
    (fun hex1result hex2result -> readHex hex1result hex2result) <!> skipChar '%' *> hex <*> hex

let a_hex4 : Parser<char, unit> = 
    readHex <!> skipChar '%' *> hex <*> hex 

let a_hex5 : Parser<char, unit> = 
    readHex |> pipe2 (skipChar '%' >>. hex) hex 

match run p_hex "%20" with 
| Success (result,_,_) -> 
    if result = ' ' 
        then printfn "Correctly parsed space" 
        else failwith "Incorrecty parsed %c" result 
| Failure (msg,_,_) -> failwithf "Parsing failed: \n%s" msg