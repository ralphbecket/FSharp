module Parserfal

open System

// A position in a source file.
type Idx = int

// A parsing result.
type Result<'t> =
    | Pass of Value: 't * Idx: Idx      // A successful match.
    | Fail                              // A failed match.
    | Error of What: string * Idx: Idx  // A genuine syntax error.

// A source file path, its contents, and a parser for skipping whiteSpace.
type Ctxt = { Path: string; Text: string; SkipWhiteSpace: string -> Idx -> Idx }

// A parser.
type Parser<'t> = Ctxt -> Idx -> Result<'t>

// Are we at the end of file?
let eof (cx: Ctxt) ix = cx.Text.Length <= ix

// Basic string ops.
let rec skipWhileCh (cond: char -> bool) (t: string) ix =
    if t.Length <= ix || not (cond t.[ix]) then ix else skipWhileCh cond t (ix + 1)

let atStr (s: string) (t: string) ix =
    let ns = s.Length
    let nt = t.Length
    let rec atStr' i =
        if i = ns then true
        else if nt <= ix + i || t.[ix + i] <> s.[i] then false
        else atStr' (i + 1)
    atStr' 0

let atCh (c: char) (t: string) ix =
    (ix < t.Length) && (t.[ix] = c)

// Various whiteSpace skippers.
let isWS c = Char.IsWhiteSpace c
let isNonEolWS c = (Char.IsWhiteSpace c) && (c <> '\n')
let isNonEol c = (c <> '\n')

let skipWhiteSpace t ix = skipWhileCh isWS t ix
let skipNonEolWhiteSpace t ix = skipWhileCh isNonEolWS t ix
let skipEolComment commentPrefix t ix =
    if   atStr commentPrefix t ix
    then 1 + skipWhileCh isNonEol t (ix + commentPrefix.Length)
    else ix
let rec skipWhiteSpaceAndEolComments commentPrefix t ix =
    let ix' = skipWhiteSpace t ix
    let ix' = skipEolComment commentPrefix t ix'
    if ix' = ix then ix else skipWhiteSpaceAndEolComments commentPrefix t ix'
let doNotSkipWhiteSpace _ ix = ix

// The current offset in the source.
let here (cx: Ctxt) ix = Pass(ix, ix)

let skipWS (cx: Ctxt) ix = cx.SkipWhiteSpace cx.Text ix

// Peek at the current char or NUL if at EOF.
let currCh (cx: Ctxt) ix =
    if ix < cx.Text.Length then cx.Text.[ix] else '\u0000'

// Match a specific char without skipping whiteSpace.
let ch (c: char) (cx: Ctxt) ix =
    if atCh c cx.Text ix then Pass(c, ix + 1) else Fail

// Test the current char.  Do not advance.
let currChIs (p: char -> bool) (cx: Ctxt) ix =
    if p(currCh cx ix) then Pass((), ix + 1) else Fail

let currChIsNot (p: char -> bool) (cx: Ctxt) ix =
    if p(currCh cx ix) then Fail else Pass((), ix)

let anyCh (cx: Ctxt) ix = Pass(currCh cx ix, ix + 1)

// Skip over whiteSpace and match the given string.
let str (s: string) (cx: Ctxt) ix =
    let ix = skipWS cx ix
    if atStr s cx.Text ix then Pass((), ix + s.Length) else Fail

// Succeed iff the given parser fails.
let butNot (p: Parser<'t>) cx ix =
    match p cx ix with
    | Pass(_, _)            -> Fail
    | Fail                  -> Pass((), ix)
    | Error(what, ix)       -> Error(what, ix)

// Parse two consecutive matches, returning both results.
let (>>>) (p: Parser<'t>) (q: Parser<'u>) cx ix =
    match p cx ix with
    | Fail                  -> Fail      
    | Error(what, ix)       -> Error(what, ix)
    | Pass(x, ix)           ->
        match q cx ix with
        | Fail              -> Fail      
        | Error(what, ix)   -> Error(what, ix)
        | Pass(y, ix)       -> Pass((x, y), ix)

// Parse two consecutive matches, returning the second result.
let (.>>) (p: Parser<'t>) (q: Parser<'u>) cx ix =
    match p cx ix with
    | Fail                  -> Fail      
    | Error(what, ix)       -> Error(what, ix)
    | Pass(_, ix)           ->
        match q cx ix with
        | Fail              -> Fail      
        | Error(what, ix)   -> Error(what, ix)
        | Pass(y, ix)       -> Pass(y, ix)

// Parse two consecutive matches, returning the first result.
let (>>.) (p: Parser<'t>) (q: Parser<'u>) cx ix =
    match p cx ix with
    | Fail                  -> Fail      
    | Error(what, ix)       -> Error(what, ix)
    | Pass(x, ix)           ->
        match q cx ix with
        | Fail              -> Fail      
        | Error(what, ix)   -> Error(what, ix)
        | Pass(_, ix)       -> Pass(x, ix)

// Parse then map the result.
let (>>*) (p: Parser<'t>) f cx ix =
    match p cx ix with
    | Fail                  -> Fail      
    | Error(what, ix)       -> Error(what, ix)
    | Pass(x, ix)           -> f x ix

// Parse then map the result.
let (>>%) (p: Parser<'t>) f cx ix =
    match p cx ix with
    | Fail                  -> Fail      
    | Error(what, ix)       -> Error(what, ix)
    | Pass(x, ix)           -> Pass(f x, ix)

// Parse then continue with the result.
let (>>~) (p: Parser<'t>) p' cx ix =
    match p cx ix with
    | Fail                  -> Fail      
    | Error(what, ix)       -> Error(what, ix)
    | Pass(x, ix)           -> p' x cx ix

// Match p, unless it fails, in which case match q.
let (|||) (p: Parser<'t>) (q: Parser<'t>) cx ix =
    match p cx ix with
    | Fail                  -> q cx ix
    | _ as result           -> result

let pass x (cx: Ctxt) ix = Pass(x, ix)

let error what (cx: Ctxt) ix = Error(what, ix)

// Match p if possible.
let opt (p: Parser<'t>) cx ix =
    match p cx ix with
    | Fail                  -> Pass(None, ix)
    | Error(what, ix)       -> Error(what, ix)
    | Pass(x, ix)           -> Pass(Some x, ix)

// Match as many p as possible, zero is acceptable.
let zeroOrMore (p: Parser<'t>) cx ix =
    let ix = skipWS cx ix
    let rec zom xs ix =
        match p cx ix with
        | Fail              -> Pass(List.rev xs, ix)
        | Error(what, ix)   -> Error(what, ix)
        | Pass(x, ix)       -> zom (x :: xs) ix
    zom [] ix

// Match as many p as possible, at least one match is required.
let oneOrMore (p: Parser<'t>) cx ix =
    match zeroOrMore p cx ix with
    | Pass([], ix)          -> Fail
    | _ as result           -> result

// Expect a match; failure indicates a syntax error.
let expect what (p: Parser<'t>) cx ix =
    let ix = skipWS cx ix
    match p cx ix with
    | Fail                  -> Error("expected " + what, ix)
    | _ as result           -> result

// Match a bracketed term.
let bracketed (l: Parser<_>) (p: Parser<'t>) (r: Parser<_>) cx ix =
    let ix = skipWS cx ix
    (l .>> p >>. r) cx ix

let lPar = str "("
let rPar = expect "')'" (str ")")
let lBracket = str "["
let rBracket = expect "']'" (str "]")
let lBrace = str "{"
let rBrace = expect "'}'" (str "}")
let comma = str ","
let semic = str ";"
let colon = str ":"
let equals = str "="

let inParentheses (p: Parser<'t>) = bracketed lPar p rPar
let inBrackets    (p: Parser<'t>) = bracketed lBracket p rBracket
let inBraces      (p: Parser<'t>) = bracketed lBrace p rBrace

let separated (pdesc: string) (p: Parser<'t>) (sep: Parser<_>) cx ix =
    let ix = skipWS cx ix
    match p cx ix with
    | Fail                  -> Pass([], ix)
    | Error(what, ix)       -> Error(what, ix)
    | Pass(x, ix)           ->
        let rec sepd xs cx ix =
            match sep cx ix with
            | Fail              -> Pass(List.rev xs, ix)
            | Error(what, ix)   -> Error(what, ix)
            | Pass(_, ix)       ->
                match expect pdesc p cx ix with
                | Error(what, ix)   -> Error(what, ix)
                | Pass(x, ix)       -> sepd (x :: xs) cx ix
                | _                 -> failwith "OMG!"
        sepd [x] cx ix

let commaSeparated (pdesc: string) p = separated pdesc p comma

let terminated (p: Parser<'t>) (sepdesc: string) (sep: Parser<'u>) cx ix =
    let ix = skipWS cx ix
    let rec terd xs cx ix =
        match p cx ix with
        | Fail              -> Pass(List.rev xs, ix)
        | Error(what, ix)   -> Error(what, ix)
        | Pass(x, ix)       ->
            match expect sepdesc sep cx ix with
            | Error(what, ix)   -> Error(what, ix)
            | Pass(_, ix)       -> terd (x :: xs) cx ix
            | _                 -> failwith "OMG!"
    terd [] cx ix

// Is the given character one that can start an identifier [A-Za-z_]?
let isIdStartCh c = (Char.IsLetter c) || (c = '_')

// Is the given character one that can be in an identifier [A-Za-z_0-9]?
let isIdCh c = (Char.IsLetterOrDigit c) || (c = '_')

// Skip whiteSpace and match a keyword (which must not be followed
// by [A-Za-z_0-9]).

// Skip whiteSpace and match an identifier [A-Za-z_][A-Za-z_0-9]*.
let identifier (cx: Ctxt) ix =
    let ix = skipWS cx ix
    if not (isIdStartCh (currCh cx ix)) then
        Fail
    else
        let ixStart = ix
        let ixEnd = skipWhileCh isIdCh cx.Text ix
        let n = ixEnd - ixStart
        let s = cx.Text.Substring(ixStart, n);
        Pass(s, ixEnd)

// A keyword is a given string that is not followed by [A-Za-z_0-9].
let keyword kw =
    str kw .>> currChIsNot (isIdCh)

// Skip whiteSpace and match an integer.
let uinteger (cx: Ctxt) ix : Result<uint64> =
    let t = cx.Text
    let ix = skipWS cx ix
    let ix' = skipWhileCh Char.IsDigit t ix
    let n = ix' - ix
    if n = 0 then
        Fail
    else
        let s = t.Substring(ix, n)
        try
            let v = UInt64.Parse(s)
            Pass(v, ix')
        with
        | _ -> Error("Unsigned integer literal out of range: " + s, ix)

let integer (cx: Ctxt) ix : Result<int64> =
    let t = cx.Text
    let ix = skipWS cx ix
    let ix' = ix + (if atCh '-' t ix then 1 else 0)
    let ix'' = skipWhileCh Char.IsDigit t ix
    let n = ix'' - ix
    if ix' = ix'' then
        Fail
    else
        let s = t.Substring(ix, n)
        try
            let v = Int64.Parse(s)
            Pass(v, ix'')
        with
        | _ -> Error("Signed integer literal out of range: " + s, ix)

// XXX TODO: double, string.

let characterEscape (cx: Ctxt) ix : Result<char> =
    let t = cx.Text
    if atCh '\\' t ix then
        failwith "NYI: character escapes"
    else
        Fail

let character (cx: Ctxt) ix : Result<char> =
    let ix = skipWS cx ix
    (   ch '''
    .>> expect "character literal body" (characterEscape ||| anyCh)
    >>. expect "closing single quote" (ch ''')
    ) cx ix

