(*

Blush
Ralph Becket
Started 2020-02-16

I used to work on the design and implementation of pure,
declarative programming languages.  Around 2010 I moved
into industry, where I have mainly used C#.  C# is, in my
opinion, the best of breed of the mainstream languages
and many ideas from functional programming have been
included in C# since its inception.  However, main of the
things that I found really productive about pure languages
either have not or cannot be part of C#.  In short, I
would like to offer commercial, imperative programmers a
language with the following features:

- algebraic data types with pattern matching;
- (no nulls!);
- immutability by default;
- (although assignment will be a natural part of the 
  language since we don't want to alienate the non-
  academics);
- a rich, statically checked type system;
- a lightweight syntax encouraging the use of many small
  types to express and enforce program invariants;
- support for (at least) monadic programming.

*)


open System
open Parserfal


(*

# Types

type index = int                        // A type alias.

type strint = fn(string) = int          // A function type alias.

type foo = array(var int)               // An array of mutable ints.
                                        // Note: var 't is a subtype of 't
                                        // for any 't.

type bool                               // A discriminated union.
| no
| yes

type maybe('a)                          // A parametric type.
| no
| yes('a)

type either('a, 'b)                     // Another parametric type.
| left('a)
| right('b)

*)

type V = string                         // A variable identifier.

type TVar = V                           // A type variable identifier.

type TName = string                     // A type name.

type Ctor = string                      // A data constructor name.

type TExpr =                            // A type expression.
    | Var of TVar                       // 'a
    | App of TName * list<TExpr>        // f(T, ...)
    | Abs of list<TExpr> * TExpr        // fn(T, ...) -> T

type TScheme = list<TVar> * TExpr

type TSkel = Skel of TName * list<TVar>

type TCtor = Ctor of Ctor * list<TExpr>

type TDefn =
    | Alias of TExpr                    // f('a, ...) -> T
    | Union of list<TCtor>              // f('a, ...) | f1(T, ...) | f2(T, ...)
    | Abstract

type TDecl = TSkel * TDefn

let reservedWords = ["type"]

let ident =
        identifier
    >>* fun x ix ->
            if   List.contains x reservedWords
            then Error("expected a name, but '" + x + "' is a reserved word", ix)
            else Pass(x, ix)

let typeKeyword = keyword "type"

let typeVar = str "'" .>> ident 

let optList pdesc p =
        inParentheses (commaSeparated pdesc p)
    ||| pass []

let typeSkel =
        ident
    >>> optList "a type variable" typeVar
    >>% fun (tname, tparams) -> Skel(tname, tparams)

let rec typeExpr cx ix =
    (       ( typeExprAtom >>~ typeExprMaybeAbs )
        ||| ( typeExprAtoms >>~ typeExprsMaybeAbs )
    ) cx ix

and typeExprVar = typeVar >>% Var

and typeExprCtor =
        ident
    >>> optList "type expression" typeExpr
    >>% App

and typeExprAtom =
        typeExprVar
    ||| typeExprCtor

and typeExprMaybeAbs t =
        ( str "->" .>> expect "type expression" typeExpr >>% fun u -> Abs([t], u))
    ||| pass t

and typeExprAtoms =
        (typeExprAtom >>% fun texpr -> [texpr])
    ||| inParentheses (commaSeparated "type expression" typeExpr)

and typeExprsMaybeAbs ts =
        ( str "->" .>> expect "type expression" typeExpr >>% fun u -> Abs(ts, u))
    ||| ( match ts with
        | [t] -> pass t
        | _ -> error "expected '->'"
        )

let typeAliasDecl =
        str "="
    .>> expect "a type expression" typeExpr
    >>% Alias

let typeCtorDecl =
        str "|"
    .>> expect "a data constructor name" ident
    >>> optList "type expression" typeExpr
    >>% Ctor

let typeCtorDecls =
        oneOrMore typeCtorDecl
    >>% Union

let typeDeclaration =
        typeKeyword
    .>> expect "a type skeleton" typeSkel
    >>> (   typeAliasDecl
        ||| typeCtorDecls
        ||| pass Abstract
        )

let testType = "
type foo
type bar = int
type baz('t) = int
type bazz('t) = 't
type maybe('t) | no | yes('t)
type fn('t, 'u) = 't -> 'u
type fnn('t, 'u) = ('t, 't) -> 'u
type fnnn('t, 'u) = maybe('t) -> 'u
type fnnnn('t, 'u) = (maybe('t), 't) -> 'u
// type bad = 123
// type bad | int
// type bad(asd)
"

[<EntryPoint>]
let main argv =
    let cx = { Path = ""; Text = testType; SkipWhiteSpace = skipWhiteSpaceAndEolComments "//" }
    let r = oneOrMore typeDeclaration cx 0
    printfn "%A" r
    0 // return an integer exit code
