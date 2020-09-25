module Qsp.Ast
open FsharpMyExtension
open Qsp

[<Struct>]
type Position = { StreamName:string; Index:int64; Line:int64; Column:int64 }
let positionCreate streamName index line column =
    { StreamName = streamName; Index = index; Line = line; Column = column }
let positionEmpty =
    positionCreate "" 0L 0L 0L

type NoEqualityPosition(pos:Position) =
    member __.Pos = pos
    override __.ToString() = pos.ToString()
    override __.Equals _ = true
    override __.GetHashCode() = 0

let test () =
    let x = NoEqualityPosition(positionCreate "" 0L 0L 0L)
    let y = NoEqualityPosition(positionCreate "" 0L 0L 1L)
    x = y

[<Struct>]
type Op =
    /// `+`
    | Plus
    /// `-`
    | Minus
    /// `*`
    | Times
    /// `/`
    | Divide
    /// `mod`
    | Mod

    /// `=`
    | Eq
    /// `>`
    | Gt
    /// `>=`
    | Ge
    /// `&lt;`
    | Lt
    /// `&lt;=`
    | Le
    /// `!` — то же самое, что и `&lt;>`
    | Bang
    /// `&lt;>`
    | Ne
    /// `=&lt;`
    | El
    /// `=>`
    | Eg
    /// `and`
    | And
    /// `or`
    | Or
type IsBinOpSymbolic = bool
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module Op =
    [<ReflectedDefinition>]
    let toString = function
        | Times -> "*"
        | Divide -> "/"
        | Mod -> "mod"
        | Plus -> "+"
        | Minus -> "-"
        | Lt -> "<"
        | Gt -> ">"
        | Le -> "<="
        | Eg -> "=>"
        | Ge -> ">="
        | El -> "=<"
        | Ne -> "<>"
        | And -> "and"
        | Or -> "or"
        | Eq -> "="
        | Bang -> "!"

    let ops =
        Reflection.Reflection.unionEnum<Op>
        |> Array.map (fun x ->
            let IsBinOpSymbolic opName =
                not <| String.exists System.Char.IsLetter opName
                : IsBinOpSymbolic
            let y = toString x
            x, (y, IsBinOpSymbolic y) )

    let fromString =
        let m = Array.map (fun (a, b) -> b, a) ops |> Map.ofArray
        fun x -> match Map.tryFind x m with Some x -> x | None -> failwithf "not found %A" x
[<Struct>]
type UnarOp =
    /// `-`
    | Neg
    /// `obj`
    | Obj
    /// `no`
    | No
    /// `loc`
    | Loc
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module UnarOp =
    [<ReflectedDefinition>]
    let toString = function | Obj -> "obj" | Neg -> "-" | No -> "no" | Loc -> "loc"
    let ops =
        Reflection.Reflection.unionEnum<UnarOp>
        |> Array.map (fun x -> x, toString x)
    let fromString =
        let m = Array.map (fun (a, b) -> b, a) ops |> Map.ofArray
        fun x -> match Map.tryFind x m with Some x -> x | None -> failwithf "not found %A" x
module Precedences =
    type T = OpB of Op | PrefB of UnarOp
    // &
    // OR
    // AND
    // OBJ, NO
    // =, <, >, !, <>, <=, >=, =<, =>
    // +, -
    // MOD
    // *, /
    // +, - (унарные)

    let prec = function
        | OpB Or -> 1
        | OpB And -> 2
        | PrefB No -> 3
        | PrefB Loc | PrefB Obj -> 4 // `no obj 'apple'` equal `no (obj 'apple')`
        // =     | <      | >      | !        | <>     | <=     | >=     | =>     | =<
        | OpB Eq | OpB Lt | OpB Gt | OpB Bang | OpB Ne | OpB Le | OpB Ge | OpB Eg | OpB El-> 5
        | OpB Plus | OpB Minus -> 6
        | OpB Mod -> 7
        | OpB Times | OpB Divide -> 8
        | PrefB Neg -> 9
[<Struct>]
type VarType =
    /// `varName`
    | NumericType
    /// `$varName`
    | StringType
type 'Predef PredefUndef =
    | Predef of 'Predef
    | Undef of string
type VarName = string
type Var = VarType * VarName
type StmtsOrRaw =
    | Raw of string
    | StaticStmts of PosStatement list
and LineKind =
    | StringKind of string
    /// Это то, что заключено между `&lt;&lt; >>`
    | ExprKind of Expr
    /// `&lt;a href="exec: ...">some text&lt;/a>`
    | HyperLinkKind of StmtsOrRaw * Line list
/// Без переносов
and Line = LineKind list

and Value =
    | Int of int
    | String of Line list

and Expr =
    | Val of Value
    | Var of var:Var
    | Func of Defines.PredefFunc PredefUndef * Expr list
    | Arr of var:Var * Expr list
    | UnarExpr of UnarOp * Expr
    | Expr of Op * Expr * Expr

and AssignWhat =
    | AssignVar of var:Var
    /// Ключом массива может быть значение любого типа
    | AssignArr of var:Var * key:Expr
    | AssignArrAppend of var:Var
and PosStatement = NoEqualityPosition * Statement
and Statement =
    | Assign of isLocal:bool * AssignWhat * Expr
    | AssignCode of AssignWhat * PosStatement list
    | Proc of string * Expr list
    | If of Expr * PosStatement list * PosStatement list
    | Act of Expr list * PosStatement list
    | For of var:Var * from:Expr * to':Expr * step:Expr option * body:PosStatement list
    | Loop of preStmts:PosStatement list * Expr * step:PosStatement list * body:PosStatement list
    | Label of string
    | Comment of string
    | Exit
type LocationName = string
/// ```qsp
/// # location name
/// 'asdf'
/// - произвольный набор символов
/// ```
type Location = Location of LocationName * PosStatement list
