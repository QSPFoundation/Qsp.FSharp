module Qsp.Ast
open FsharpMyExtension

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
    /// `!` or `&lt;>`
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
                not <| String.exists FParsec.CharParsers.isLetter opName
                : IsBinOpSymbolic
            let y = toString x
            x, (y, IsBinOpSymbolic y) )

    let fromString =
        let m = Array.map (fun (a, b) -> b, a) ops |> Map.ofArray
        fun x -> match Map.tryFind x m with Some x -> x | None -> failwithf "not found %A" x
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

type VarType =
    /// `varName`, если к такой присвоить строковое значение, то интерпретатор попытается преобразовать ее в число. Если не получится, выбьет ошибку.
    | ImplicitNumericType
    /// `#varName`, если к такой присвоить строковое значение, то интерпретатор попытается преобразовать ее в число. Если не получится, выбьет ошибку.
    | ExplicitNumericType
    /// `$varName`, к такой переменной можно смело присваивать и число, и строку
    | StringType
type Var = VarType * string
type StmtsOrRaw =
    | Raw of string
    | StaticStmts of Statement list
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
    | Func of string * Expr list
    | Arr of var:Var * Expr list
    | UnarExpr of UnarOp * Expr
    | Expr of Op * Expr * Expr

and AssignWhat =
    | AssignVar of var:Var
    /// Ключом массива может быть значение любого типа
    | AssignArr of var:Var * key:Expr
    | AssignArrAppend of var:Var
and Statement =
    | Assign of AssignWhat * Expr
    | AssignCode of AssignWhat * Statement list
    | CallSt of string * Expr list
    /// Вычисляется `expr` и посылается в `*pl`
    | StarPl of Expr
    | If of Expr * Statement list * Statement list
    | Act of Expr list * Statement list
    | For of var:Var * from:Expr * to':Expr * body:Statement list
    | Label of string
    | Comment of string
    | Exit
type LocationName = string
/// ```qsp
/// # location name
/// 'asdf'
/// - произвольный набор символов
/// ```
type Location = Location of LocationName * Statement list
