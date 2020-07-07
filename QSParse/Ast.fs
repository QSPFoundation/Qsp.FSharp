module Qsp.Ast
open FsharpMyExtension
type Value =
    | Int of int
    | String of string

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
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module UnarOp =
    [<ReflectedDefinition>]
    let toString = function | Obj -> "obj" | Neg -> "-" | No -> "no"
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
        | PrefB Obj | PrefB No -> 3
        // =     | <      | >      | !        | <>     | <=     | >=     | =>     | =<
        | OpB Eq | OpB Lt | OpB Gt | OpB Bang | OpB Ne | OpB Le | OpB Ge | OpB Eg | OpB El-> 4
        | OpB Plus | OpB Minus -> 5
        | OpB Mod -> 6
        | OpB Times | OpB Divide -> 7
        | PrefB Neg -> 8

type VarType =
    /// `varName`, если к такой присвоить строковое значение, то интерпретатор попытается преобразовать ее в число. Если не получится, выбьет ошибку.
    | ImplicitNumericType
    /// `#varName`, если к такой присвоить строковое значение, то интерпретатор попытается преобразовать ее в число. Если не получится, выбьет ошибку.
    | ExplicitNumericType
    /// `$varName`, к такой переменной можно смело присваивать и число, и строку
    | StringType
type Expr =
    | Val of Value
    | Var of var:(VarType * string)
    | Func of string * Expr list
    | Arr of var:(VarType * string) * Expr list
    | UnarExpr of UnarOp * Expr
    | Expr of Op * Expr * Expr

type Assign =
    | AssignVar of var:(VarType * string)
    /// Ключом массива может быть значение любого типа
    | AssignArr of var:(VarType * string) * key:Expr

type Statement =
    | Assign of Assign * Expr
    | AssingCode of Assign * Statement list
    | CallSt of string * Expr list
    /// Вычисляется `expr` и посылается в `*pl`
    | StarPl of Expr
    | If of Expr * Statement list * Statement list
    | Act of Expr list * Statement list
    | Label of string
    | Comment of string
    | Exit

/// ```qsp
/// # location name
/// 'asdf'
/// - произвольный набор символов
/// ```
type Location = Location of string * Statement list
