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

    let ops =
        Reflection.Reflection.unionEnum<Op>
        |> Array.map (fun x -> x, toString x)

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

    //&
    //OR
    //AND
    //OBJ, NO
    //=, <, >, !, <>, <=, >=, =<, =>
    //+, -
    //MOD
    //*, /
    //+, - (унарные)

//        "=, <, >, <>, <=, >=".Split([|", "|], System.StringSplitOptions.None)
//        |> Array.map (Op.fromString >> sprintf "OpB %A") |> join " | "
    let prec = function
        | OpB Or -> 1
        | OpB And -> 2
        | PrefB Obj | PrefB No -> 3
        // =     | <      | >      | <>     | <=     | >=     | =>     | =<
        | OpB Eq | OpB Lt | OpB Gt | OpB Ne | OpB Le | OpB Ge | OpB Eg | OpB El-> 4
        | OpB Plus | OpB Minus -> 5
        | OpB Mod -> 6
        | OpB Times | OpB Divide -> 7
        | PrefB Neg -> 8
type Expr =
    | Val of Value
    | Var of string
    | Func of string * Expr list
    | Arr of string * Expr list
    | UnarExpr of UnarOp * Expr
    | Expr of Op * Expr * Expr

type Assign =
    | AssignVar of string
    | AssignArr of string * Expr
type Statement =
    | Assign of Assign * Expr
    | AssingCode of Assign * Statement list
    | CallSt of string * Expr list
    | StarPl of Expr
    | If of Expr * Statement list * Statement list
    | Act of Expr list * Statement list
    | Sign of string
    | Comment of string
type Location = Location of string * Statement list
