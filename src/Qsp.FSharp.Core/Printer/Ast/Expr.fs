[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Qsp.Printer.Ast.Expr

module Printer =
    open FsharpMyExtension
    open FsharpMyExtension.ShowList

    open Qsp
    open Qsp.Ast

    let ops = Op.toString >> showString

    let unar = function No -> "no" | Obj -> "obj" | Neg -> "-" | Loc -> "loc"
    let showFuncName = function
        | PredefUndef.Predef name ->
            match Map.tryFind name Qsp.Defines.functionBySymbolic with
            | Some x ->
                let _, returnedType = x.Signature
                let returnedType =
                    match returnedType with
                    | Defines.Numeric -> id
                    | Defines.String -> showChar '$'
                    | Defines.Any -> id // TODO: defines by argument type
                let nameStr = (string name).ToLower()
                returnedType << showString nameStr
            | None -> failwithf "%A not found in `functionBySymbolic`" name
        | PredefUndef.Undef name ->
            showString name

    let showArrayArgs xs =
        bet "[" "]" (join ", " xs)

    let showTupleArgs xs =
        bet "(" ")" (join ", " xs)

    let rec showExpr showStmtsInline = function
        | Val v -> Value.Printer.showValue (showExpr showStmtsInline) showStmtsInline v
        | Var v -> Value.Printer.showVar v
        | Func(name, args) ->
            let args =
                if List.isEmpty args then
                    empty
                else
                    showTupleArgs (List.map (showExpr showStmtsInline) args)
            showFuncName name << args
        | UnarExpr(op, e) ->
            let space = function Obj | No | Loc -> showSpace | Neg -> id
            showString (unar op) << space op << showExpr showStmtsInline e
        | Expr(op, e1, e2) ->
            let prec = Precedences.OpB >> Precedences.prec
            let f = function
                | Expr(op', _, _) -> showParen (prec op > prec op')
                | UnarExpr _ -> showParen true | _ -> id
            let f x = f x (showExpr showStmtsInline x)
            f e1 << showSpace << ops op << showSpace << f e2
        | Tuple args ->
            showTupleArgs (List.map (showExpr showStmtsInline) args)
        | Arr(var, args) ->
            Value.Printer.showVar var << showArrayArgs (List.map (showExpr showStmtsInline) args)
