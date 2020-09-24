module Qsp.Parser.Scope
open FsharpMyExtension

type VarId = int
type 'VarName Scopes when 'VarName : comparison = Map<'VarName, VarId> list

type ScopeSystem<'VarName, 'Value> when 'VarName : comparison =
    {
        Scopes: 'VarName Scopes
        NewVarId: VarId
        Result : Map<VarId, 'VarName * 'Value list>
    }
let scopeSystemEmpty =
    {
        Scopes = [Map.empty]
        NewVarId = 0
        Result = Map.empty
    }
let addAsRead (varName:'VarName, getValue) (scopeSystem: ScopeSystem<_,_>) =
    let result = scopeSystem.Result
    let rec f acc (scopes:_ Scopes) =
        match scopes with
        | [m] ->
            match Map.tryFind varName m with
            | Some varId ->
                let result =
                    let x = mapSnd getValue result.[varId]
                    Map.add varId x result
                let scopes =
                    List.fold (fun xs x -> x::xs) scopes acc
                let x =
                    {
                        Scopes = scopes
                        NewVarId = scopeSystem.NewVarId
                        Result = result
                    }
                varId, x
            | None ->
                let m = Map.add varName scopeSystem.NewVarId m
                let result =
                    Map.add scopeSystem.NewVarId (varName, getValue []) result
                let scopes =
                    List.fold (fun xs x -> x::xs) [m] acc
                let x =
                    {
                        Scopes = scopes
                        NewVarId = scopeSystem.NewVarId + 1
                        Result = result
                    }
                scopeSystem.NewVarId, x
        | m::ms ->
            match Map.tryFind varName m with
            | Some varId ->
                let result =
                    let x = mapSnd getValue result.[varId]
                    Map.add varId x result
                let scopes =
                    List.fold (fun xs x -> x::xs) scopes acc
                let x =
                    {
                        Scopes = scopes
                        NewVarId = scopeSystem.NewVarId
                        Result = result
                    }
                varId, x
            | None ->
                f (m::acc) ms
        | [] -> failwith "the scope cannot be empty"
    f [] scopeSystem.Scopes

let addAsWrite (varName:'VarName, getValue) (scopeSystem: ScopeSystem<_,_>) =
    match scopeSystem.Scopes with
    | m::ms ->
        let newVarId = scopeSystem.NewVarId
        let m = Map.add varName newVarId m // that's ok: variables can be overwritten
        let result =
            Map.add newVarId (varName, getValue ()) scopeSystem.Result
        let scopes = m::ms
        let x =
            {
                NewVarId = newVarId + 1
                Scopes = scopes
                Result = result
            }
        newVarId, x
    | [] -> failwith "the scope cannot be empty"

let appendScope (scopes:_ Scopes) = Map.empty::scopes

let removeScope (scopes:_ Scopes) =
    match scopes with
    | x::xs -> xs
    | [] -> failwith "scopes is empty"

let test () =
    let init =
        {
            Scopes = [Map.empty]
            NewVarId = 0
            Result = Map.empty
        }

    let (_, x) = addAsRead ("x", (fun xs -> (0, 0)::xs)) init
    addAsRead ("x", (fun xs -> (0, 0)::xs)) x
