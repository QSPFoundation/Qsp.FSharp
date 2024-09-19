namespace Qsp.Parser.Scope
open FsharpMyExtension

type VarId = int
type 'VarName Scopes when 'VarName : comparison = Map<'VarName, VarId> list

[<RequireQualifiedAccess>]
module Scopes =
    let pushEmpty (scopes:_ Scopes) : Scopes<_> = Map.empty::scopes

    let pop (scopes:_ Scopes) : Scopes<_> =
        match scopes with
        | x::xs -> xs
        | [] -> failwith "scopes is empty"

type ScopeSystem<'VarName, 'Value> when 'VarName : comparison =
    {
        Scopes: 'VarName Scopes
        NewVarId: VarId
        Variables: Map<VarId, 'VarName * 'Value list>
    }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module ScopeSystem =
    let empty =
        {
            Scopes = [Map.empty]
            NewVarId = 0
            Variables = Map.empty
        }

    let pushEmptyScope (scopeSystem: ScopeSystem<_,_>) =
        { scopeSystem with
            Scopes = Scopes.pushEmpty scopeSystem.Scopes
        }

    let popScope (scopeSystem: ScopeSystem<_,_>) =
        { scopeSystem with
            Scopes = Scopes.pop scopeSystem.Scopes
        }

    let addAsRead (varName: 'VarName, getValue) (scopeSystem: ScopeSystem<_,_>) =
        let vars = scopeSystem.Variables
        let rec f acc (scopes: _ Scopes) =
            match scopes with
            | [m] ->
                match Map.tryFind varName m with
                | Some varId ->
                    let vars =
                        let x = mapSnd getValue vars.[varId]
                        Map.add varId x vars
                    let scopes =
                        List.fold (fun xs x -> x::xs) scopes acc
                    let x =
                        {
                            Scopes = scopes
                            NewVarId = scopeSystem.NewVarId
                            Variables = vars
                        }
                    varId, x
                | None ->
                    let m = Map.add varName scopeSystem.NewVarId m
                    let vars =
                        Map.add scopeSystem.NewVarId (varName, getValue []) vars
                    let scopes =
                        List.fold (fun xs x -> x::xs) [m] acc
                    let x =
                        {
                            Scopes = scopes
                            NewVarId = scopeSystem.NewVarId + 1
                            Variables = vars
                        }
                    scopeSystem.NewVarId, x
            | m::ms ->
                match Map.tryFind varName m with
                | Some varId ->
                    let vars =
                        let x = mapSnd getValue vars.[varId]
                        Map.add varId x vars
                    let scopes =
                        List.fold (fun xs x -> x::xs) scopes acc
                    let x =
                        {
                            Scopes = scopes
                            NewVarId = scopeSystem.NewVarId
                            Variables = vars
                        }
                    varId, x
                | None ->
                    f (m::acc) ms
            | [] -> failwith "the scope cannot be empty"
        f [] scopeSystem.Scopes

    let addAsWrite (varName: 'VarName, getValue) (scopeSystem: ScopeSystem<_,_>) =
        match scopeSystem.Scopes with
        | m::ms ->
            match Map.tryFind varName m with
            | None ->
                let newVarId = scopeSystem.NewVarId
                let m = Map.add varName newVarId m
                let vars =
                    Map.add newVarId (varName, getValue []) scopeSystem.Variables
                let scopes = m::ms
                let x =
                    {
                        NewVarId = newVarId + 1
                        Scopes = scopes
                        Variables = vars
                    }
                newVarId, x
            | Some varId ->
                let m = Map.add varName varId m
                let vars = scopeSystem.Variables
                let vars =
                    let x = mapSnd getValue vars.[varId]
                    Map.add varId x vars
                let scopes = m::ms
                let x =
                    {
                        NewVarId = scopeSystem.NewVarId
                        Scopes = scopes
                        Variables = vars
                    }
                varId, x
        | [] -> failwith "the scope cannot be empty"
