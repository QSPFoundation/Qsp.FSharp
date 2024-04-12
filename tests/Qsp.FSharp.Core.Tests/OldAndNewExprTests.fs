module OldAndNewExprTests
open Fuchu

let testf input =
    testCase input <| fun _ ->
        let f input =
            let exp =
                Qsp.Parser.Generic.runStateEither Qsp.Parser.Expr.pExprOld Qsp.Parser.Generic.emptyState input
                |> snd
            let act =
                Qsp.Parser.Generic.runStateEither Qsp.Parser.Expr.pExprNew Qsp.Parser.Generic.emptyState input
                |> snd
            exp, act
        let exp, act = f input
        Assert.Equal("", exp, act)
[<Tests>]
let tests =
    testList "expr test" [
        testf "1 + 2 * 3"
        testf "1 + 2 mod 3 * 4"
        testf "-1 - -1"
        testf "-(a + b)"
        testf "var1 and var2 and no var3 and obj var4"
        testf "var1[var1 + var2] and func(arg1, arg2[expr], x + y)"
        testf "a = 10 or b = 20 and c = 30"
        testf "a = pstam> (pmaxstam/4)*2 and pstam <= (pmaxstam/4)*3"
        testf "no obj 'apple'"
        testf "a = no -a > b"
        testf "a and b = c and d"
        testf "obj 'яблоко' = 0"
    ]
let start () = run tests
