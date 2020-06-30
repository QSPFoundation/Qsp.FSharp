open Fuchu
open FsharpMyExtension

[<Tests>]
let simpleTest =
    testCase "simpleTest" (fun _ ->
        let exp = "a"
        let act = "a"
        Assert.Equal("msg", exp, act)
    )
// run simpleTest // <- если нужно запустить тест вручную

[<Tests>]
let simpleTestList =
    testList "testListName" [
        testCase "testCase1" (fun _ ->
            let exp = true
            let act = true
            Assert.Equal("msg1", exp, act)
        )
        testCase "testCase2" (fun _ ->
            let exp = 1
            let act = 1
            Assert.Equal("msg2", exp, act)
        )
        testCase "testCase3" (fun _ ->
            let exp = ()
            let act = ()
            Assert.Equal("msg3", exp, act)
        )
    ]
// run simpleTestList

[<EntryPoint;System.STAThread>]
let main arg =
    defaultMainThisAssembly arg
