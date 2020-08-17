module Qsp.Defines

module Tools =
    open FParsec
    type 'a Parser = Parser<'a, unit>
    let removeEmptyLines () =
        Clipboard.getSet (fun str ->
            // let x = System.Text.RegularExpressions.Regex.Replace(str, "^\n", "", System.Text.RegularExpressions.RegexOptions.Multiline)
            // x
            let ws = manySatisfy (fun c -> System.Char.IsWhiteSpace c && c <> '\n')
            let wsLine = ws .>>? skipNewline
            let p =
                spaces
                >>. many
                    (
                        ws >>. many1Satisfy ((<>) '\n')
                        .>> (skipNewline <|> eof)
                        .>> many wsLine
                    )
                |>> String.concat "\n"
            match run p str with
            | Success(x, _, _) -> x
            | Failure(x, _, _) -> failwithf "%A" x
        )
    // removeEmptyLines()
    module Show =
        open FsharpMyExtension
        open FsharpMyExtension.ShowList
        let print tabsCount isFunction xs =
            let tab = replicate 4 ' '
            let showStr x =
                showAutoParen "\""
                    (showString
                        (x
                         |> String.collect (
                             function
                             | '"' -> "\\\""
                             | '\\' -> "\\\\"
                             | x -> string x )))
            xs
            |> List.collect (fun (descs, varName) ->
                let desc =
                    [
                        yield showChar '['
                        yield!
                            descs
                            |> List.map (fun x ->
                                tab << showStr x)
                        yield showChar ']' << showString " |> String.concat \"\\n\""
                    ]
                [
                    yield showString "let dscr ="
                    yield! List.map ((<<) tab) desc
                    let signature =
                        if isFunction then
                            showString ", " << showString "failwith \"not implemented\""
                        else
                            empty
                    yield
                        showAutoParen "\"" (showString varName) << showString ", " << showString "dscr"
                        << signature

                ]
            )
            |> List.map ((<<) (showReplicate tabsCount tab))
            |> joinEmpty "\n"
            |> show

    let parse tabsCount isFunction =
        Clipboard.getSet (fun str ->
            let description =
                many1
                    (pstring "///" >>. optional (skipChar ' ') >>. manySatisfy ((<>) '\n') .>> spaces)
            let expr =
                between
                    (skipChar '"')
                    (skipChar '"')
                    (manySatisfy ((<>) '"'))
            let p = spaces >>. many (description .>>. expr .>> spaces)
            match run (p .>> eof) str with
            | Success(xs, _, _) -> Show.print tabsCount isFunction xs
            | Failure(x, _, _) -> failwithf "%A" x
        )
    // parse 2 true
type VarType =
    | Any
    | String
    | Numeric

// type X () =
//     member __.F(x:string, [<System.ParamArray>] args: string[]) =
//         printfn "first"
//         for arg in args do
//             printfn "%A" arg
//     member __.F(x:string, y:string, [<System.ParamArray>] args: string[]) =
//         printfn "second"
//         for arg in args do
//             printfn "%A" arg
// let x = X()
// x.F("1")
// x.F("1", y = "2")
// x.F("1", y = "2", "3")

type 'Func OverloadType =
    | JustOverloads of (VarType [] * 'Func) list
    /// Если говорить родным F#, то это:
    /// ```fsharp
    /// member __.F(x:Type1, y:Type2, [&lt;System.ParamArray>] args: Type3 []) =
    /// ```
    /// выражается так:
    /// ```fsharp
    /// {| Requireds: [ Type1; Type2 ]; ArgList: Type3 |}
    /// ```
    | ParamArrayOverload of {| Requireds: VarType []; ArgList: VarType |} * 'Func
module Show =
    open FsharpMyExtension.ShowList
    let showVarType = function
        | Any -> showString "any"
        | String -> showString "string"
        | Numeric -> showString "numeric"
    let showParamArray typ =
        // На Lua:
        // ```lua
        // function f(...)
        //     -- ...
        // end
        // ```
        // В Python:
        // ```python
        // def my_function(*argName):
        //     # ...
        // ```
        // Но это то, как оно объявляется, а сигнатуру-то как написать?
        // showChar '*' << showChar ':' << showVarType typ
        showVarType typ << showSpace << showString "[]"
    let showArgs varTypes =
        varTypes
        |> Array.map showVarType
        |> List.ofArray
        |> joins (showChar ',' << showSpace)
    let printSignature (name:string) =
        function
        | JustOverloads xs ->
            xs
            |> List.map (fun (varTypes, _) ->
                showString name << showParen true (showArgs varTypes)
            )
            |> lines
        | ParamArrayOverload(x, _) ->
            showString name
            << showParen true
                (showArgs x.Requireds << showChar ',' << showSpace
                 << showParamArray x.ArgList)
        >> show
    let printFuncSignature (name:string) (returnType:VarType) =
        function
        | JustOverloads xs ->
            xs
            |> List.map (fun (varTypes, _) ->
                showString name << showParen true (showArgs varTypes)
                << showChar ':' << showSpace << showVarType returnType
            )
            |> lines
        | ParamArrayOverload(x, _) ->
            showString name
            << showParen true
                (showArgs x.Requireds << showChar ',' << showSpace
                 << showParamArray x.ArgList)
            << showChar ':' << showSpace << showVarType returnType
        >> show

let getFuncByOverloadType overloadType (inputArgs: _ []) =
    let inputArgsLength = Array.length inputArgs
    match overloadType with
    | JustOverloads os ->
        os
        |> List.tryPick (fun (x, func) ->
            if x.Length = inputArgsLength then
                Some func
            else None
        )
    | ParamArrayOverload(x, func) ->
        if inputArgsLength < x.Requireds.Length then
            None
        else
            Some func

let arg x =
    [[|x|] , ()] |> JustOverloads

let unit' =
    [[||] , ()] |> JustOverloads

let args xs =
    [xs |> Array.ofList, ()] |> JustOverloads

let argsAndOptional xs opt =
    [
        xs |> Array.ofList, ()
        xs @ [opt] |> Array.ofList, ()
    ] |> JustOverloads

let argList typ =
    ({| Requireds = [||]; ArgList = typ |}, ())
    |> ParamArrayOverload

let argAndArgList x typ =
    ({| Requireds = [|x|]; ArgList = typ |}, ())
    |> ParamArrayOverload
type VarName = string
type Description = string

let proceduresWithAsterix =
    [
        let dscr =
            [
                "`*CLEAR` или `*CLR` - очистка основного окна описаний."
            ] |> String.concat "\n"
        ("*clear":VarName), (dscr:Description), unit'
        let dscr =
            [
                "`*CLEAR` или `*CLR` - очистка основного окна описаний."
            ] |> String.concat "\n"
        "*clr", dscr, unit'
        let dscr =
            [
                "`*NL [выражение]` - переход на новую строку, затем вывод текста в основном окне описаний. Если `[выражение]` не указано, то перевод строки. Отличается от *PL порядком вывода текста."
            ] |> String.concat "\n"
        let funcs =
            [
                [| String |], ()
                [| |], ()
            ] |> JustOverloads
        "*nl", dscr, funcs
        let dscr =
            [
                "`*P [выражение]` - вывод текста в основное окно описаний (по умолчанию находится слева сверху и не может быть отключено)."
            ]|> String.concat "\n"
        "*p", dscr, arg Any
        let dscr =
            [
                "`*PL [выражение]` - вывод текста, затем переход на новую строку в основном окне описаний. Если `[выражение]` не указано, то перевод строки. Аналогичным образом можно вывести текст, просто написав нужное выражение вместо данного оператора. Например, строки:"
                "```qsp"
                "*PL $AAA+'989'"
                "*PL 'Вы находитесь в парке'"
                "*PL 'Преформатированная"
                "строка'"
                "```"
                "и"
                "```qsp"
                "$AAA+'989'"
                "'Вы находитесь в парке'"
                "'Преформатированная"
                "строка'"
                "```"
                "выполнят одно и то же действие."
            ] |> String.concat "\n"
        "*pl", dscr, arg Any
    ]
    |> List.map (fun (name, dscr, sign) -> name, (dscr, sign))
    |> Map.ofList

/// Заданные переменные
let vars =
    [
        let dscr =
            [
                "содержит путь к файлу изображения локации. Изображение локации показывается в том случае, если значение данной переменной отлично от '' (не пустая строка) и файл изображения удалось загрузить."
            ] |> String.concat "\n"
        ("$backimage":VarName), (dscr:Description)
        let dscr =
            [
                "Название выделенного действия."
            ] |> String.concat "\n"
        "$selact", dscr
        let dscr =
            [
                "содержит название локации-счётчика. Локация-счётчик полезна для проверки выделенных предметов, введённого текста..."
            ] |> String.concat "\n"
        "$counter", dscr
        let dscr =
            [
                "текущие действия в виде текста. Сохранив значение в переменной, восстановить действия можно в любой момент игры с помощью оператора `DYNAMIC`."
            ] |> String.concat "\n"
        "$curacts", dscr
        let dscr =
            [
                "текст, находящийся в основном окне описаний. Также есть функция `maintxt`"
            ] |> String.concat "\n"
        "$maintxt", dscr
        let dscr =
            [
                "содержит название используемого в данный момент шрифта. Если равна '' (пустая строка), то используется шрифт, заданный в настройках программы."
            ] |> String.concat "\n"
        "$fname", dscr
        let dscr =
            [
                "содержит название локации-обработчика выбора действия. Данная локация полезна, к примеру, для вывода изображений или проигрывания звуков при выборе действий. Получить название выбранного действия можно через функцию `SELACT`."
            ] |> String.concat "\n"
        "$onactsel", dscr
        let dscr =
            [
                "содержит название локации-обработчика загрузки состояния. Данная локация полезна для выполнения каких-либо действий после загрузки состояния игры."
            ] |> String.concat "\n"
        "$ongload", dscr
        let dscr =
            [
                "содержит название локации-обработчика сохранения состояния. Данная локация полезна для выполнения каких-либо действий перед сохранением состояния игры."
            ] |> String.concat "\n"
        "$ongsave", dscr
        let dscr =
            [
                "содержит название локации-обработчика перехода на новую локацию (аналог локации \"common\" в URQ). Может заменить часть функций локации-счётчика. Получить название локации, на которую был осуществлён переход, можно с помощью функции `CURLOC`."
            ] |> String.concat "\n"
        "$onnewloc", dscr
        let dscr =
            [
                "содержит название локации-обработчика добавления предмета. При добавлении предмета локация вызывается с аргументом `$ARGS[0]` - названием добавленного предмета. Данная локация полезна, к примеру, для ограничения вместительности рюкзака."
            ] |> String.concat "\n"
        "$onobjadd", dscr
        let dscr =
            [
                "содержит название локации-обработчика удаления предмета. При удалении предмета локация вызывается с аргументом `$ARGS[0]` - названием удалённого предмета. Данная локация полезна, к примеру, для проверки возможности удаления предмета."
            ] |> String.concat "\n"
        "$onobjdel", dscr
        let dscr =
            [
                "содержит название локации-обработчика выбора предмета. Данная локация полезна, к примеру, для вывода меню предметов. Получить название выбранного предмета можно через функцию `SELOBJ`."
            ] |> String.concat "\n"
        "$onobjsel", dscr
        let dscr =
            [
                "текст, находящийся в окне пользователя. Также есть функция `stattxt`"
            ] |> String.concat "\n"
        "$stattxt", dscr
        let dscr =
            [
                "содержит название локации-обработчика строки ввода. Полезна при организации парсера (управление игрой с помощью строки ввода). Текущий текст строки ввода возвращает функция `USER_TEXT`."
            ] |> String.concat "\n"
        "$usercom", dscr
        let dscr =
            [
                "название текущей локации, также можно использовать `curloc`"
            ] |> String.concat "\n"
        "$curloc", dscr
        let dscr =
            [
                "содержит размер используемого в данный момент шрифта. Если равна 0, то используется размер, заданный в настройках программы. Относительно данного значения в HTML-режиме вычисляются размеры шрифтов тега \"FONT\"."
            ] |> String.concat "\n"
        "fsize", dscr
        let dscr =
            [
                "содержит цвет текущего фона. Если равна 0, то используется цвет, заданный в настройках программы."
            ] |> String.concat "\n"
        "bcolor", dscr
        let dscr =
            [
                "содержит цвет используемого в данный момент шрифта. Если равна 0, то используется цвет, заданный в настройках программы."
            ] |> String.concat "\n"
        "fcolor", dscr
        let dscr =
            [
                "содержит текущий цвет ссылок. Если равна 0, то используется цвет, заданный в настройках программы."
            ] |> String.concat "\n"
        "lcolor", dscr
        let dscr =
            [
                "если отлична от 0, включает возможность использования HTML в описании локации, в дополнительном описании, в списках действий и предметов, а также в диалоге ввода текста, вызываемого функцией `INPUT`. Выводимый текст распознаётся как HTML. Список поддерживаемых тегов и их атрибутов смотрите в приложении."
            ] |> String.concat "\n"
        "usehtml", dscr
        let dscr =
            [
                "Массив, который содержит аргументы текущей локации. Подробнее расписано в процедуре `GOSUB` и ей подобной."
            ] |> String.concat "\n"
        "args", dscr
        "#args", dscr
        "$args", dscr
        let dscr =
            "Если текущая локация вызвана с помощью `FUNC` или создана динамически с помощью `DYNEVAL`, то по итогу вернется это значение. Ах да, эта переменная бывает нескольких типов: `result`, `#result` и `$result`. И возникает вопрос: а что, собственно, вернет, скажем, вызов `DYNEVAL(\"$result = 'string' & #result = 1\")`? Что ж, сработает только `$x = DYNEVAL(\"$result = 'string' & #result = 1\") & $x`, а `#x = DYNEVAL(\"$result = 'string' & #result = 1\") & #x` — выбьет ошибку про несоответствие данных, что довольно странно."
        "result", dscr
        "$result", dscr
        "#result", dscr
        let dscr =
            [
                "если значение переменной не равно 0, то отключается проверка идентификатора игры при загрузке состояния. Полезно для отладки."
            ] |> String.concat "\n"
        "debug", dscr
        let dscr =
            [
                "если значение переменной не равно 0, то запрещает автопрокрутку текста при его выводе в основное или дополнительное окно описания локации."
            ] |> String.concat "\n"
        "disablescroll", dscr
        let dscr =
            [
                "если значение переменной не равно 0, то запрещает использование \"подвыражений\" в тексте (например, значением `'<<5+6>>'` будет строка `'<<5+6>>'`, а не `'11'`)."
            ] |> String.concat "\n"
        "disablesubex", dscr
        let dscr =
            [
                "Если её значение отлично от 0, то сохранение состояния игры пользователем невозможно."
            ] |> String.concat "\n"
        "nosave", dscr
    ]
    |> Map.ofList
[<Struct>]
type PredefFunc =
    | Arrcomp
    | Arrpos
    | Arrsize
    | Countobj
    | Curacts
    | Curloc
    | Desc
    | Dyneval
    | Func
    | Getobj
    | Iif
    | Input
    | Instr
    | Isnum
    | Isplay
    | Lcase
    | Len
    | Maintxt
    | Max
    | Mid
    | Min
    | Msecscount
    | Qspver
    | Rand
    | Replace
    | Rgb
    | Rnd
    | Selact
    | Selobj
    | Stattxt
    | Str
    | Strcomp
    | Strfind
    | Strpos
    | Trim
    | Ucase
    | User_text
    | Usrtxt
    | Val
let functions =
    let arg x (return':VarType) = arg x, return'
    let unit' (return':VarType) = unit', return'
    let args xs (return':VarType) = args xs, return'
    let argList typ (return':VarType) = argList typ, return'
    let argAndArgList x typ (return':VarType) = argAndArgList x typ, return'
    [
        let dscr =
            [
                "`RAND([#выражение 1],[#выражение 2])` - возвращает случайное число между числами `[#выражение 1]` и `[#выражение 2]`. Параметр `[#выражение 2]` может отсутствовать, при этом он принимается равным 0."
            ] |> String.concat "\n"
        let os =
            [
                [| Numeric; Numeric |], ()
                [| Numeric |], ()
            ] |> JustOverloads
        Rand, dscr, (os, Numeric)
        let dscr =
            [
                "возвращает название текущей локации, также можно использовать переменную `$curloc`"
            ] |> String.concat "\n"
        Curloc, dscr, unit' String
        let dscr =
            [
                "возвращает случайное значение от 1 до 1000."
            ] |> String.concat "\n"
        Rnd, dscr, unit' Numeric
        let dscr =
            [
                "`INPUT([$выражение])` - выводит окно ввода с приглашением [$выражение]. Возвращает введённый играющим текст, либо '' (пустая строка), если была нажата кнопка \"Отмена\"."
            ] |> String.concat "\n"
        Input, dscr, arg String String
        let dscr =
            [
                "возвращает текст, находящийся в строке ввода. Синоним `usrtxt`"
            ] |> String.concat "\n"
        User_text, dscr, unit' String
        let dscr =
            [
                "возвращает текст, находящийся в строке ввода. Синоним `user_text`"
            ] |> String.concat "\n"
        Usrtxt, dscr, unit' String
        let dscr =
            [
                "`MAX([выражение 1],[выражение 2], ...)` - возвращает максимальное из значений выражений-аргументов. Если передан один аргумент, то считается, что указано имя массива - в этом случае поиск максимального элемента происходит среди строковых (если название массива указано со знаком \"$\") или среди числовых значений элементов массива. Например:"
                "```qsp"
                "MAX(1,2,5,2,0) & ! вернёт 5"
                "MAX(a,b,c) & ! вернёт максимальное из значений переменных"
                "MAX('aa','ab','zz') & ! вернёт 'zz'"
                "MAX('a') & ! вернёт максимальное из числовых значений элементов массива \"A\""
                "MAX('$b') & ! вернёт максимальное из строковых значений элементов массива \"B\""
                "```"
            ] |> String.concat "\n"
        Max, dscr, argList Any Any
        let dscr =
            [
                "`MIN([выражение 1],[выражение 2], ...)` - возвращает минимальное из значений выражений-аргументов. Если передан один аргумент, то считается, что указано имя массива - в этом случае поиск минимального элемента происходит среди строковых (если название массива указано со знаком \"$\") или среди числовых значений элементов массива."
            ] |> String.concat "\n"
        Min, dscr, argList Any Any
        let dscr =
            [
                "`IIF([#выражение],[выражение_да],[выражение_нет])` - возвращает значение выражения [выражение_да], если [#выражение] верно, иначе значение выражения [выражение_нет]."
            ] |> String.concat "\n"
        Iif, dscr, args [Numeric; Any; Any] Any
        let dscr =
            [
                "`RGB([#выражение 1],[#выражение 2],[#выражение 3])` - возвращает код цвета на основе 3-х числовых аргументов. [#выражение 1], [#выражение 2] и [#выражение 3] определяют соответственно уровни красного, зелёного и синего цветов. Все значения аргументов должны быть в отрезке [0, 255]. Данная функция используется совместно с системными переменными `BCOLOR`, `FCOLOR` и `LCOLOR`."
            ] |> String.concat "\n"
        Rgb, dscr, args [Numeric; Numeric; Numeric] Numeric
        let dscr =
            [
                "`ISPLAY([$выражение])` - проверяет, проигрывается ли файл с заданным названием в текущий момент времени и возвращает -1, если файл воспроизводится, иначе 0."
            ] |> String.concat "\n"
        Isplay, dscr, arg String Numeric
        let dscr =
            [
                "возвращает количество миллисекунд, прошедших с момента начала игры."
            ] |> String.concat "\n"
        Msecscount, dscr, unit' Numeric
        let dscr =
            [
                "`DESC([$выражение])` - возвращает текст базового описания локации с заданным в [$выражение] названием."
            ] |> String.concat "\n"
        Desc, dscr, arg String String
        let dscr =
            [
                "возвращает текст, находящийся в основном окне описаний. Также есть переменная `$maintxt`"
            ] |> String.concat "\n"
        Maintxt, dscr, unit' String
        let dscr =
            [
                "возвращает текст, находящийся в окне пользователя. Также есть переменная `$stattxt`"
            ] |> String.concat "\n"
        Stattxt, dscr, unit' String
        let dscr =
            [
                "возвращает версию интерпретатора в формате \"X.Y.Z\""
            ] |> String.concat "\n"
        Qspver, dscr, unit' String
        let dscr =
            [
                "`FUNC([$выражение],[параметр 1],[параметр 2], ...)` - обработка локации с названием `[$выражение]`. Указанные параметры передаются в массиве `ARGS`. Результат функции равен значению `$RESULT` при возврате строкового значения или `RESULT` при возврате числового значения. Если при обработке локации были установлены и `RESULT`, и `$RESULT`, то предпочтение отдаётся строковому значению. После обработки локации предыдущие значения `ARGS` и `RESULT` восстанавливаются. Примеры:"
                "```qsp"
                "PL 4 + FUNC('функция') & ! обработка локации \"функция\" как функции. Массив ARGS пуст. Результат передается через `$RESULT` или `RESULT`, в зависимости от кода обрабатываемой локации."
                "PL FUNC($name, 1) * 78 & ! обработка локации с названием в $name как функции. `ARGS[0]` равен 1."
                "MSG \"text\" + FUNC($name, \"строка\", 2) & ! обработка локации с названием в $name как функции. `$ARGS[0]` содержит строку \"строка\", `ARGS[1]` равен 2."
                "```"
                "Также можно использовать `$func`:"
                "```qsp"
                "# start"
                "$func('toString', 1) & ! -> '1'"
                "-"
                ""
                "# toString"
                "$result = ARGS[0]"
                "-"
                "```"
            ] |> String.concat "\n"
        Func, dscr, argList Any Any
        let dscr =
            [
                "`DYNEVAL([$выражение],[параметр 1],[параметр 2], ...)` - возвращает значение указанного выражения. Функция позволяет вычислять значения динамически сгенерированных выражений. Указанные параметры передаются в массиве `ARGS`, а после вычисления выражения предыдущие значения `ARGS` восстанавливаются. Примеры:"
                "```qsp"
                "DYNEVAL('3+4')"
                "PL DYNEVAL('mid(\"abcd\",2,1)+\"qwerty\"')"
                "PL DYNEVAL($test + ' + val(\"<<$test>>\")')"
                "DYNEVAL(\" $args[0] <> 'текст' \", 'строка')"
                "$x = DYNEVAL(\"$result = 'текст'\") & $x"
                "```"
            ] |> String.concat "\n"
        Dyneval, dscr, argAndArgList String Any Any
        let dscr =
            [
                "возвращает количество предметов в рюкзаке."
            ] |> String.concat "\n"
        Countobj, dscr, unit' Numeric
        let dscr =
            [
                "возвращает название выделенного предмета."
            ] |> String.concat "\n"
        Selobj, dscr, unit' String
        let dscr =
            [
                "`GETOBJ([#выражение])` - возвращает название предмета в рюкзаке, расположенного в заданной позиции. Индексация предметов рюкзака ведётся с 1."
                "Если предмета с заданным индексом не существует, возвращается пустая строка ('')."
                ""
                "Примеры:"
                "```qsp"
                "GETOBJ(1) & ! вернёт название первого предмета в рюкзаке"
                "GETOBJ(COUNTOBJ) &! вернёт название последнего добавленного предмета"
                "```"
                ""
                "Код, подсчитывающий в массиве `OBJECTS` число предметов с одинаковым названием:"
                "```qsp"
                "i = 1"
                ":loop"
                "IF i <= COUNTOBJ:"
                "    OBJECTS[$GETOBJ(i)] = OBJECTS[$GETOBJ(i)] + 1"
                "    i = i + 1"
                "    JUMP 'loop'"
                "END"
                "```"
            ] |> String.concat "\n"
        Getobj, dscr, arg Numeric String
        let dscr =
            [
                "`ARRCOMP([#выражение 1],[$выражение 2],[$выражение 3])` - возвращает индекс элемента массива с названием `[$выражение 2]`, соответствующего регулярному выражению `[$выражение 3]`. Поиск начинается с элемента номер `[#выражение 1]`; индексация элементов массива ведётся с нуля. Параметр `[#выражение 1]` может отсутствовать, при этом он принимается равным 0. Если элемент не найден, функция возвращает -1."
                "Поиск происходит среди текстовых элементов массива. Примеры:"
                "```qsp"
                "ARRCOMP(0,'A','This') & ! найдёт строку 'This' среди текстовых элементов массива \"A\" (или вернёт -1, если такого значения не существует)"
                "ARRCOMP(2,'A','abc\\d+') & ! найдёт строку, соответствующую регулярному выражению \"abc\\d+\", в текстовых значениях массива \"A\" (первые два элемента массива игнорируются)"
                "ARRCOMP(0,'A','.*string.*') & ! аналогично предыдущему примеру, но поиск осуществляется по всем текстовым элементам массива"
                "ARRCOMP('A','This') & ! эквивалентно 1-му варианту"
                "```"
            ] |> String.concat "\n"
        let funcs =
            [
                [| Numeric; String; String |], ()
                [| String; String |], ()
            ] |> JustOverloads
        Arrcomp, dscr, (funcs, String)
        let dscr =
            [
                "`STRCOMP([$выражение],[$шаблон])` - проводит сравнение строки `[$выражение]` на соответствие регулярному выражению `[$шаблон]`. Возвращает -1, если строка соответствует шаблону, иначе 0. Сравни с функцией `STRFIND`."
            ] |> String.concat "\n"
        Strcomp, dscr, args [String; String] Numeric
        let dscr =
            [
                "`STRFIND([$выражение],[$шаблон],[#номер])` - возвращает подстроку в строке `[$выражение]`, соответствующую группе с номером `[#номер]` регулярного выражения `[$шаблон]`. Если подстрока с указанным номером отсутствует, то возвращается пустая строка. Нумерация групп подстрок начинается с 1. Если параметр `[#номер]` отсутствует или равен 0, то возвращается подстрока, соответствующая всему регулярному выражению `[$шаблон]`. Примеры:"
                "```qsp"
                "STRFIND(' идти к пещере', '^(\\S+)\\s(\\S+)\\s(\\S+)$', 0) & ! -> ''"
                "STRFIND('идти к пещере', '^(\\S+)\\s(\\S+)\\s(\\S+)$', 1) & ! -> 'идти'"
                "STRFIND('идти к пещере', '^(\\S+)\\s(\\S+)\\s(\\S+)$', 2) & ! -> 'к'"
                "STRFIND('идти к пещере', '^(\\S+)\\s(\\S+)\\s(\\S+)$', 3) & ! -> 'пещере'"
                "STRFIND('идти к пещере', '^(\\S+)\\s(\\S+)(\\s(\\S+))?$', 4) & ! -> 'пещере'"
                "STRFIND('искать ключ', '^(\\S+)\\s(\\S+)(\\s(\\S+))?$', 1) & ! -> 'искать'"
                "STRFIND('искать', '^(\\S+)\\s(\\S+)(\\s(\\S+))?$', 0) & ! -> ''"
                "STRFIND('искать', '^(\\S+)\\s(\\S+)(\\s(\\S+))?$', 1) & ! -> ''"
                "STRFIND('искать', '^(\\S+)(\\s(\\S+)(\\s(\\S+))?)?$', 1) & ! -> 'искать'"
                "STRFIND('идти к дому', 'к\\s(\\S+)', 0) & ! -> 'к дому'"
                "STRFIND('идти к дому', 'к\\s(\\S+)') & ! -> 'к дому'"
                "STRFIND('идти к дому', 'к\\s(\\S+)', 1) & ! -> 'дому'"
                "STRFIND('идти к своему дому', 'к\\s(\\S+)', 1) & ! -> 'своему'"
                "```"
            ] |> String.concat "\n"
        Strfind, dscr, (argsAndOptional [ String; String ] Numeric, String)
        let dscr =
            [
                "`STRPOS([$выражение],[$шаблон],[#номер])` - возвращает позицию символа, с которого начинается вхождение подстроки в строке `[$выражение]`, соответствующей группе с номером `[#номер]` регулярного выражения `[$шаблон]`. Если подстрока с указанным номером отсутствует, то возвращается 0. Нумерация групп подстрок начинается с 1. Если параметр `[#номер]` отсутствует или равен 0, то возвращается позиция символа, с которого начинается вхождение подстроки, соответствующей всему регулярному выражению `[$шаблон]`."
                "Примеры:"
                "```qsp"
                "STRPOS(' идти к пещере', '^(\\S+)\\s(\\S+)\\s(\\S+)$', 0) & ! -> 0"
                "STRPOS('идти к пещере', '^(\\S+)\\s(\\S+)\\s(\\S+)$', 1) & ! -> 1"
                "STRPOS('идти к пещере', '^(\\S+)\\s(\\S+)\\s(\\S+)$', 2) & ! -> 6"
                "STRPOS('идти к пещере', '^(\\S+)\\s(\\S+)\\s(\\S+)$', 3) & ! -> 8"
                "STRPOS('идти к пещере', '^(\\S+)\\s(\\S+)(\\s(\\S+))?$', 4) & ! -> 8"
                "STRPOS('искать ключ', '^(\\S+)\\s(\\S+)(\\s(\\S+))?$', 1) & ! -> 1"
                "STRPOS('искать', '^(\\S+)\\s(\\S+)(\\s(\\S+))?$', 0) & ! -> 0"
                "STRPOS('искать', '^(\\S+)\\s(\\S+)(\\s(\\S+))?$', 1) & ! -> 0"
                "STRPOS('искать', '^(\\S+)(\\s(\\S+)(\\s(\\S+))?)?$', 1) & ! -> 1"
                "STRPOS('идти к дому', 'к\\s(\\S+)', 0) & ! -> 6"
                "STRPOS('идти к дому', 'к\\s(\\S+)') & ! -> 6"
                "STRPOS('идти к дому', 'к\\s(\\S+)', 1) & ! -> 8"
                "STRPOS('идти к своему дому', 'к\\s(\\S+)', 1) & ! -> 8"
                "```"
            ] |> String.concat "\n"
        Strpos, dscr, (argsAndOptional [ String; String ] Numeric, Numeric)
        let dscr =
            [
                "возвращает текущие действия в виде текста. Сохранив значение в переменной, восстановить действия можно в любой момент игры с помощью оператора `DYNAMIC`."
            ] |> String.concat "\n"
        Curacts, dscr, unit' String
        let dscr =
            [
                "`ARRPOS([#выражение 1],[$выражение 2],[выражение 3])` - возвращает индекс элемента массива с названием `[$выражение 2]`, равного значению выражения `[выражение 3]`. Поиск начинается с элемента номер `[#выражение 1]`; индексация элементов массива ведётся с нуля. Параметр [#выражение 1] может отсутствовать, при этом он принимается равным 0. Если указанное значение не найдено, функция возвращает -1."
                "Чтобы найти числовое значение в массиве, нужно в `[$выражение 2]` подставить лишь название массива (в кавычках), для строкового - название массива с символом \"$\" перед его названием. Примеры:"
                "`ARRPOS(0,'$A','This')` - найдёт строку 'This' в текстовых значениях массива \"A\" (или вернёт -1, если такого значения не существует)"
                "`ARRPOS(2,'A',65)` - найдёт число 65 в числовых значениях массива \"A\" (при этом первые два элемента массива игнорируются)"
                "`ARRPOS('$B','test')` - поиск строки 'test' среди текстовых значений массива \"B\""
            ] |> String.concat "\n"
        let funcs =
            [
                [| Numeric; String; Any |], ()
                [| String; Any |], ()
            ] |> JustOverloads
        Arrpos, dscr, (funcs, Numeric)
        let dscr =
            [
                "`ARRSIZE([$выражение])` - возвращает число элементов в массиве с названием `[$выражение]`."
            ] |> String.concat "\n"
        Arrsize, dscr, arg String Numeric
        let dscr =
            [
                "`INSTR([#выражение 1],[$выражение 2],[$выражение 3])` - возвращает номер позиции символа, с которого начинается вхождение строки `[$выражение 3]` в строку `[$выражение 2]` (или 0, если такой строки нет). Поиск начинается с символа номер `[#выражение 1]`. Параметр `[#выражение 1]` может отсутствовать, при этом он принимается равным 1. Примеры:"
                "`INSTR(1,'ABCDefgh','BC')` равно `2`"
                "`INSTR(1,'ABCDefgh','Be')` равно `0`"
                "`INSTR('abcdef','abc')` равно `1`"
            ] |> String.concat "\n"
        let funcs =
            [
                [| Numeric; String; String |], ()
                [| String; String |], ()
            ] |> JustOverloads
        Instr, dscr, (funcs, Numeric)
        let dscr =
            [
                "`ISNUM([$выражение])` - функция проверяет, все ли символы в строке являются цифрами (учитывая знак \"-\" в начале, прилегающие пробелы и символы табуляции). Если в указанной строке есть хотя бы один символ - не-цифра (исключая возможный \"-\" в начале и прилегающие пробелы / символы табуляции), то функция возвращает 0 (ложь), иначе -1 (истина)."
                "Функция полезна при проверке введённой играющим строки на число. Примеры:"
                "`ISNUM('9999 ')` равно `-1`"
                "`ISNUM(' -888')` равно `-1`"
                "`ISNUM('777a6')` равно `0`"
            ] |> String.concat "\n"
        Isnum, dscr, arg String Numeric
        let dscr =
            [
                "`LCASE([$выражение])` - возвращает строку маленьких букв, полученную изменением регистра букв исходной строки `[$выражение]`. Пример:"
                "```qsp"
                "LCASE('TExT#') & ! 'text#'"
                "```"
            ] |> String.concat "\n"
        Lcase, dscr, arg String String
        let dscr =
            [
                "`LEN([$выражение])` - возвращает длину строки `[$выражение]`."
            ] |> String.concat "\n"
        Len, dscr, arg String Numeric
        let dscr =
            [
                "`MID([$выражение],[#выражение 1],[#выражение 2])` - вырезает из строки `[$выражение]` строку, которая начинается с символа номер `[#выражение 1]` и имеет длину `[#выражение 2]`. Индексация символов в строке ведётся с 1."
                "Параметр `[#выражение 2]` может отсутствовать, при этом вырезается вся строка, начиная с символа [#выражение 1]. Примеры:"
                "```qsp"
                "MID('abcd', 1, 2) равно 'ab'"
                "MID('abcd', 2, 3) равно 'bcd'"
                "MID('abcd', 2) равно 'bcd'"
                "```"
            ] |> String.concat "\n"
        Mid, dscr, (argsAndOptional [ String; Numeric ] Numeric, String)
        let dscr =
            [
                "`REPLACE([$выражение 1],[$выражение 2],[$выражение 3])` - заменяет в строке [$выражение 1] все вхождения строки [$выражение 2] строкой [$выражение 3]. Если [$выражение 3] отсутствует или указана пустая строка, то удаляет в исходной строке все вхождения искомой строки. Примеры:"
                "`REPLACE('test', '12', '4')` равно `'test'`"
                "`REPLACE('test', 'e', 's')` равно `'tsst'`"
                "`REPLACE('test', 't', '34')` равно `'34es34'`"
                "`REPLACE('test', 't')` равно `'es'`"
            ] |> String.concat "\n"
        Replace, dscr, (argsAndOptional [ String; String ] String, String)
        let dscr =
            [
                "`STR([#выражение])` - переводит число (числовое выражение) `[#выражение]` в соответствующую строку. Например,"
                "`PL STR(56)` выведет строку `56`."
            ] |> String.concat "\n"
        Str, dscr, arg Numeric String
        let dscr =
            [
                "`TRIM([$выражение])` - удаляет прилегающие пробелы и символы табуляции из `[$выражение]`. Затем возвращает полученную строку. Пример:"
                "`TRIM('     TRIM TEST        ')` равно `'TRIM TEST'`"
            ] |> String.concat "\n"
        Trim, dscr, arg String String
        let dscr =
            [
                "`UCASE([$выражение])` - возвращает строку больших букв, полученную изменением регистра букв исходной строки `[$выражение]`. Пример:"
                "```qsp"
                "UCASE('TexT#') & ! -> 'TEXT#'"
                "```"
            ] |> String.concat "\n"
        Ucase, dscr, arg String String
        let dscr =
            [
                "`VAL([$выражение])` - переводит строку цифр `[$выражение]` в соответствующее число. Если [$выражение] равно '' (пустая строка) или если оно содержит не-цифры, то возвращается 0."
            ] |> String.concat "\n"
        Val, dscr, arg String Numeric
        let dscr =
            [
                "возвращает название выделенного действия."
            ] |> String.concat "\n"
        Selact, dscr, unit' String
    ]
    |> List.map (fun (name, dscr, sign) ->
        let stringName = (string name).ToLower()
        stringName, {| SymbolicName = name; Description = dscr; Signature = sign |})
    |> Map.ofList
/// Да-да, всё это — процедуры: они что-то выполняют и никуда не перемещают.
let procedures =
    [
        let dscr =
            [
                "`OPENGAME [$выражение]` - если `[$выражение]` равно '' (пустая строка) или отсутствует, то вызов окна загрузки состояния игры, иначе загрузка состояния из указанного файла. См. также локацию-обработчик события загрузки игры."
            ] |> String.concat "\n"
        let os =
            [
                [||], ()
                [| String |], ()
            ] |> JustOverloads
        "opengame", dscr, os
        let dscr =
            [
                "`CMDCLEAR` или `CMDCLR` - очистка строки ввода."
            ] |> String.concat "\n"
        "cmdclear", dscr, unit'
        let dscr =
            [
                "`CMDCLEAR` или `CMDCLR` - очистка строки ввода."
            ] |> String.concat "\n"
        "cmdclr", dscr, unit'
        let dscr =
            [
                "`SAVEGAME [$выражение]` - если `[$выражение]` равно '' (пустая строка) или отсутствует, то вызов окна сохранения состояния игры, иначе сохранение состояния в указанный файл. См. также локацию-обработчик события сохранения игры."
            ] |> String.concat "\n"
        let os =
            [
                [||], ()
                [| String |], ()
            ] |> JustOverloads
        "savegame", dscr, os
        let dscr =
            [
                "`OPENQST [$выражение]` - открытие и запуск заданного файла игры. При использовании данного оператора, не происходит удаления переменных, удаления предметов инвентаря, очистки дополнительного описания и строки ввода, а также остановки проигрываемых файлов (для этого в начале загружаемой игры можно выполнить \"KILLALL & CLS & CLOSE ALL\")."
            ] |> String.concat "\n"
        "openqst", dscr, arg String
        let dscr =
            [
                "`ADDQST [$выражение]` - из заданного файла игры добавляет все локации, названия которых отсутствуют среди текущих игровых локаций. Загруженные локации полностью эквивалентны локациям из основного файла игры."
            ] |> String.concat "\n"
        "addqst", dscr, arg String
        let dscr =
            "Синоним `addqst`"
        "addlib", dscr, arg String
        let dscr =
            "Синоним `addqst`"
        "inclib", dscr, arg String
        let dscr =
            [
                "KILLQST - удаляет все локации, добавленные с помощью оператора `ADDQST`"
            ] |> String.concat "\n"
        "killqst", dscr, unit'
        "freelib", dscr, unit'
        "dellib", dscr, unit'
        let dscr =
            [
                "`DELACT [$название]` или `DEL ACT [$название]` - удаляет действие из списка действий на локации (если такое действие существует). Например:"
                "```qsp"
                "DELACT 'Идти вперед'"
                "DELACT $SELACT"
                "```"
            ] |> String.concat "\n"
        "delact", dscr, arg String
        let dscr =
            [
                "CLA - очистка списка текущих действий."
            ] |> String.concat "\n"
        "cla", dscr, unit'
        let dscr =
            [
                "`ADDOBJ [$название],[$путь к файлу изображения]` или `ADD OBJ [$название],[$путь к файлу изображения]` - добавление предмета с заданным изображением в рюкзак."
                "К предметам добавляется новый с названием `[$название]` и изображением `[$путь к файлу изображения]`."
                ""
                "Параметр `[$путь к файлу изображения]` может отсутствовать, при этом предмет добавится без изображения."
                ""
                "Обратите внимание - для использования одинаковых предметов инвентаря, например денег, патронов и т.п., лучше использовать дополнительную переменную, обозначающую количество этих предметов, чтобы не загромождать инвентарь списком из 137 предметов Рубль / Патрон. Для хранения числа предметов можно использовать массивы, индексируемые через строки:"
                ""
                "```qsp"
                "OBJECTS['деньги'] = 12"
                "OBJECTS['патроны'] = 137"
                "'Количество: <<OBJECTS[$getobj(countobj)]>>'"
                "```"
            ] |> String.concat "\n"
        "addobj", dscr, argsAndOptional [String] String
        let dscr =
            [
                "`DELOBJ [$название]` или `DEL OBJ [$название]` - удаление предмета из рюкзака, если таковой имеется. Также см. локацию-обработчик удаления предмета."
            ] |> String.concat "\n"
        "delobj", dscr, arg String
        let dscr =
            [
                "`KILLOBJ [#выражение]` - удаление предмета, расположенного в заданной позиции. Если параметр [#выражение] не указан, то очистка рюкзака."
                "Индексация предметов рюкзака ведётся с 1. Также см. локацию-обработчик удаления предмета."
            ] |> String.concat "\n"
        let os =
            [
                [||], ()
                [| Numeric |], ()
            ] |> JustOverloads
        "killobj", dscr, os
        let dscr =
            [
                "`UNSELECT` или `UNSEL` - отмена выбора предмета. При выборе играющим какого-либо предмета, он остаётся выделенным. Данная команда снимает выделение."
            ] |> String.concat "\n"
        "unsel", dscr, unit'
        "unselect", dscr, unit'
        let dscr =
            [
                "KILLALL - эквивалентен конструкции `KILLVAR & KILLOBJ`."
            ] |> String.concat "\n"
        "killall", dscr, unit'
        let dscr =
            [
                "`KILLVAR [$название массива],[#индекс элемента]` - удаление элемента массива. Если индекс элемента не указан, то очищается весь массив. Если оператор вызван без аргументов, то удаляются все переменные - обычно применяется в начале игры, чтобы при возврате в начальную локацию после неудачного прохождения какого-то этапа игры обнулить все переменные (в противном случае, может оказаться, что запертые двери уже открыты, жена похищена до свадьбы, а Баба-Яга уже отдала кому-то нужный клубочек). Примеры:"
                "```qsp"
                "KILLVAR"
                "KILLVAR 'a'"
                "KILLVAR 'a',3"
                "```"
            ] |> String.concat "\n"
        let funcs =
            [
                [| String; Numeric |], ()
                [| String; |], ()
                [||], ()
            ] |> JustOverloads
        "killvar", dscr, funcs
        let dscr =
            [
                "`COPYARR [$массив-приёмник],[$массив-источник]` - копирование содержимого массива в другой массив. Копируются как текстовые, так и числовые значения массива. Размер массива-приёмника при копировании не имеет значения. Примеры:"
                "```qsp"
                "COPYARR '$a','$b'"
                "COPYARR 'a','b'"
                "COPYARR $arrname1,$arrname2"
                "COPYARR 'a<<$arrname1>>','a<<$arrname2>>'"
                "```"
            ] |> String.concat "\n"
        "copyarr", dscr, args [String; String]
        let dscr =
            [
                "`CLEAR` или `CLR` - очистка окна пользователя."
            ] |> String.concat "\n"
        "clear", dscr, unit'
        let dscr =
            [
                "`CLEAR` или `CLR` - очистка окна пользователя."
            ] |> String.concat "\n"
        "clr", dscr, unit'
        let dscr =
            [
                "`CLOSE [$путь к звуковому файлу]` - остановка проигрывания звукового файла с заданным названием."
                "`CLOSE ALL` - остановка проигрывания всех активных звуковых файлов."
            ] |> String.concat "\n"
        "close", dscr, arg String
        let dscr = "`CLOSE ALL` - остановка проигрывания всех активных звуковых файлов."
        "close all", dscr, unit' // особый-преособый случай
        let dscr =
            [
                "`CLS` - эквивалентен конструкции `CLEAR & *CLEAR & CLA & CMDCLEAR`, т.е. очищает всё, кроме списка предметов."
            ] |> String.concat "\n"
        "cls", dscr, unit'
        let dscr =
            [
                "`DYNAMIC [$строка кода],[параметр 1],[параметр 2], ...` - выполнение кода. Данный оператор позволяет динамически генерировать код игры. Переданные параметры хранятся в массиве `ARGS`. После выполнения кода предыдущие значения `ARGS` восстанавливаются. Примеры:"
                "```qsp"
                "DYNAMIC '$a=\"string<<$b>>\"'"
                "DYNAMIC '$a'"
                "DYNAMIC 'if $a=\"string\":''text!'''"
                "DYNAMIC \""
                "$args[0]"
                "addobj $args[1]"
                "\",'Текст','Вилка'"
                "```"
            ] |> String.concat "\n"
        "dynamic", dscr, argAndArgList String Any
        let dscr =
            [
                "`MENU [$выражение]` - вызов меню с заданным названием"
            ] |> String.concat "\n"
        "menu", dscr, arg String
        let dscr =
            [
                "`MSG [выражение]` - вывод заданного сообщения в информационном окне."
            ] |> String.concat "\n"
        "msg", dscr, arg String
        let dscr =
            [
                "`NL [выражение]` - переход на новую строку (перевод каретки), затем вывод текста в окне пользователя. Если `[выражение]` не указано, то перевод строки. Отличается от `PL` порядком вывода текста."
            ] |> String.concat "\n"
        let funcs =
            [
                [| String |], ()
                [| |], ()
            ] |> JustOverloads
        "nl", dscr, funcs
        let dscr =
            [
                "`P [выражение]` - вывод текста в окно пользователя (по умолчанию находится справа внизу, обычно служит для вспомогательных целей)."
            ] |> String.concat "\n"
        "p", dscr, arg String
        let dscr =
            [
                "`PL [выражение]` - вывод текста, затем переход на новую строку в окне пользователя. Если `[выражение]` не указано, то перевод строки"
            ] |> String.concat "\n"
        "pl", dscr, arg String
        let dscr =
            [
                "`PLAY [$путь к звуковому файлу],[#громкость]` - проигрывание звукового файла с заданным названием и громкостью. Громкость указывается в процентах от 0 до 100."
                "Параметр `[#громкость]` может отсутствовать, при этом громкость принимается равной 100%. Примеры:"
                "`PLAY 'sound/music.mp3'` - проигрывает файл с громкостью 100%"
                "`PLAY 'sound/music.mp3',50` - проигрывает файл в половину возможной громкости"
                "`PLAY 'sound/music.mp3',0` - проигрывает файл с громкостью 0% (без звука)"
                "`PLAY '<<$file>>.mid',volume` - проигрывает файл, имя которого хранится в $file (расширение \"mid\") с громкостью, значение которой задано в volume"
                "`PLAY $file,volume` - аналогично"
                "Если файл уже проигрывается, то изменяется громкость звучания без его \"перезапуска\". Поддерживается множество различных аудиоформатов и одновременное звучание до 32-х композиций."
            ] |> String.concat "\n"
        "play", dscr, argsAndOptional [ String ] Numeric
        let dscr =
            [
                "обновление интерфейса (а также смена цветов, шрифта, назначенных с помощью системных переменных `BCOLOR`, `FCOLOR`, `LCOLOR`, `FSIZE`, `$FNAME`)."
            ] |> String.concat "\n"
        "refint", dscr, unit'
        let dscr =
            [
                "`SETTIMER [#выражение]` - задает интервал таймера для локации-счётчика (по умолчанию 500мс, т.е. локация-счётчик обрабатывается 2 раза в секунду). Также влияет на частоту автоматического обновления интерфейса."
            ] |> String.concat "\n"
        "settimer", dscr, arg Numeric
        let dscr =
            [
                "`SHOWACTS [#выражение]` - если значение выражения отлично от 0, то показывает список действий, иначе скрывает его. Примеры:"
                "`SHOWACTS 1` - показывает список действий"
                "`SHOWACTS 0` - скрывает список действий"
            ] |> String.concat "\n"
        "showacts", dscr, arg Numeric
        let dscr =
            [
                "`SHOWINPUT [#выражение]` - если значение выражения отлично от 0, то показывает строку ввода, иначе скрывает её."
            ] |> String.concat "\n"
        "showinput", dscr, arg Numeric
        let dscr =
            [
                "`SHOWOBJS [#выражение]` - если значение выражения отлично от 0, то показывает список предметов, иначе скрывает его."
            ] |> String.concat "\n"
        "showobjs", dscr, arg Numeric
        let dscr =
            [
                "`SHOWSTAT [#выражение]` - если значение выражения отлично от 0, то показывает дополнительное описание, иначе скрывает его."
            ] |> String.concat "\n"
        "showstat", dscr, arg Numeric
        let dscr =
            [
                "`VIEW [$путь к графическому файлу]` - просмотр картинки из указанного файла. Если вместо `[$путь к графическому файлу]` указана пустая строка (`''`) или параметр не указан, то это скроет окно с картинкой."
            ] |> String.concat "\n"
        let funcs =
            [
                [| String |], ()
                [| |], ()
            ] |> JustOverloads
        "view", dscr, funcs
        let dscr =
            [
                "`WAIT [#выражение]` - остановка выполнения программы на заданное количество миллисекунд (1 секунда = 1000 миллисекунд)."
            ] |> String.concat "\n"
        "wait", dscr, arg Numeric
    ]
let jump =
    let dscr =
        [
            "`JUMP [$выражение]` - переход в текущем коде (при обработке локации / выбора действия) на метку `[$выражение]`. Метка на локации обозначается как `:[название метки]`. После описания метки (через \"&\") могут идти операторы. Если интерпретатор находит случайную метку, то он её просто игнорирует. Например:"
            "```qsp"
            "jump 'КонеЦ'"
            "p 'Это сообщение не будет выведено'"
            ":конец"
            "p 'А это сообщение пользователь увидит'"
            "```"
            "С помощью оператора `JUMP` можно организовывать циклы:"
            "```qsp"
            "s = 0"
            ":loop"
            "if s < 9:"
            "    s=s+1"
            "    pl s"
            "    jump 'LOOP'"
            "end"
            "p 'Всё!'"
            "```"
            "Оператор `JUMP` также полезен во время отладки квеста, чтобы \"обойти\" группу операторов, которые временно не нужны."
        ] |> String.concat "\n"
    "jump", dscr, arg String
let transferOperators =
    [
        let dscr =
            [
                "`GOSUB [$выражение],[параметр 1],[параметр 2], ...` или `GS [$выражение],[параметр 1],[параметр 2], ...` - обработка локации с названием `[$выражение]`. Базовое описание локации добавляется к текущему описанию, базовые действия добавляются к текущим действиям, и происходит выполнение операторов в поле \"Выполнить при посещении\", затем возврат на исходную строку (продолжение выполнения программы)."
                "Переданные параметры хранятся в массиве `ARGS`. После обработки локации предыдущие значения `ARGS` восстанавливаются. Примеры:"
                "```qsp"
                "GS 'ход' & ! обработка локации \"ход\". Массив `ARGS` пуст."
                "GS $loc,1 & ! обработка локации, название которой хранится в $loc с передачей одного параметра. ARGS[0] равен 1."
                "GS 'ход',$var,2,'данные' & ! обработка локации \"ход\" с передачей 3-х параметров. `$ARGS[0]` равен значению `$var`, `ARGS[1]` равен 2, `$ARGS[2]` содержит строку \"данные\"."
                "```"
            ] |> String.concat "\n"
        let sign = argAndArgList String Any
        "gosub", dscr, sign
        "gs", dscr, sign
        let dscr =
            [
                "`GOTO [$выражение],[параметр 1],[параметр 2], ...` или `GT [$выражение],[параметр 1],[параметр 2], ...` - переход на локацию с названием `[$выражение]`. Поле основного описания локации, а также список текущих действий заменяются описанием и действиями новой локации."
                "Переданные параметры хранятся в массиве `ARGS`. Примеры:"
                "```qsp"
                "GT 'локация' & ! переход на локацию \"локация\". Массив `ARGS` пуст."
                "GT 'локация',1,'данные' & ! переход на локацию \"локация\" с передачей 2-х параметров. `ARGS[0]` равен 1, `$ARGS[1]` содержит строку \"данные\"."
                "```"
            ] |> String.concat "\n"
        let sign = argAndArgList String Any
        "goto", dscr, sign
        "gt", dscr, sign
        let dscr =
            [
                "`XGOTO [$выражение],[параметр 1],[параметр 2], ...` или `XGT [$выражение],[параметр 1],[параметр 2], ...` - отличается от \"`GOTO` / `GT`\" тем, что при переходе не очищается поле основного описания локации, а базовое описание новой локации добавляется к текущему основному описанию. Тем не менее, список действий заменяется действиями новой локации."
            ] |> String.concat "\n"
        let sign = argAndArgList String Any
        "xgoto", dscr, sign
        "xgt", dscr, sign
    ]
let transferOperatorsSet =
    transferOperators
    |> List.map (fun (name, dscr, sign) -> name)
    |> Set.ofList
/// Всевозможные процедуры
let procs =
    jump::(procedures @ transferOperators)
    |> List.map (fun (name, dscr, sign) -> name, ((dscr:Description), sign))
    |> Map.ofList

let exprSymbolicOperators =
    let arg x (return':VarType) = arg x, return'
    let unit' (return':VarType) = unit', return'
    let args xs (return':VarType) = args xs, return'
    let argList typ (return':VarType) = argList typ, return'
    let argAndArgList x typ (return':VarType) = argAndArgList x typ, return'
    [
        let dscr =
            "`[$выражение 1] & [$выражение 2]` - операция объединения строковых выражений."
        "&", dscr, args [String; String] String
    ]

let exprNamedOperators =
    let arg x (return':VarType) = arg x, return'
    let unit' (return':VarType) = unit', return'
    let args xs (return':VarType) = args xs, return'
    let argList typ (return':VarType) = argList typ, return'
    let argAndArgList x typ (return':VarType) = argAndArgList x typ, return'
    [
        // Для значения "верно" настоятельно рекомендуется использовать -1.
        let dscr =
            [
                "`[#выражение 1] AND [#выражение 2]` - операция \"и\". Если оба рядом стоящие выражения верны, то верно и всё выражение."
            ] |> String.concat "\n"
        "and", dscr, args [Numeric; Numeric] Numeric
        let dscr =
            [
                "`[#выражение 1] OR [#выражение 2]` - операция \"или\". Если хотя бы одно из рядом стоящих выражений верно, то верно и всё выражение."
            ] |> String.concat "\n"
        "or", dscr, args [Numeric; Numeric] Numeric
        let dscr =
            [
                "`[#выражение 1] MOD [#выражение 2]` - операция вычисления остатка от деления."
            ] |> String.concat "\n"
        "mod", dscr, args [Numeric; Numeric] Numeric
        let dscr =
            [
                "`NO [#выражение]` - отрицание. Верно, если `[#выражение]` ложно и наоборот (аналогично \"NOT\" в Basic)."
            ] |> String.concat "\n"
        "no", dscr, arg Numeric Numeric
        let dscr =
            [
                "`OBJ [$выражение]` - верно, если в рюкзаке есть предмет `[$выражение]`."
            ] |> String.concat "\n"
        "obj", dscr, arg String Numeric
        let dscr =
            [
                "`LOC [$выр]` - верно, если в игре есть локация с названием `[$выр]`."
            ] |> String.concat "\n"
        "loc", dscr, arg String Numeric
    ]
let keywords =
    [
        let dscr =
            [
                "`IF [#выражение]:[оператор1] & [оператор2] & ... ELSE [оператор3] & [оператор4] & ...` - если `[#выражение]` верно (не равно 0), то выполнить заданные операторы до ключевого слова `ELSE`, иначе выполнить операторы после `ELSE`."
                "Если ключевое слово `ELSE` не указано, то при верном значении [#выражение], выполняются все операторы, находящиеся после символа `:`."
                "Примеры:"
                "```qsp"
                "if ((a+b)/c)=45+54 or (b<5 or c>45) and no obj 'лопата' and $f=$vvv+'RRRRR':p 'OK' & goto 'Next'"
                "if был_здесь[$curloc]:exit"
                "if a<3:jump 'sss'"
                "if $имя = '':msg 'Введите имя!' & jump 'ввод'"
                "if a+b=2:c=30 & gt 'next' else c=10"
                "```"
            ] |> String.concat "\n"
        "if", dscr
        "else", dscr
        "elseif", dscr
        "end", "Завершает конструкции `ACT`, `IF` или `FOR`."
        let dscr =
            [
                "`ACT [$название],[$путь к файлу изображения]:[оператор] & [оператор] & ...` - добавление действия к существующим на локации."
                "К действиям добавляется новое с описанием `[$название]` и изображением `[$путь к файлу изображения]`. При нажатии на него выполнятся заданные операторы."
                "Параметр `[$путь к файлу изображения]` может отсутствовать, при этом действие добавится без изображения."
            ] |> String.concat "\n"
        "act", dscr
        let dscr =
            [
                "`SET [название переменной]=[выражение]`, `LET [название переменной]=[выражение]` или `[название переменной]=[выражение]` - установка значения переменной. Если нужно установить текстовое значение переменной, то перед её названием ставится `$`."
                "Примеры:"
                "```qsp"
                "SET A=123"
                "SET $B='строка'"
                "LET C=A"
                "D=456"
                "$D='ещё строка'"
                "$D='и ещё"
                "       одна"
                "строка'"
                "```"
            ] |> String.concat "\n"
        "set", dscr
        "let", dscr
        let dscr =
            [
                "завершение выполнения текущего кода (преждевременный выход из подпрограммы / обработчика какого-либо события...)."
            ] |> String.concat "\n"
        "exit", dscr
        let dscr =
            [
                "**FOR** `[#переменная]` **=** `[#выражение]` **TO** `[#выражение]`**:** `[операторы]` - Выполняет `[#операторы]` несколько раз, по очереди присваивая `[#переменной]` все численные значения от первого до второго `[#выражения]`."
                ""
                "Однострочная форма записи:"
                "```qsp"
                "for номер_нпц = 1 to количество_нпц: gs 'инициализировать нпц', номер_нпц"
                "стоимость['меч'] = 10"
                "стоимость['доспех'] = 250"
                "стоимость['щит'] = 15"
                "стоимость_снаряжения = 0"
                "for номер_предмета = 0 to arrsize('стоимость')-1: стоимость_снаряжения += стоимость[номер предмета]"
                "```"
                ""
                "Многострочная форма записи:"
                "* После символа `:` ставится перенос строки"
                "* Заканчивается FOR строкой `END`"
                "* Допускается вложенность неограниченной глубины. Каждый уровень вложения должен заканчиваться своей строкой `END`."
                "* Пример:"
                "    ```qsp"
                "    for i = 0 to arrsize('arr')-1:"
                "        *pl arr[i]"
                "        if arr[i] > 10:"
                "            jump конец"
                "        end"
                "    end"
                "    ```"
                ""
                "Можно еще задать шаг цикла, для этого используется **STEP**:"
                "```qsp"
                "for нечётные = 1 to 10 step 2: *pl нечётные"
                "```"
            ] |> String.concat "\n"
        "for", dscr
        "to", "**TO** — ключевое слово для конструкции FOR"
        "step", "**STEP** — ключевое слово для конструкции FOR"
    ]
