# Qsp.FSharp

[QSP](https://github.com/QSPFoundation/qsp) экосистема, написанная в F# среде. Содержит: ядро языка (AST, парсер, принтер), языковой сервер ([LSP](https://github.com/QSPFoundation/qsp)) и CLI утилиту для манипуляций исходниками языка.

## Карта проекта

* [Qsp.FSharp.Core](./src/Qsp.FSharp.Core/README.md)

  Содержит:
  * AST — абстрактное синтаксическое дерево языка QSP
  * Парсер — переводит исходный код QSP в AST
  * Принтер — переводит AST в исходный код QSP
* `Qsp.FSharp.LanguageServer`

  Сервер, который предоставляет поддержку языка QSP в редакторах, которые поддерживают [LSP](https://microsoft.github.io/language-server-protocol/specifications/specification-current/). Сейчас используется только в VS Code через [Qsp.FSharp.VsCodeExtension](https://github.com/gretmn102/QSP-VSCode) расширение.

  Использует `Qsp.FSharp.Core`.

  Для своей работы требует:
  * либо net461 runtime
  * либо dotnet 3.1 runtime
* `Qsp.FSharp.Cli`

  Использует `Qsp.FSharp.Core`.

## Deploy

* Install [net461 Dev Pack](https://dotnet.microsoft.com/en-us/download/dotnet-framework/net461) (not necessary on Linux)
* Install [dotnet 3.1.xxx version](https://dotnet.microsoft.com/en-us/download/dotnet/3.1)
* Run `build.cmd` on Windows, or `./build.sh` on Linux
