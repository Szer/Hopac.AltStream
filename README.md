# Hopac.AltStream

Contains just one module with **better** streams for Hopac library.
They are not persistent by default, takes constant heap memory (also generates less memory traffic overall) and slightly faster.

## Installation

* `paket add Hopac.AltStream`
* `dotnet add package Hopac.AltStream`
* `Install-Package Hopac.AltStream`

Or simply add `alt-stream.fs` directly via `paket.dependencies`

* `github Szer/Hopac.AltStream alt-stream.fs`

## Requirements

    * .NET Standard >= 2.0
    * Hopac >= 0.4.1

## Maintainer(s)

* [Ayrat Hudaygulov][ayratMail]

[ayratMail]: mailto:ayrat@hudaygulov.ru "Ayrat Hudaygulov email"