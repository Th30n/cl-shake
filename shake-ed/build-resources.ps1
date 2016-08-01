$dir = Split-Path $MyInvocation.MyCommand.Path -Parent
rcc.exe -binary (Join-Path $dir "resource.qrc") -o (Join-Path $dir "resource.rcc")
