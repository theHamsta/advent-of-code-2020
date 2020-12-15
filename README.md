# advent-of-code-2020

## Installation of dotnet Framework

```bash
snap install dotnet-sdk --classic
```

## Create and Run Project

New F# project
```bash
dotnet new console -lang "F#" -o src/01
```

Run

```bash
dotnet run --project src/02 input/02
```

Files without a main can be run directly using `dotnet fsi`. Installation:

dotnet tool install -g --add-source "https://pkgs.dev.azure.com/dnceng/public/_packaging/dotnet-tools/nuget/v3/index.json" Microsoft.dotnet-interactive`
