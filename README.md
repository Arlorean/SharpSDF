# SharpSDF

SharpSDF is an F# library for writing HLSL shaders that can be tested and renderered using the CPU using .NET and also transpiled into HLSL (or GLSL) shader code for execution on the GPU.

The library also contains a set of Signed Distance Functions for rendering images without using geometry.

The initial setup uses Fable/F# to render using a WebBrowser.

Future versions will add support for Unity ShaderLab HLSL shader generation at compile time (not possible at runtime).

## Example

> ![alt text](docs/images/SharpSDF-Example.png)

```fsharp
let sdfShader =
    outerShadow gray50 30.0 |>>
    solidFill blue |>> 
    innerShadow gray50 30.0 |>> 
    solidStroke red 20.0
    
let sdfShape =
    (_circle 100.0 |> _sdf)

let shader position =
    (position |> SdfContext.Create |> sdfShape |> sdfShader clear) 

let renderer = CanvasRenderer(".view")
renderer.Render shader
```

## Setup

### .NET SDK (9.0)

https://dotnet.microsoft.com/en-us/download

### VSCode + Ionide Extension (F# Development)

- https://code.visualstudio.com/
- https://marketplace.visualstudio.com/items?itemName=Ionide.Ionide-fsharp

> [!NOTE]
> Run all commands below using the VS Code Terminal.

### Fable

```powershell
dotnet tool install fable
```

### Fable Transpile (to JavaScript)

```powershell
dotnet fable
```

## Testing

Vite (via npm) is a useful tool to allow for hot reloading of Fable/F# code. Fable watches for any change in F# source files and the re-transpiles them to JavaScript and notifes vite to reload the resulting .fs.js generated files that have changed.

> [!NOTE] Permission Problems
> If you run into any permission problems using Powershell on Windows
> you may have to run this command:
>
> ```powershell
> dotnet dev-certs https --trust
> ```

### NPM (Node Package Manager)

The easiest way to install `npm` is to install `node.js`:

https://nodejs.org/

### Vite Install

```powershell
npm i -D vite 
```

### Vite Hot Reload

```powershell
dotnet fable watch --run npx vite
```

The terminal output will provide a local link that will display the contents of your index.html file, and your Fable/F#/SharpSDF application, e.g.

```powershell
.> cmd /C npx vite
  VITE v6.0.7  ready in 119 ms

  ➜  Local:   http://localhost:5173/
  ➜  Network: use --host to expose
  ➜  press h + enter to show help

Fable compilation finished in 1354ms

Watching .
```
