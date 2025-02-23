namespace SharpSDF

open System

type Intrinsics =
    static member inline abs (v:int) = Math.Abs v
    static member inline abs (v:float) = Math.Abs v
    static member inline clamp (v:int, min:int, max:int) = Math.Clamp(v, min, max)
    static member inline clamp (v:float, min:float, max:float) = Math.Clamp(v, min, max)
    static member inline exp (v:float) = Math.Exp v
    static member inline length (x:float, y:float) = Math.Sqrt(x*x + y*y)
    static member inline length (x:float, y:float, z:float) = Math.Sqrt(x*x + y*y + z*z)
    static member inline length (x:float, y:float, z:float, w:float) = Math.Sqrt(x*x + y*y + z*z + w*w)
    static member inline lerp (v1, v2, t) =
        v1 + t*(v2-v1)
    static member inline max (v1:int, v2:int) = Math.Max(v1,v2)
    static member inline max (v1:float, v2:float) = Math.Max(v1,v2)
    static member inline min (v1:int, v2:int) = Math.Min(v1,v2)
    static member inline min (v1:float, v2:float) = Math.Min(v1,v2)
    static member inline smoothstep (min:float, max:float, v:float) =
        let t = Math.Clamp((v - min) / (max - min), 0.0, 1.0)
        t*t*(3.0-2.0*t)
    static member inline step (v1:float, v2:float) =
        if v1 >= v2 then 1.0 else 0.0

