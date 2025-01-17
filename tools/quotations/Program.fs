module SharpSDF.Main


open SharpSDF.HLSL_Ast
open type SharpSDF.HLSL_Ast.Intrinsics

// let sdCircle (r: Float) (p : Float2) =
//     length p - r
    
// let sdRoundedBox (hs:Float2) (tr:Float,br:Float,tl:Float,bl:Float) (p:Float2) : Float =


let inline tee (f : 't -> unit)  v: 't =
    f v
    v
    
let test (v : 't) = ()

let main() =

    let p = f2__ 2 2 

    (p |> 
        Expression.sdRoundedBox 
            (f2__ 4 4) 
            (f_ 5, f_ 5, f_ 5, f_ 5)
    )
    // Test
    |> tee test 
    // Log
    |> printfn "%A" 

    
main()



