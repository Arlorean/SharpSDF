module SharpSDF.Expression

open SharpSDF.HLSL_Ast
open type SharpSDF.HLSL_Ast.Intrinsics

let sdCircle (r: Float) (p : Float2) =
    length p - r
    
let sdRoundedBox (hs:Float2) (tr:Float,br:Float,tl:Float,bl:Float) (p:Float2) : Float =
    
    let tb = 
        if2_ (gt__ p.x 0)
            (f2_  tr br)
            (f2_  br tr)
    
    let t = tb.x
    let b = tb.y

    let r = if_ (gt__ p.y 0) t b

    let q = 
        (f2_ (abs p.x) (abs p.y))
        - hs
        + (f2_ r r)

    let a =
        min (max (q.x,q.y), f_ 0)

    let b = 
        max (q, Float2.Zero) |> length 

    (a + b - r)
        
open SharpSDF.HLSL
open type SharpSDF.HLSL.Intrinsics

type SdfContext = 
    {
        Position: Float2
    }
    with
        static member Create( p ) = { Position = p }
        // member __.X = __.Position.x
        // member __.Y = __.Position.y
        // member __.Offset( x, y ) = { __ with Position = float2( __.X + x, __.Y  + y ) }
        // member __.Offset( xy : float2 ) = { __ with Position = float2( __.X + xy.x, __.Y  + xy.y ) }

type SdfResult = Float
type SdfFn = SdfContext -> SdfResult

type SdfShape = 
    {
        Sdf : SdfFn
        Name : string
    }
    override __.ToString (): string = __.Name

let _sdf (shape : SdfShape) (ctx : SdfContext) : Float =
    ctx |> shape.Sdf 

let mkShape name fn = { Sdf = fn; Name = name }
let mkShapeWithMeta name fn = { Sdf = fn; Name = name }

let _circle (r : Float) : SdfShape =
    fun ctx -> sdCircle r (ctx.Position)
    |> mkShape "Circle"

// let _translate (offset : float2) (s1 : SdfShape) : SdfShape =
//     fun (ctx : SdfContext) ->
//         ctx.Offset(offset) |> _sdf s1
//     |> mkShape(sprintf "translate [%s] by (%f,%f)" s1.Name (offset.x) (offset.y))

let _add (s1 : SdfShape) (s2 : SdfShape) : SdfShape =
    s1

// let _sub (s1 : SdfShape) (s2 : SdfShape) : SdfShape =
//     fun ctx ->
//         opSubtraction 
//             (_sdf s1 ctx) 
//             (_sdf s2 ctx)
//     |> mkShape(sprintf "%s-%s" s1.Name s2.Name)

let (+.) = _add

//let (-.) = _sub

