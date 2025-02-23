module SharpSDF.DOT

open SharpSDF.Ast

let floatString (v:float) = 
    match string v with
    | s when s.Contains('.') -> s
    | s -> s+".0"

let compare = function
    | EQ -> "=="
    | NE -> "!="
    | LT -> "<"
    | GT -> ">"
    | LE -> "<="
    | GE -> ">="

type NodeId = int

type Expr =
    | Leaf of string
    | Node of string * NodeId[]
    
let getSubExprId (v:Expr) (exprs:Map<Expr,int>) =
    match exprs.TryFind v with
    | Some i -> i, exprs
    | None ->
        let i = exprs.Count
        i, exprs.Add(v,i)

let rec EvalB (v:Bool) (acc) =
    let Eval = EvalB
    match v with
    | Bool.Varying s -> acc |> getSubExprId(Leaf s)
    | Bool.Literal l -> acc |> getSubExprId(Leaf (string l))
    | Bool.IfThenElse (cond,t,f) ->
        let cond,acc = acc |> EvalB cond
        let t,acc = acc |> EvalB t
        let f,acc = acc |> EvalB f
        acc |> getSubExprId (Node ("IfThenElse", [|cond; t; f|]))
    | Bool.(!) v ->
        let v,acc = acc |> Eval v
        acc |> getSubExprId (Node ("!", [|v|]))
    | Bool.(&&) (v1, v2) ->
        let v1,acc = acc |> Eval v1
        let v2,acc = acc |> Eval v2
        acc |> getSubExprId (Node ("&&", [|v1; v2|]))
    | Bool.(||) (v1, v2) ->
        let v1,acc = acc |> Eval v1
        let v2,acc = acc |> Eval v2
        acc |> getSubExprId (Node ("||", [|v1; v2|]))
    | Bool.(==) (v1, v2) ->
        let v1,acc = acc |> Eval v1
        let v2,acc = acc |> Eval v2
        acc |> getSubExprId (Node ("==", [|v1; v2|]))
    | Bool.(!=) (v1, v2) ->
        let v1,acc = acc |> Eval v1
        let v2,acc = acc |> Eval v2
        acc |> getSubExprId (Node ("!=", [|v1; v2|]))
    | Bool.CompareInt (fn, v1, v2) ->
        let v1,acc = acc |> EvalI v1
        let v2,acc = acc |> EvalI v2
        acc |> getSubExprId (Node (compare fn, [|v1; v2|]))
    | Bool.CompareFloat (fn, v1, v2) ->
        let v1,acc = acc |> EvalF v1
        let v2,acc = acc |> EvalF v2
        acc |> getSubExprId (Node (compare fn, [|v1; v2|]))

and EvalI(v:Int) (acc) =
    let Eval = EvalI
    match v with
    | Int.Varying s -> acc |> getSubExprId(Leaf s)
    | Int.Literal l -> acc |> getSubExprId(Leaf (string l))
    | Int.IfThenElse (cond,t,f) ->
        let cond,acc = acc |> EvalB cond
        let t,acc = acc |> Eval t
        let f,acc = acc |> Eval f
        acc |> getSubExprId (Node ("IfThenElse", [|cond; t; f|]))
    | Int.(~-) v ->
        let v,acc = acc |> Eval v
        acc |> getSubExprId (Node ("-", [|v|]))
    | Int.(~+) v ->
        let v,acc = acc |> Eval v
        acc |> getSubExprId (Node ("+", [|v|]))
    | Int.(+) (v1, v2) ->
        let v1,acc = acc |> Eval v1
        let v2,acc = acc |> Eval v2
        acc |> getSubExprId (Node ("+", [|v1; v2|]))
    | Int.(-) (v1, v2) ->
        let v1,acc = acc |> Eval v1
        let v2,acc = acc |> Eval v2
        acc |> getSubExprId (Node ("-", [|v1; v2|]))
    | Int.(*) (v1, v2) ->
        let v1,acc = acc |> Eval v1
        let v2,acc = acc |> Eval v2
        acc |> getSubExprId (Node ("*", [|v1; v2|]))
    | Int.(/) (v1, v2) ->
        let v1,acc = acc |> Eval v1
        let v2,acc = acc |> Eval v2
        acc |> getSubExprId (Node ("/", [|v1; v2|]))
    | Int.(%) (v1, v2) ->
        let v1,acc = acc |> Eval v1
        let v2,acc = acc |> Eval v2
        acc |> getSubExprId (Node ("%", [|v1; v2|]))
    | Int.Abs v ->
        let v,acc = acc |> Eval v
        acc |> getSubExprId (Node ("abs", [|v|]))
    | Int.Clamp (v, min, max) ->
        let v,acc = acc |> Eval v
        let min,acc = acc |> Eval min
        let max,acc = acc |> Eval max
        acc |> getSubExprId (Node ("clamp", [|v; min; max|]))
    | Int.Max (v1, v2) ->
        let v1,acc = acc |> Eval v1
        let v2,acc = acc |> Eval v2
        acc |> getSubExprId (Node ("max", [|v1; v2|]))
    | Int.Min (v1, v2) ->
        let v1,acc = acc |> Eval v1
        let v2,acc = acc |> Eval v2
        acc |> getSubExprId (Node ("min", [|v1; v2|]))

and EvalF(v:Float) (acc) =
    let Eval = EvalF
    match v with
    | Float.Varying s -> acc |> getSubExprId(Leaf s)
    | Float.Literal l -> acc |> getSubExprId(Leaf (floatString l))
    | Float.Length2 (x, y) ->
        let x,acc = acc |> Eval x
        let y,acc = acc |> Eval y
        acc |> getSubExprId (Node ("length2", [|x; y|]))
    | Float.Length3 (x, y, z) ->
        let x,acc = acc |> Eval x
        let y,acc = acc |> Eval y
        let z,acc = acc |> Eval z
        acc |> getSubExprId (Node ("length3", [|x; y; z|]))
    | Float.Length4 (x, y, z, w) ->
        let x,acc = acc |> Eval x
        let y,acc = acc |> Eval y
        let z,acc = acc |> Eval z
        let w,acc = acc |> Eval w
        acc |> getSubExprId (Node ("length4", [|x; y; z; w|]))
    | Float.IfThenElse (cond,t,f) ->
        let cond,acc = acc |> EvalB cond
        let t,acc = acc |> Eval t
        let f,acc = acc |> Eval f
        acc |> getSubExprId (Node ("IfThenElse", [|cond; t; f|]))
    | Float.(~-) v ->
        let v,acc = acc |> Eval v
        acc |> getSubExprId (Node ("-", [|v|]))
    | Float.(~+) v ->
        let v,acc = acc |> Eval v
        acc |> getSubExprId (Node ("+", [|v|]))
    | Float.(+) (v1, v2) ->
        let v1,acc = acc |> Eval v1
        let v2,acc = acc |> Eval v2
        acc |> getSubExprId (Node ("+", [|v1; v2|]))
    | Float.(-) (v1, v2) ->
        let v1,acc = acc |> Eval v1
        let v2,acc = acc |> Eval v2
        acc |> getSubExprId (Node ("-", [|v1; v2|]))
    | Float.(*) (v1, v2) ->
        let v1,acc = acc |> Eval v1
        let v2,acc = acc |> Eval v2
        acc |> getSubExprId (Node ("*", [|v1; v2|]))
    | Float.(/) (v1, v2) ->
        let v1,acc = acc |> Eval v1
        let v2,acc = acc |> Eval v2
        acc |> getSubExprId (Node ("/", [|v1; v2|]))
    | Float.(%) (v1, v2) ->
        let v1,acc = acc |> Eval v1
        let v2,acc = acc |> Eval v2
        acc |> getSubExprId (Node ("%", [|v1; v2|]))
    | Float.Abs v ->
        let v,acc = acc |> Eval v
        acc |> getSubExprId (Node ("abs", [|v|]))
    | Float.Clamp (v, min, max) ->
        let v,acc = acc |> Eval v
        let min,acc = acc |> Eval min
        let max,acc = acc |> Eval max
        acc |> getSubExprId (Node ("clamp", [|v; min; max|]))
    | Float.Exp v ->
        let v,acc = acc |> Eval v
        acc |> getSubExprId (Node ("exp", [|v|]))
    | Float.Lerp (v1, v2, t) ->
        let v1,acc = acc |> Eval v1
        let v2,acc = acc |> Eval v2
        let t,acc = acc |> Eval t
        acc |> getSubExprId (Node ("lerp", [|v1; v2; t|]))
    | Float.Max (v1, v2) ->
        let v1,acc = acc |> Eval v1
        let v2,acc = acc |> Eval v2
        acc |> getSubExprId (Node ("max", [|v1; v2|]))
    | Float.Min (v1, v2) ->
        let v1,acc = acc |> Eval v1
        let v2,acc = acc |> Eval v2
        acc |> getSubExprId (Node ("min", [|v1; v2|]))
    | Float.SmoothStep (min, max, v) ->
        let min,acc = acc |> Eval min
        let max,acc = acc |> Eval max
        let v,acc = acc |> Eval v
        acc |> getSubExprId (Node ("smoothstep", [|min; max; v|]))
    | Float.Step (v1, v2) ->
        let v1,acc = acc |> Eval v1
        let v2,acc = acc |> Eval v2
        acc |> getSubExprId (Node ("step", [|v1; v2|]))



let NodeArgId (exprMap:Map<NodeId,Expr>) (parentId:NodeId) (arg:int) (id:NodeId)  =
    match exprMap[id] with
    | Leaf s -> $"_{parentId}_{arg}"
    | Node (op, args) -> $"_{id}"

let NodeArgDeclaration (exprMap:Map<NodeId,Expr>) (parentId:NodeId) (arg:int) (id:NodeId)  =
    match exprMap[id] with
    | Leaf s -> $"_{parentId}_{arg} [label=\"{s}\"]"
    | Node (op, args) -> ""

let NodeArgsDeclaration (exprMap:Map<NodeId,Expr>) (expr:Expr, id:NodeId)  =
    match expr with
    | Leaf s -> ""
    | Node (op, args) -> args |> Array.mapi (NodeArgDeclaration exprMap id) |> String.concat "\n"

let NodeDeclaration (exprMap:Map<NodeId,Expr>) (expr:Expr, id:NodeId)  =
    match expr with
    | Leaf s -> ""
    | Node (op, args) -> $"_{id} [label=\"{op}\"]" 

let EdgeDeclaration (exprMap:Map<int,Expr>) (expr:Expr, id:NodeId) =
    let concatWithSpace = String.concat " "
    match expr with
    | Leaf s -> ""
    | Node (op, args) -> $"_{id} -> {{ {args |> Array.mapi (NodeArgId exprMap id) |> concatWithSpace } }}  # {op}"

let GenerateGraph (shader : Wrappers.float4) = 
    let color = shader
    let r,exprs = EvalF color.r.expr Map.empty
    let g,exprs = EvalF color.g.expr exprs
    let b,exprs = EvalF color.b.expr exprs
    let a,exprs = EvalF color.a.expr exprs
    let swap (x, y) = y, x
    let exprMap = exprs |> Map.toList |> List.map swap |> Map.ofList
    let nodeArgs = exprs |> Map.toList |> List.map (NodeArgsDeclaration exprMap) |> String.concat "\n"
    let nodes = exprs |> Map.toList |> List.map (NodeDeclaration exprMap) |> String.concat "\n"
    let edges = exprs |> Map.toList |> List.map (EdgeDeclaration exprMap) |> String.concat "\n"
    $"""
 digraph
 {{
 	fontname="Helvetica,Arial,sans-serif"
	node [fontname="Helvetica,Arial,sans-serif"]
	edge [fontname="Helvetica,Arial,sans-serif"]

 // Nodes
 r [style=filled fillcolor=red]
 g [style=filled fillcolor=green]
 b [style=filled fillcolor=blue]
 a [style="filled,dashed"]
 {nodes}

 // Node Args
 {nodeArgs}

// Edges
 {edges}
 r -> _{r}
 g -> _{g}
 b -> _{b}
 a -> _{a}
 }}
    """
