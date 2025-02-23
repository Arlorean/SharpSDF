namespace SharpSDF.Ast

type Compare = | EQ | NE | LT | GT | LE | GE

and [<RequireQualifiedAccess>] Bool =
    | Varying of string
    | Literal of System.Boolean
    | IfThenElse of Bool * Bool * Bool
    | (!) of Bool
    | (&&) of Bool * Bool
    | (||) of Bool * Bool
    | (==) of Bool * Bool
    | (!=) of Bool * Bool
    | CompareInt of Compare * Int * Int
    | CompareFloat of Compare * Float * Float

and [<RequireQualifiedAccess>] Int =
    | Varying of string
    | Literal of System.Int32
    | IfThenElse of Bool * Int * Int
    | (~-) of Int
    | (~+) of Int
    | (+) of Int * Int
    | (-) of Int * Int
    | (*) of Int * Int
    | (/) of Int * Int
    | (%) of Int * Int
    | Abs of Int
    | Clamp of Int * Int * Int
    | Max of Int * Int
    | Min of Int * Int

and [<RequireQualifiedAccess>] Float =
    | Varying of string
    | Literal of System.Double
    | Length2 of Float * Float
    | Length3 of Float * Float * Float
    | Length4 of Float * Float * Float * Float
    | IfThenElse of (Bool * Float * Float)
    | (~-) of Float
    | (~+) of Float
    | (+) of Float * Float
    | (-) of Float * Float
    | (*) of Float * Float
    | (/) of Float * Float
    | (%) of Float * Float
    | Abs of Float
    | Clamp of Float * Float * Float
    | Exp of Float
    | Lerp of Float * Float * Float
    | Max of Float * Float
    | Min of Float * Float
    | SmoothStep of Float * Float * Float
    | Step of Float * Float

