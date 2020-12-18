#if INTERACTIVE
#r "nuget: FParsec"
#endif

open System.IO
open System
open FParsec

type Operator =
    | Plus
    | Times

type BinaryOperation<'operand,'op> = { lhs: 'operand; op: 'op; rhs: 'operand }

type AstNode =
    | BinOp of BinaryOperation<AstNode, Operator>
    | Number of int64

let rec evaluateExpression (expression: AstNode) =
    match expression with
    | Number n -> n
    | BinOp expr ->
        let lhs = evaluateExpression expr.lhs
        let rhs = evaluateExpression expr.rhs

        match expr.op with
        | Plus -> lhs + rhs
        | Times -> lhs * rhs

let createParser plusPrecedence =
    let ws = spaces
    let str_ws s = pstring s >>. ws
    let pNumber = pint64 .>> spaces |>> Number

    let pOperatorPrecedence =
        new OperatorPrecedenceParser<AstNode, unit, unit>()
    let pExpr = pOperatorPrecedence.ExpressionParser
    let pTerm =
        choice [ pNumber .>> ws
                 between (str_ws "(") (str_ws ")") pExpr ]
    pOperatorPrecedence.TermParser <- pTerm
    pOperatorPrecedence.AddOperator(InfixOperator("+", ws, plusPrecedence, Associativity.Left, (fun x y -> BinOp { lhs = x; op = Plus; rhs = y })))
    pOperatorPrecedence.AddOperator(InfixOperator("*", ws, 1, Associativity.Left, (fun x y -> BinOp { lhs = x; op = Times; rhs = y })))
    ///Direct solution: OperatorPrecedenceParser<int64, unit, unit>()
    //pOperatorPrecedence.AddOperator(InfixOperator("+", ws, plusPrecedence, Associativity.Left, (+)))
    //pOperatorPrecedence.AddOperator(InfixOperator("*", ws, 1, Associativity.Left, (*)))

    (fun text ->
        match run pExpr text with
        | Success(ast, _, _) -> ast
        | Failure(err, _, _) -> failwith err)

let data = File.ReadAllText "input/18"
let solution1 = data.Split('\n', StringSplitOptions.RemoveEmptyEntries) |> Array.sumBy ((createParser 1) >> evaluateExpression)
let solution2 = data.Split('\n', StringSplitOptions.RemoveEmptyEntries) |> Array.sumBy ((createParser 2) >> evaluateExpression)
