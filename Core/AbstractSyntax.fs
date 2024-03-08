namespace Calculator.Core

module AbstractSyntax =
    type Operator =
        | ADD
        | SUB
        | MUL
        | DIV
        | POWER
    
    type Separator = | LPAR | RPAR | COMMA

    type Unary =
        | UMINUS

    type FunctionType = | SQRT

    type Expression =
        | Number of float
        | BinaryOperatorExpressionStart of Expression * Expression
        | BinaryOperatorExpressionList of Operator * Expression * Expression
        | BinaryOperatorExpressionEnd of Operator * Expression
        | UnaryOperatorExpression of Unary * Expression
        | ParanthesisedExpression of Expression
        | FunctionExpression of FunctionType * ParameterList
        | Nothing
    and ParameterList = Expression list



    let isAddition op =
        match op with
        | ADD -> true
        | _ -> false

    let isSubtraction op =
        match op with
        | SUB -> true
        | _ -> false

    let isMultiplication op =
        match op with
        | MUL -> true
        | _ -> false

    let isDivision op =
        match op with
        | DIV -> true
        | _ -> false

    let isUminus unary =
        match unary with
        | UMINUS -> true
        | _ -> false
    
    let isNumber expr =
        match expr with
        | Number x -> true
        | _ -> false

    let isBinaryOperatorExpressionStart expr =
        match expr with
        | BinaryOperatorExpressionStart(x, y) -> true
        | _ -> false

    let isBinaryOperatorExpressionList expr =
        match expr with
        | BinaryOperatorExpressionList(x, y, z) -> true
        | _ -> false

    let isBinaryOperatorExpressionEnd expr =
        match expr with
        | BinaryOperatorExpressionEnd(x, y) -> true
        | _ -> false

    let isUnaryOperatorExpression expr =
        match expr with
        | UnaryOperatorExpression(x, y) -> true
        | _ -> false

    let isParanthesisedExpression expr =
        match expr with
        | ParanthesisedExpression x -> true
        | _ -> false

    let isNothing expr =
        match expr with
        | Nothing -> true
        | _ -> false