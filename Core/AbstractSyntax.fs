namespace Calculator.Core

module AbstractSyntax =
    type Operator =
        | ADD
        | SUB
        | MUL
        | DIV
    
    type Unary =
        | UMINUS

    type Expression =
        | Number of float
        | BinaryOperatorExpressionStart of Expression * Expression
        | BinaryOperatorExpressionList of Operator * Expression *Expression
        | BinaryOperatorExpressionEnd of Operator * Expression
        | UnaryOperatorExpression of Unary * Expression
        | ParanthesisedExpression of Expression
        | Nothing
    
