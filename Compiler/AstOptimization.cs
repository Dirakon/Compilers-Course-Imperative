using Compiler.Utils;

namespace Compiler;

public static class AstOptimization
{
    public static IDeclaration? ExpressionSimplifier(IDeclaration inputDeclaration)
    {
        return inputDeclaration switch
        {
            RoutineDeclaration routineDeclaration => CheckRoutineDeclaration(routineDeclaration),
            VariableDeclaration variableDeclaration => CheckVariableDeclaration(variableDeclaration),
            // TODO: текущая имплементация WithNodesTransformed удаляет все null
            // 1) не обработан TypeDeclaration - они удалятся из AST
            // 2) не обработан TypeDeclaration - в нем ничего не сократится (например размеры внутри массивов)
            TypeDeclaration typeDeclaration => typeDeclaration,
            _ => null
        };
    }

    private static RoutineDeclaration CheckRoutineDeclaration(RoutineDeclaration routineDeclaration)
    {
        return routineDeclaration with { Body = routineDeclaration.Body.WithNodesTransformed(CheckBodyElement) };
    }

    private static IBodyElement? CheckBodyElement(IBodyElement bodyElement)
    {
        return bodyElement switch
        {
            VariableDeclaration variableDeclaration => CheckVariableDeclaration(variableDeclaration),
            TypeDeclaration typeDeclaration => typeDeclaration, // TODO: не обработан TypeDeclaration - в нем ничего не сократится (например размеры внутри массивов)
            Return returnStatement => returnStatement, // TODO: не обработан Return - в нем ничего не сократится (например `return 1 + 1;`)
            RoutineCall routineCall => routineCall, // TODO: не обработан RoutineCall - в нем ничего не сократится (например `Call(1 + 1, 2);`)
            IfStatement ifStatement => CheckIfStatement(ifStatement),
            WhileLoop whileLoop => CheckWhileLoop(whileLoop),
            Assignment assignment => assignment, // TODO: не обработан Assignment - в нем ничего не сократится (например `a := 1 + 1;`)
            ForLoop forLoop => forLoop, // TODO: не обработан ForLoop - в нем ничего не сократится
                                        // a) внутри body
                                        // b) в range
                                        // c) он не удалится кога из-за range итераций не будет
            _ => throw new ArgumentOutOfRangeException(nameof(bodyElement), bodyElement, null)
        };
    }

    private static IfStatement? CheckIfStatement(IfStatement ifStatement)
    {
        ifStatement = ifStatement with { Condition = SimplifyExpression(ifStatement.Condition) };
        // TODO: не обработано тело ifStatement и тело elseStatement
        if (ifStatement.Condition.First.First.First.First is BoolPrimary boolPrimary)
            return boolPrimary.Value ? ifStatement : null;
        return ifStatement;
    }

    private static WhileLoop? CheckWhileLoop(WhileLoop whileLoop)
    {
        whileLoop = whileLoop with { Condition = SimplifyExpression(whileLoop.Condition) };
        // TODO: не обработано тело while
        if (whileLoop.Condition.First.First.First.First is BoolPrimary boolPrimary)
            return boolPrimary.Value ? whileLoop : null;
        return whileLoop;
    }

    private static VariableDeclaration CheckVariableDeclaration(VariableDeclaration variableDeclaration)
    {
        // TODO: variableDeclaration.Type не обработан - там могут быть expression, например
        // a) размер массива
        // b) другие variableDeclaration внутри рекорда
        if (variableDeclaration.Expression != null)
            return variableDeclaration with { Expression = SimplifyExpression(variableDeclaration.Expression) };
        return variableDeclaration;
    }

    public static TPrimary? TrySimplifyToPrimary<TPrimary>(this Expression expression) where TPrimary : class, IPrimary
    {
        var simplifiedExpression = expression.SimplifyExpression();
        var allFactors = simplifiedExpression
            .GetAllFactors();
        return allFactors is [TPrimary singlePrimaryOfRightType]
            ? singlePrimaryOfRightType
            : null;
    }

    public static IFactor[] GetAllFactors(this Expression expression)
    {
        return expression
            .Operations.Select(relOp => relOp.Relation)
            .Prepend(expression.First)
            .SelectMany(
                relation => 
                    new[] { relation.Operation?.Simple, relation.First }.NotNull()
                    .SelectMany(
                        simple =>
                            simple
                                .Operations.Select(sumOp => sumOp.Summand)
                                .Prepend(simple.First)
                                .SelectMany(
                                    summand =>
                                        summand
                                            .Operations.Select(facOp => facOp.Factor)
                                            .Prepend(summand.First))))
            .ToArray();
    }

    public static Expression SimplifyExpression(this Expression expression)
    {
        return expression.Operations is NonEmptyNodeList<RelationOperation> operations
            ? CheckRelationOperations(expression, expression.First, operations)
            : expression with { First = CheckRelation(expression.First) };
    }

    private static Expression CheckRelationOperations(Expression expression, Relation relation,
        IEnumerable<RelationOperation> operations)
    {
        return CheckFactor(relation.First.First.First) switch
        {
            BoolPrimary boolPrimary => SimplifyRelation(expression, boolPrimary, operations),
            ExpressionFactor expressionFactor => CheckRelationOperations(expression, expressionFactor.Expression.First,
                operations),
            _ => expression
        };
    }

    private static Expression SimplifyRelation(Expression expression, BoolPrimary first,
        IEnumerable<RelationOperation> operations)
    {
        var result = first.Value;
        foreach (var op in operations)
        {
            var switchedSummand = CheckSummand(op.Relation.First.First);
            switch (switchedSummand.First)
            {
                case BoolPrimary boolPrimary:
                    result = PerformRelationOperation(result, boolPrimary.Value, op.Type);
                    break;
                case ExpressionFactor expressionFactor
                    when CheckFactor(expressionFactor.Expression.First.First.First.First) is BoolPrimary bP:
                    result = PerformRelationOperation(result, bP.Value, op.Type);
                    break;
                default:
                    return expression;
            }
        }

        return expression with
        {
            First = expression.First with
            {
                First = new Simple(
                    new Summand(new BoolPrimary(result, new CustomLexLocation()),
                        new EmptyNodeList<FactorOperation>(), new CustomLexLocation()),
                    new EmptyNodeList<SummandOperation>(), new CustomLexLocation())
            },
            Operations = new EmptyNodeList<RelationOperation>()
        };
    }

    private static bool PerformRelationOperation(bool first, bool second, RelationOperationType operationType)
    {
        return operationType switch
        {
            RelationOperationType.And => first && second,
            RelationOperationType.Or => first || second,
            RelationOperationType.Xor => first ^ second,
            _ => throw new ArgumentOutOfRangeException(nameof(operationType), operationType, null)
        };
    }

    private static Relation CheckRelation(Relation relation)
    {
        return relation.Operation is { } operation
            ? CheckSimpleOperation(relation, relation.First, operation)
            : relation with { First = CheckSimple(relation.First) };
    }

    private static Relation CheckSimpleOperation(Relation relation, Simple simple,
        SimpleOperation operation)
    {
        return CheckFactor(simple.First.First) switch
        {
            IntegerPrimary integerPrimary => SimplifyIntegerSimple(relation, integerPrimary, operation),
            RealPrimary realPrimary => SimplifyRealSimple(relation, realPrimary, operation),
            ExpressionFactor expressionFactor => CheckSimpleOperation(relation, expressionFactor.Expression.First.First,
                operation),
            _ => relation
        };
    }

    private static Relation SimplifyIntegerSimple(Relation relation, IntegerPrimary first,
        SimpleOperation operation)
    {
        bool result;
        var switchedSummand = CheckSummand(operation.Simple.First);
        switch (switchedSummand.First)
        {
            case IntegerPrimary integerPrimary:
                result = PerformIntSimpleOperation(first.Literal, integerPrimary.Literal, operation.Type);
                break;
            case ExpressionFactor expressionFactor
                when CheckFactor(expressionFactor.Expression.First.First.First.First) is IntegerPrimary iP:
                result = PerformIntSimpleOperation(first.Literal, iP.Literal, operation.Type);
                break;
            default:
                return relation;
        }

        return relation with
        {
            First = new Simple(
                new Summand(new BoolPrimary(result, new CustomLexLocation()), new EmptyNodeList<FactorOperation>(),
                    new CustomLexLocation()), new EmptyNodeList<SummandOperation>(), new CustomLexLocation())
        };
    }

    private static Relation SimplifyRealSimple(Relation relation, RealPrimary first,
        SimpleOperation operation)
    {
        bool result;
        var switchedSummand = CheckSummand(operation.Simple.First);
        switch (switchedSummand.First)
        {
            case IntegerPrimary integerPrimary:
                result = PerformRealSimpleOperation(first.Literal, integerPrimary.Literal, operation.Type);
                break;
            case ExpressionFactor expressionFactor
                when CheckFactor(expressionFactor.Expression.First.First.First.First) is RealPrimary rP:
                result = PerformRealSimpleOperation(first.Literal, rP.Literal, operation.Type);
                break;
            default:
                return relation;
        }

        return relation with
        {
            First = new Simple(
                new Summand(new BoolPrimary(result, new CustomLexLocation()), new EmptyNodeList<FactorOperation>(),
                    new CustomLexLocation()), new EmptyNodeList<SummandOperation>(), new CustomLexLocation())
        };
    }

    private static bool PerformIntSimpleOperation(int first, int second, SimpleOperationType operationType)
    {
        return operationType switch
        {
            SimpleOperationType.Equal => first == second,
            SimpleOperationType.Greater => first > second,
            SimpleOperationType.Less => first < second,
            SimpleOperationType.NotEqual => first != second,
            SimpleOperationType.LessOrEqual => first <= second,
            SimpleOperationType.GreaterOrEqual => first >= second,
            _ => throw new ArgumentOutOfRangeException(nameof(operationType), operationType, null)
        };
    }

    private static bool PerformRealSimpleOperation(double first, double second, SimpleOperationType operationType)
    {
        return operationType switch
        {
            SimpleOperationType.Equal => Math.Abs(first - second) < 0.000000001,
            SimpleOperationType.Greater => first > second,
            SimpleOperationType.Less => first < second,
            SimpleOperationType.NotEqual => Math.Abs(first - second) > 0.000000001,
            SimpleOperationType.LessOrEqual => first <= second,
            SimpleOperationType.GreaterOrEqual => first >= second,
            _ => throw new ArgumentOutOfRangeException(nameof(operationType), operationType, null)
        };
    }

    private static Simple CheckSimple(Simple simple)
    {
        return simple.Operations is NonEmptyNodeList<SummandOperation> operations
            ? CheckSummandOperations(simple, simple.First, operations)
            : simple with { First = CheckSummand(simple.First) };
    }

    private static Simple CheckSummandOperations(Simple simple, Summand summand,
        IEnumerable<SummandOperation> operations)
    {
        return CheckFactor(summand.First) switch
        {
            IntegerPrimary integerPrimary => SimplifyIntegerSummand(simple, integerPrimary, operations),
            RealPrimary realPrimary => SimplifyRealSummand(simple, realPrimary, operations),
            ExpressionFactor expressionFactor => CheckSummandOperations(simple,
                expressionFactor.Expression.First.First.First, operations),
            _ => simple
        };
    }

    private static Simple SimplifyIntegerSummand(Simple simple, IntegerPrimary first,
        IEnumerable<SummandOperation> operations)
    {
        var result = first.Literal;
        foreach (var op in operations)
        {
            var switchedSummand = CheckSummand(op.Summand);
            switch (switchedSummand.First)
            {
                case IntegerPrimary integerPrimary:
                    result = PerformIntSummandOperation(result, integerPrimary.Literal, op.Type);
                    break;
                case ExpressionFactor expressionFactor
                    when CheckFactor(expressionFactor.Expression.First.First.First.First) is IntegerPrimary iP:
                    result = PerformIntSummandOperation(result, iP.Literal, op.Type);
                    break;
                default:
                    return simple;
            }
        }

        return simple with
        {
            First = simple.First with { First = first with { Literal = result }, },
            Operations = new EmptyNodeList<SummandOperation>()
        };
    }

    private static Simple SimplifyRealSummand(Simple simple, RealPrimary first,
        IEnumerable<SummandOperation> operations)
    {
        var result = first.Literal;
        foreach (var op in operations)
        {
            var switchedSummand = CheckSummand(op.Summand);
            switch (switchedSummand.First)
            {
                case RealPrimary realPrimary:
                    result = PerformRealSummandOperation(result, realPrimary.Literal, op.Type);
                    break;
                case ExpressionFactor expressionFactor
                    when CheckFactor(expressionFactor.Expression.First.First.First.First) is RealPrimary rP:
                    result = PerformRealSummandOperation(result, rP.Literal, op.Type);
                    break;
                default:
                    return simple;
            }
        }

        return simple with
        {
            First = simple.First with { First = first with { Literal = result }, },
            Operations = new EmptyNodeList<SummandOperation>()
        };
    }

    private static Summand CheckSummand(Summand summand)
    {
        return summand.Operations is NonEmptyNodeList<FactorOperation> operations
            ? CheckFactorOperations(summand, summand.First, operations)
            : summand with { First = CheckFactor(summand.First) };
    }

    private static Summand CheckFactorOperations(Summand summand, IFactor factor,
        IEnumerable<FactorOperation> operations)
    {
        return CheckFactor(factor) switch
        {
            IntegerPrimary integerPrimary => SimplifyIntegerFactor(summand, integerPrimary, operations),
            RealPrimary realPrimary => SimplifyRealFactor(summand, realPrimary, operations),
            ExpressionFactor expressionFactor => CheckFactorOperations(summand,
                expressionFactor.Expression.First.First.First.First, operations),
            _ => summand
        };
    }

    private static Summand SimplifyIntegerFactor(Summand summand, IntegerPrimary first,
        IEnumerable<FactorOperation> operations)
    {
        var result = first.Literal;
        foreach (var op in operations)
        {
            var newFactor = CheckFactor(op.Factor);
            switch (newFactor)
            {
                case IntegerPrimary integerPrimary:
                    result = PerformIntFactorOperation(result, integerPrimary.Literal, op.Type);
                    break;
                case ExpressionFactor expressionFactor
                    when CheckFactor(expressionFactor.Expression.First.First.First.First) is IntegerPrimary iP:
                    result = PerformIntFactorOperation(result, iP.Literal, op.Type);
                    break;
                default:
                    return summand;
            }
        }

        return summand with
        {
            First = first with { Literal = result }, Operations = new EmptyNodeList<FactorOperation>()
        };
    }

    private static Summand SimplifyRealFactor(Summand summand, RealPrimary first,
        IEnumerable<FactorOperation> operations)
    {
        var result = first.Literal;
        foreach (var op in operations)
        {
            var newFactor = CheckFactor(op.Factor);
            switch (newFactor)
            {
                case RealPrimary realPrimary:
                    result = PerformRealFactorOperation(result, realPrimary.Literal, op.Type);
                    break;
                case ExpressionFactor expressionFactor
                    when CheckFactor(expressionFactor.Expression.First.First.First.First) is RealPrimary rP:
                    result = PerformRealFactorOperation(result, rP.Literal, op.Type);
                    break;
                default:
                    return summand;
            }
        }

        return summand with
        {
            First = first with { Literal = result }, Operations = new EmptyNodeList<FactorOperation>()
        };
    }

    private static int PerformIntFactorOperation(int first, int second, FactorOperationType operationType)
    {
        return operationType switch
        {
            FactorOperationType.Multiplication => first * second,
            FactorOperationType.Division => first / second,
            FactorOperationType.ModularDivision => first % second,
            _ => throw new ArgumentOutOfRangeException(nameof(operationType), operationType, null)
        };
    }

    private static double PerformRealFactorOperation(double first, double second, FactorOperationType operationType)
    {
        return operationType switch
        {
            FactorOperationType.Multiplication => first * second,
            FactorOperationType.Division => first / second,
            FactorOperationType.ModularDivision => first % second,
            _ => throw new ArgumentOutOfRangeException(nameof(operationType), operationType, null)
        };
    }

    private static int PerformIntSummandOperation(int first, int second, SummandOperationType operationType)
    {
        return operationType switch
        {
            SummandOperationType.Plus => first + second,
            SummandOperationType.Minus => first - second,
            _ => throw new ArgumentOutOfRangeException(nameof(operationType), operationType, null)
        };
    }

    private static double PerformRealSummandOperation(double first, double second, SummandOperationType operationType)
    {
        return operationType switch
        {
            SummandOperationType.Plus => first + second,
            SummandOperationType.Minus => first - second,
            _ => throw new ArgumentOutOfRangeException(nameof(operationType), operationType, null)
        };
    }

    private static IFactor CheckFactor(IFactor factor)
    {
        return factor switch
        {
            ModifiablePrimary modifiablePrimary => modifiablePrimary, // TODO: modifiable primary не обработан
                                                                      // - можно упростить например
                                                                      // 1 + a[1 + 1].variable[2 + 3]
            IntegerPrimary integerPrimary => integerPrimary,
            BoolPrimary boolPrimary => boolPrimary,
            RealPrimary realPrimary => realPrimary,
            ExpressionFactor expressionFactor => expressionFactor with
            {
                Expression = SimplifyExpression(expressionFactor.Expression)
            },
            RoutineCall routineCall => routineCall, // TODO: RoutineCall не обработан
                                                // - можно упростить например
                                                // 1 + Call(1 + 1, 2)
            _ => throw new ArgumentOutOfRangeException(nameof(factor), factor, null)
        };
    }
}