using System.Diagnostics.Contracts;

namespace Compiler.TypeChecking;

public record TypeInferenceResult(IResolvedType? InferredType, OperationFailure? PossibleError);

public static class TypeInference
{
    [Pure]
    public static TypeInferenceResult TryInferType(this Expression expression, Scope scope)
    {
        var firstInferredType = expression.First
            .TryInferType(scope);
        return firstInferredType
            .AppendError(
                expression.Operations.TryGetInvalidOperationGivenTypeError<RelationOperation, Relation>(
                    firstInferredType.InferredType,
                    scope,
                    isSuitable: (operation, type) => operation.Type switch
                    {
                        RelationOperationType.And => type is ResolvedBoolType,
                        RelationOperationType.Or => type is ResolvedBoolType,
                        RelationOperationType.Xor => type is ResolvedBoolType,
                        _ => throw new ArgumentOutOfRangeException(nameof(operation), operation, null)
                    },
                    getValue: op => op.Relation,
                    getOpName: op => op.Type.ToString(),
                    inferValueType: TryInferType)
            );
    }

    [Pure]
    public static TypeInferenceResult TryInferType(this Relation relation, Scope scope)
    {
        var firstInferredType = relation.First.TryInferType(scope);
        return relation.Operation == null
            ? firstInferredType
            : new TypeInferenceResult(
                new ResolvedBoolType(),
                (firstInferredType, relation.Operation.Simple.TryInferType(scope))
                .AddErrorOnSuccess(
                    (type1, type2) =>
                        TryGetUnsuitableOperationError(
                                relation.Operation,
                                isSuitableFunc: (operation, type) => operation.Type switch
                                {
                                    SimpleOperationType.Less => type is ResolvedRealType or ResolvedIntType,
                                    SimpleOperationType.LessOrEqual => type is ResolvedRealType or ResolvedIntType,
                                    SimpleOperationType.Greater => type is ResolvedRealType or ResolvedIntType,
                                    SimpleOperationType.GreaterOrEqual => type is ResolvedRealType
                                        or ResolvedIntType,
                                    SimpleOperationType.Equal => type is ResolvedRealType or ResolvedIntType
                                        or ResolvedBoolType,
                                    SimpleOperationType.NotEqual => type is ResolvedRealType or ResolvedIntType
                                        or ResolvedBoolType,
                                    _ => throw new ArgumentOutOfRangeException(nameof(operation), operation, null)
                                },
                                getOpName: op => op.Type.ToString(),
                                type1, relation.Operation.LexLocation)
                            .TryAdd(type2.TryGetWrongTypeError(type1, relation.Operation.LexLocation)))
            );
    }

    [Pure]
    public static TypeInferenceResult TryInferType(this Simple simple, Scope scope)
    {
        var firstInferredType = simple.First
            .TryInferType(scope);
        return firstInferredType
            .AppendError(
                simple.Operations.TryGetInvalidOperationGivenTypeError<SummandOperation, Summand>(
                    firstInferredType.InferredType,
                    scope,
                    isSuitable: (operation, type) => operation.Type switch
                    {
                        SummandOperationType.Plus => type is ResolvedRealType or ResolvedIntType,
                        SummandOperationType.Minus => type is ResolvedRealType or ResolvedIntType,
                        _ => throw new ArgumentOutOfRangeException(nameof(operation), operation, null)
                    },
                    getValue: op => op.Summand,
                    getOpName: op => op.Type.ToString(),
                    inferValueType: TryInferType));
    }

    [Pure]
    public static TypeInferenceResult TryInferType(this Summand summand, Scope scope)
    {
        var firstInferredType = summand.First
            .TryInferType(scope);
        return firstInferredType
            .AppendError(
                summand.Operations.TryGetInvalidOperationGivenTypeError<FactorOperation, IFactor>(
                    firstInferredType.InferredType,
                    scope,
                    isSuitable: (operation, type) => operation.Type switch
                    {
                        FactorOperationType.Multiplication => type is ResolvedIntType or ResolvedRealType,
                        FactorOperationType.Division => type is ResolvedIntType or ResolvedRealType,
                        FactorOperationType.ModularDivision => type is ResolvedIntType or ResolvedRealType,
                        _ => throw new ArgumentOutOfRangeException(nameof(operation), operation, null)
                    },
                    getValue: op => op.Factor,
                    getOpName: op => op.Type.ToString(),
                    inferValueType: TryInferType));
    }

    [Pure]
    public static OperationFailure? TryGetInvalidOperationGivenTypeError<TOp, TVal>(this INodeList<TOp> operations,
        IResolvedType? maybeRequiredType,
        Scope scope,
        Func<TOp, IResolvedType, bool> isSuitable,
        Func<TOp, TVal> getValue,
        Func<TOp, string> getOpName,
        Func<TVal, Scope, TypeInferenceResult> inferValueType) where TOp : INode where TVal : INode
    {
        return operations switch
        {
            EmptyNodeList<TOp> => null,
            NonEmptyNodeList<TOp>(var op, var otherOps, var lexLocation) =>
                ((inferValueType(getValue(op), scope), requiredType: maybeRequiredType) switch
                {
                    ((null, var errors), null) => errors,
                    (({ } inferredType, var errors), null) => errors
                        .TryAdd(TryGetUnsuitableOperationError(op, isSuitable, getOpName, inferredType, lexLocation)),
                    ((null, var errors), { } requiredType) => errors
                        .TryAdd(TryGetUnsuitableOperationError(op, isSuitable, getOpName, requiredType, lexLocation)),
                    (({ } inferredType, var errors), { } requiredType) => errors
                        .TryAdd(TryGetUnsuitableOperationError(op, isSuitable, getOpName, requiredType, lexLocation))
                        .TryAdd(inferredType.TryGetWrongTypeError(requiredType, lexLocation))
                })
                .TryAdd(otherOps.TryGetInvalidOperationGivenTypeError(maybeRequiredType, scope, isSuitable, getValue,
                    getOpName, inferValueType)),
            _ => throw new ArgumentOutOfRangeException(nameof(operations))
        };
    }

    [Pure]
    public static OperationFailure? TryGetUnsuitableOperationError<TOp>(
        TOp op,
        Func<TOp, IResolvedType, bool> isSuitableFunc,
        Func<TOp, string> getOpName,
        IResolvedType requiredType,
        CustomLexLocation lexLocation)
    {
        var isSuitable = isSuitableFunc(op, requiredType);
        if (isSuitable)
        {
            return null;
        }

        return new TypeCheckerError(
            $"Cannot use operation {getOpName(op)} on type {requiredType.GetTypeName()}",
            new[] { lexLocation }).ToFailure();
    }

    [Pure]
    public static TypeInferenceResult TryInferType(this IFactor factor, Scope scope) =>
        factor switch
        {
            ExpressionFactor expressionFactor => expressionFactor.Expression.TryInferType(scope),
            BoolPrimary => new TypeInferenceResult(new ResolvedBoolType(), null),
            IntegerPrimary => new TypeInferenceResult(new ResolvedIntType(), null),
            RealPrimary => new TypeInferenceResult(new ResolvedRealType(), null),
            ModifiablePrimary modifiablePrimary => modifiablePrimary.TryInferModifiablePrimaryType(scope),
            RoutineCall routineCall =>
                scope.TryGetRoutine(routineCall.RoutineName, routineCall.LexLocation) switch
                {
                    ({ ReturnType: ResolvedDeclaredRoutineReturnType({ } someType) } declaredRoutine, var errors) =>
                        new TypeInferenceResult(someType, null)
                            .AppendError(
                                TypeChecker.GetBadArgumentsProvidedError(
                                    declaredRoutine.Arguments.Select(arg => arg.Type).ToArray(),
                                    routineCall.Arguments,
                                    routineCall.LexLocation, scope))
                            .PrependError(errors),
                    ({ } declaredRoutine, var errors) =>
                        new TypeCheckerError(
                                $"Usage of routine without return {routineCall.RoutineName} in expression",
                                new[] { routineCall.LexLocation })
                            .ToTypeInferenceFailure()
                            .AppendError(
                                TypeChecker.GetBadArgumentsProvidedError(
                                    declaredRoutine.Arguments.Select(arg => arg.Type).ToArray(),
                                    routineCall.Arguments,
                                    routineCall.LexLocation, scope))
                            .PrependError(errors),
                    (null, var errors) =>
                        errors.ToTypeInferenceFailure()
                            .AppendError(OperationFailure.CombineErrors(
                                routineCall.Arguments
                                    .Select(arg => arg.TryInferType(scope).PossibleError)
                                    .NotNull()))
                }
        };

    [Pure]
    public static TypeInferenceResult TryInferModifiablePrimaryType(
        this ModifiablePrimary modifiablePrimary,
        Scope scope)
    {
        return scope.TryGetVariable(modifiablePrimary.Identifier, modifiablePrimary.LexLocation) switch
        {
            ({ } someVariable, var errors) =>
                someVariable.Type
                    .TryInferModifiablePrimaryType(modifiablePrimary.Operations)
                    .PrependError(errors),
            (null, var errors) =>
                // TODO: typecheck array indexing anyways to int. do this also on failures below
                errors.ToTypeInferenceFailure()
        };
    }

    [Pure]
    private static TypeInferenceResult TryInferModifiablePrimaryType(this IResolvedType currentType,
        INodeList<IModifiablePrimaryOperation> operations)
    {
        return (currentType, operations) switch
        {
            (ResolvedArrayType type,
                NonEmptyNodeList<IModifiablePrimaryOperation>(ArrayIndexing, var otherOps, _)
                ) =>
                type.UnderlyingType.TryInferModifiablePrimaryType(otherOps),
            (ResolvedRecordType type,
                NonEmptyNodeList<IModifiablePrimaryOperation>(
                MemberCall(var memberName, _),
                var otherOps,
                var lexLocation)
                ) =>
                type.Variables.GetValueOrDefault(memberName) switch
                {
                    null => new TypeCheckerError($"Cannot get member {memberName} on type {type.GetTypeName()}",
                        new[] { lexLocation }).ToTypeInferenceFailure(),
                    { } someMemberType => someMemberType.TryInferModifiablePrimaryType(otherOps)
                },
            ({ } type, EmptyNodeList<IModifiablePrimaryOperation>) => new TypeInferenceResult(type, null),
            ({ } type,
                NonEmptyNodeList<IModifiablePrimaryOperation>(MemberCall, _, var lexLocation)
                ) =>
                new TypeCheckerError($"Cannot apply member call operation on type {type.GetTypeName()}",
                    new[] { lexLocation }).ToTypeInferenceFailure(),
            ({ } type,
                NonEmptyNodeList<IModifiablePrimaryOperation>(ArrayIndexing, _, var lexLocation)
                ) =>
                new TypeCheckerError($"Cannot apply array indexing operation on type {type.GetTypeName()}",
                    new[] { lexLocation }).ToTypeInferenceFailure(),
            _ => throw new ArgumentOutOfRangeException()
        };
    }
}