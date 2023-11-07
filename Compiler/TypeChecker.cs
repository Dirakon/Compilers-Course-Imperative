using System.Collections.Immutable;
using System.Diagnostics.Contracts;

namespace Compiler;

public static class TypeChecker
{
    public static TypeCheckerError[]? TypeCheck(Program program)
    {
        var currentScope = new Scope(ImmutableDictionary<string, IDeclaredEntity>.Empty);
        var failure = (OperationFailure?)null;
        
        var allGlobalTypeDeclarations = program.Declarations
            .OfSubType<IDeclaration, TypeDeclaration>();
        (currentScope, failure) = ExtractGlobalDeclarations(allGlobalTypeDeclarations, currentScope, failure);
        
        var allGlobalRoutineDeclarations = program.Declarations
            .OfSubType<IDeclaration, RoutineDeclaration>();
        (currentScope, failure) = ExtractGlobalDeclarations(allGlobalRoutineDeclarations, currentScope, failure);
        
        var allGlobalVariableDeclarations = program.Declarations
            .OfSubType<IDeclaration, VariableDeclaration>();
        (currentScope, failure) = ExtractGlobalDeclarations(allGlobalVariableDeclarations, currentScope, failure);

        var declarationLocationsGroupedByIdentifier = program.Declarations
            .GroupBy(
                keySelector: declaration => declaration.GetIdentifier(),
                elementSelector: declaration => declaration.LexLocation);
        failure = failure.TryAdd(
            OperationFailure.CombineErrors(
                declarationLocationsGroupedByIdentifier
                    .Where(lexLocationGroup => lexLocationGroup.Count() > 1)
                    .Select(lexLocationGroup =>
                        new OperationFailure(new[]
                        {
                            new TypeCheckerError(
                                $"Duplicate declarations of {lexLocationGroup.Key} found",
                                lexLocationGroup.ToArray())
                        })))
        );
        return failure?.Errors;
    }

    [Pure]
    private static (Scope currentScope, OperationFailure? failure) ExtractGlobalDeclarations<T>(
        IEnumerable<T> allGlobalVariableDeclarations, Scope currentScope, OperationFailure? failure) where T : IDeclaration
    {
        // Note that we iterate through type declarations from top to bottom.
        // There might be a smart way to rearrange them to allow out-of-order non-recursive type declarations.
        foreach (var declaredVariable in allGlobalVariableDeclarations)
        {
            switch (declaredVariable.AsDeclaredEntity(currentScope))
            {
                case DeclarationResolveFailure<IDeclaredEntity> declarationResolveFailure:
                    failure = failure.TryAdd(declarationResolveFailure.AsGeneralFailure());
                    break;
                case SuccessfulDeclarationResolve<IDeclaredEntity>(var declaredType):
                    if (!currentScope.DeclaredEntities.ContainsKey(declaredVariable.GetIdentifier()))
                    {
                        currentScope = new Scope(DeclaredEntities: currentScope.DeclaredEntities.Add(
                            declaredVariable.GetIdentifier(),
                            declaredType));
                    }
                    break;
                default:
                    throw new ArgumentOutOfRangeException();
            }
        }

        return (currentScope, failure);
    }

    [Pure]
    public static OperationFailure? TryAdd(this OperationFailure? error1Maybe, OperationFailure? error2Maybe)
    {
        return (error1Maybe, error2Maybe) switch
        {
            (null, null) => null,
            ({ } error1, null) => error1,
            (null, { } error2) => error2,
            ({ } error1, { } error2) => error1.Add(error2)
        };
    }

    [Pure]
    public static ITypeInferenceResult OrElse(this OperationFailure? maybeError, ITypeInferenceResult elseResult)
    {
        return maybeError ?? elseResult;
    }


    [Pure]
    public static ITypeInferenceResult OrElse(this OperationFailure? error1Maybe, IResolvedType elseType)
    {
        return error1Maybe.OrElse(new SuccessfulTypeInference(elseType));
    }
}

public record TypeCheckerError(string Message, CustomLexLocation[] Locations);

public record Scope(ImmutableDictionary<string, IDeclaredEntity> DeclaredEntities);

public interface IDeclaredEntity
{
    string Identifier { get; init; }
}

public record DeclaredVariable(string Identifier, IResolvedType Type) : IDeclaredEntity;

public record DeclaredType(string Identifier, IResolvedType Type) : IDeclaredEntity;

public record DeclaredRoutine
    (string Identifier, IResolvedType? ReturnType, IResolvedType[] ArgumentTypes) : IDeclaredEntity;

public interface ITypeInferenceResult
{
}

public record SuccessfulTypeInference(IResolvedType InferredType) : ITypeInferenceResult;

public record OperationFailure(TypeCheckerError[] Errors) : ITypeInferenceResult
{
    [Pure]
    public OperationFailure Add(OperationFailure? other)
    {
        return new OperationFailure(Errors.Concat(other?.Errors ?? Array.Empty<TypeCheckerError>()).ToArray());
    }

    [Pure]
    public static OperationFailure? CombineErrors(IEnumerable<OperationFailure> failures)
    {
        return failures.Aggregate<OperationFailure, OperationFailure?>(
            null,
            (accumulator, nextFailure) => accumulator.TryAdd(nextFailure));
    }

    [Pure]
    public DeclarationResolveFailure<T> AsDeclarationResolveFailure<T>() where T : IDeclaredEntity
    {
        return new DeclarationResolveFailure<T>(Errors);
    }
}

public interface IDeclarationResolveResult<T> where T : IDeclaredEntity
{
}

public record SuccessfulDeclarationResolve<T>(T DeclaredEntity) : IDeclarationResolveResult<T>
    where T : IDeclaredEntity;

public record DeclarationResolveFailure<T>(TypeCheckerError[] Errors) : IDeclarationResolveResult<T>
    where T : IDeclaredEntity
{
    [Pure]
    public OperationFailure AsGeneralFailure()
    {
        return new OperationFailure(Errors);
    }
}

public static class TypeCheckingAstExtensions
{
    [Pure]
    public static IDeclarationResolveResult<IDeclaredEntity> AsDeclaredEntity(
        this IDeclaration declaration, Scope scope)
    {
        return declaration switch
        {
            TypeDeclaration typeDeclaration => typeDeclaration.AsDeclaredEntity(scope) switch
            {
                DeclarationResolveFailure<DeclaredType>(var errors) => new DeclarationResolveFailure<IDeclaredEntity>(errors),
                SuccessfulDeclarationResolve<DeclaredType>(var type) => new SuccessfulDeclarationResolve<IDeclaredEntity>(type),
                _ => throw new ArgumentOutOfRangeException()
            },
            VariableDeclaration variableDeclaration => variableDeclaration.AsDeclaredEntity(scope) switch
            {
                DeclarationResolveFailure<DeclaredVariable>(var errors) => new DeclarationResolveFailure<IDeclaredEntity>(errors),
                SuccessfulDeclarationResolve<DeclaredVariable>(var variable) => new SuccessfulDeclarationResolve<IDeclaredEntity>(variable),
                _ => throw new ArgumentOutOfRangeException()
            },
            RoutineDeclaration routineDeclaration => routineDeclaration.AsDeclaredEntity(scope) switch
            {
                DeclarationResolveFailure<DeclaredRoutine>(var errors) => new DeclarationResolveFailure<IDeclaredEntity>(errors),
                SuccessfulDeclarationResolve<DeclaredRoutine>(var routine) => new SuccessfulDeclarationResolve<IDeclaredEntity>(routine),
                _ => throw new ArgumentOutOfRangeException()
            },
            _ => throw new ArgumentOutOfRangeException(nameof(declaration))
        };
    }
    public static string GetIdentifier(
        this IDeclaration declaration)
    {
        return declaration switch
        {
            TypeDeclaration typeDeclaration => typeDeclaration.Name,
            VariableDeclaration variableDeclaration => variableDeclaration.Name,
            RoutineDeclaration routineDeclaration => routineDeclaration.RoutineName,
            _ => throw new ArgumentOutOfRangeException(nameof(declaration))
        };
    }

    [Pure]
    public static IDeclarationResolveResult<DeclaredVariable> AsDeclaredEntity(
        this VariableDeclaration variableDeclaration, Scope scope)
    {
        var inferredTypeMaybe = (variableDeclaration.Type, variableDeclaration.Expression) switch
        {
            (null, null) => throw new ArgumentOutOfRangeException(nameof(variableDeclaration)),
            (null, { } expression) => expression.TryInferType(scope),
            ({ } type, null) => type.TryResolveType(scope),
            ({ } type, { } expression) => (type.TryResolveType(scope), expression.TryInferType(scope)) switch
            {
                (OperationFailure failure1, OperationFailure failure2) => failure1.Add(failure2),
                (OperationFailure failure, _) => failure,
                (_, OperationFailure failure) => failure,
                (SuccessfulTypeInference(var specifiedType), SuccessfulTypeInference(var inferredType)) => inferredType
                    .IsConvertibleTo(specifiedType)
                    ? new SuccessfulTypeInference(specifiedType)
                    : new OperationFailure(new[]
                    {
                        new TypeCheckerError(
                            $"Cannot do type conversion for variable declaration. Specified: {specifiedType.GetTypeName()}, got: {inferredType.GetTypeName()}",
                            new[] { variableDeclaration.LexLocation })
                    }),
                _ => throw new ArgumentOutOfRangeException()
            }
        };
        return inferredTypeMaybe switch
        {
            OperationFailure operationFailure => operationFailure.AsDeclarationResolveFailure<DeclaredVariable>(),
            SuccessfulTypeInference(var inferredType) => new SuccessfulDeclarationResolve<DeclaredVariable>(
                new DeclaredVariable(variableDeclaration.Name,
                    inferredType)),
            _ => throw new ArgumentOutOfRangeException(nameof(inferredTypeMaybe))
        };
    }

    [Pure]
    public static IDeclarationResolveResult<DeclaredType> AsDeclaredEntity(this TypeDeclaration typeDeclaration,
        Scope scope)
    {
        return typeDeclaration.Type.TryResolveType(scope) switch
        {
            OperationFailure operationFailure => operationFailure.AsDeclarationResolveFailure<DeclaredType>(),
            SuccessfulTypeInference(var inferredType) => new SuccessfulDeclarationResolve<DeclaredType>(
                new DeclaredType(typeDeclaration.Name, inferredType)),
            _ => throw new ArgumentOutOfRangeException()
        };
    }

    [Pure]
    public static IDeclarationResolveResult<DeclaredRoutine> AsDeclaredEntity(
        this RoutineDeclaration routineDeclaration, Scope scope)
    {
        var maybeArgumentTypes = routineDeclaration.Parameters
            .Select(parameter => parameter.Type.TryResolveType(scope))
            .ToArray();
        var resolvingError = OperationFailure.CombineErrors(
            maybeArgumentTypes.OfSubType<ITypeInferenceResult, OperationFailure>());
        if (resolvingError != null)
        {
            return resolvingError.AsDeclarationResolveFailure<DeclaredRoutine>();
        }

        var argumentTypes = maybeArgumentTypes
            .OfSubType<ITypeInferenceResult, SuccessfulTypeInference>()
            .Select(success => success.InferredType)
            .ToArray();

        return routineDeclaration.ReturnType?.TryResolveType(scope) switch
        {
            null => new SuccessfulDeclarationResolve<DeclaredRoutine>(
                new DeclaredRoutine(
                    routineDeclaration.RoutineName,
                    null,
                    argumentTypes
                )),
            SuccessfulTypeInference successfulTypeInference => new SuccessfulDeclarationResolve<DeclaredRoutine>(
                new DeclaredRoutine(
                    routineDeclaration.RoutineName,
                    successfulTypeInference.InferredType,
                    argumentTypes
                )),
            OperationFailure operationFailure => operationFailure.AsDeclarationResolveFailure<DeclaredRoutine>(),
            _ => throw new ArgumentOutOfRangeException()
        };
    }

    [Pure]
    private static ITypeInferenceResult TryInferType(this Expression expression, Scope scope)
    {
        var firstInferredType = expression.First.TryInferType(scope);
        return firstInferredType.FlatMap(resolvedType =>
            expression.Operations.TryGetInvalidOperationGivenTypeError(resolvedType, scope).OrElse(resolvedType));
    }

    [Pure]
    private static OperationFailure? TryGetInvalidOperationGivenTypeError(this INodeList<RelationOperation> operations,
        IResolvedType requiredType, Scope scope)
    {
        return operations switch
        {
            EmptyNodeList<RelationOperation> => null,
            NonEmptyNodeList<RelationOperation>(var op, var otherOps, var lexLocation) =>
                op.Type.TryGetUnsuitableOperationError(requiredType, lexLocation)
                    .TryAdd(op.Relation.TryInferType(scope) switch
                    {
                        OperationFailure operationFailure => operationFailure,
                        SuccessfulTypeInference(var inferredType) => inferredType.TryGetWrongTypeError(requiredType,
                            lexLocation),
                        _ => throw new ArgumentOutOfRangeException()
                    })
                    .TryAdd(otherOps.TryGetInvalidOperationGivenTypeError(requiredType, scope)),
            _ => throw new ArgumentOutOfRangeException(nameof(operations))
        };
    }

    [Pure]
    private static OperationFailure? TryGetUnsuitableOperationError(this RelationOperationType operation,
        IResolvedType type, CustomLexLocation lexLocation)
    {
        var isSuitable = operation switch
        {
            RelationOperationType.And => type is ResolvedBoolType,
            RelationOperationType.Or => type is ResolvedBoolType,
            RelationOperationType.Xor => type is ResolvedBoolType,
            _ => throw new ArgumentOutOfRangeException(nameof(operation), operation, null)
        };
        if (isSuitable)
        {
            return null;
        }

        return new OperationFailure(new[]
        {
            new TypeCheckerError($"Cannot use operation {operation} on type {type.GetTypeName()}",
                new[] { lexLocation })
        });
    }

    [Pure]
    private static ITypeInferenceResult TryInferType(this Relation relation, Scope scope)
    {
        var firstInferredType = relation.First.TryInferType(scope);
        return relation.Operation == null
            ? firstInferredType
            : firstInferredType
                .CombineWith(
                    relation.Operation.Simple.TryInferType(scope),
                    (type1, type2) => relation.Operation.Type
                        .TryGetUnsuitableOperationError(type1, relation.Operation.LexLocation)
                        .TryAdd(type2.TryGetWrongTypeError(type1, relation.Operation.LexLocation))
                        .OrElse(new ResolvedBoolType())
                );
    }

    [Pure]
    private static OperationFailure? TryGetUnsuitableOperationError(this SimpleOperationType operation,
        IResolvedType type, CustomLexLocation lexLocation)
    {
        var isSuitable = operation switch
        {
            SimpleOperationType.Less => type is ResolvedRealType or ResolvedIntType,
            SimpleOperationType.LessOrEqual => type is ResolvedRealType or ResolvedIntType,
            SimpleOperationType.Greater => type is ResolvedRealType or ResolvedIntType,
            SimpleOperationType.GreaterOrEqual => type is ResolvedRealType or ResolvedIntType,
            SimpleOperationType.Equal => type is ResolvedRealType or ResolvedIntType or ResolvedBoolType,
            SimpleOperationType.NotEqual => type is ResolvedRealType or ResolvedIntType or ResolvedBoolType,
            _ => throw new ArgumentOutOfRangeException(nameof(operation), operation, null)
        };
        if (isSuitable)
        {
            return null;
        }

        return new OperationFailure(new[]
        {
            new TypeCheckerError($"Cannot use operation {operation} on type {type.GetTypeName()}",
                new[] { lexLocation })
        });
    }

    [Pure]
    private static ITypeInferenceResult TryInferType(this Simple simple, Scope scope)
    {
        var firstInferredType = simple.First.TryInferType(scope);
        return firstInferredType.FlatMap(resolvedType =>
            simple.Operations.TryGetInvalidOperationGivenTypeError(resolvedType, scope).OrElse(resolvedType));
    }

    [Pure]
    private static OperationFailure? TryGetInvalidOperationGivenTypeError(this INodeList<SummandOperation> operations,
        IResolvedType requiredType, Scope scope)
    {
        return operations switch
        {
            EmptyNodeList<SummandOperation> => null,
            NonEmptyNodeList<SummandOperation>(var op, var otherOps, var lexLocation) =>
                op.Type.TryGetUnsuitableOperationError(requiredType, lexLocation)
                    .TryAdd(op.Summand.TryInferType(scope) switch
                    {
                        OperationFailure operationFailure => operationFailure,
                        SuccessfulTypeInference(var inferredType) => inferredType.TryGetWrongTypeError(requiredType,
                            lexLocation),
                        _ => throw new ArgumentOutOfRangeException()
                    })
                    .TryAdd(otherOps.TryGetInvalidOperationGivenTypeError(requiredType, scope)),
            _ => throw new ArgumentOutOfRangeException(nameof(operations))
        };
    }

    [Pure]
    private static OperationFailure? TryGetUnsuitableOperationError(this SummandOperationType operation,
        IResolvedType type, CustomLexLocation lexLocation)
    {
        var isSuitable = operation switch
        {
            SummandOperationType.Plus => type is ResolvedRealType or ResolvedIntType,
            SummandOperationType.Minus => type is ResolvedRealType or ResolvedIntType,
            _ => throw new ArgumentOutOfRangeException(nameof(operation), operation, null)
        };
        if (isSuitable)
        {
            return null;
        }

        return new OperationFailure(new[]
        {
            new TypeCheckerError($"Cannot use operation {operation} on type {type.GetTypeName()}",
                new[] { lexLocation })
        });
    }

    [Pure]
    private static ITypeInferenceResult TryInferType(this Summand summand, Scope scope)
    {
        var firstInferredType = summand.First.TryInferType(scope);
        return firstInferredType.FlatMap(resolvedType =>
            summand.Operations.TryGetInvalidOperationGivenTypeError(resolvedType, scope).OrElse(resolvedType));
    }

    [Pure]
    private static OperationFailure? TryGetInvalidOperationGivenTypeError(this INodeList<FactorOperation> operations,
        IResolvedType requiredType, Scope scope)
    {
        return operations switch
        {
            EmptyNodeList<FactorOperation> => null,
            NonEmptyNodeList<FactorOperation>(var op, var otherOps, var lexLocation) =>
                op.Type.TryGetUnsuitableOperationError(requiredType, lexLocation)
                    .TryAdd(op.Factor.TryInferType(scope) switch
                    {
                        OperationFailure operationFailure => operationFailure,
                        SuccessfulTypeInference(var inferredType) => inferredType.TryGetWrongTypeError(requiredType,
                            lexLocation),
                        _ => throw new ArgumentOutOfRangeException()
                    })
                    .TryAdd(otherOps.TryGetInvalidOperationGivenTypeError(requiredType, scope)),
            _ => throw new ArgumentOutOfRangeException(nameof(operations))
        };
    }

    [Pure]
    private static OperationFailure? TryGetUnsuitableOperationError(this FactorOperationType operation,
        IResolvedType type, CustomLexLocation lexLocation)
    {
        var isSuitable = operation switch
        {
            FactorOperationType.Multiplication => type is ResolvedIntType or ResolvedRealType,
            FactorOperationType.Division => type is ResolvedIntType or ResolvedRealType,
            FactorOperationType.ModularDivision => type is ResolvedIntType or ResolvedRealType,
            _ => throw new ArgumentOutOfRangeException(nameof(operation), operation, null)
        };
        if (isSuitable)
        {
            return null;
        }

        return new OperationFailure(new[]
        {
            new TypeCheckerError($"Cannot use operation {operation} on type {type.GetTypeName()}",
                new[] { lexLocation })
        });
    }

    [Pure]
    private static OperationFailure? TryGetWrongTypeError(this IResolvedType someType, IResolvedType requiredType,
        CustomLexLocation lexLocation)
    {
        if (someType.IsEquivalentTo(requiredType))
        {
            return null;
        }

        return new OperationFailure(new[]
        {
            new TypeCheckerError($"Different types used in one expression: {requiredType} and {someType}",
                new[] { lexLocation })
        });
    }

    [Pure]
    public static ITypeInferenceResult TryInferType(this IFactor factor, Scope scope)
    {
        return factor switch
        {
            ExpressionFactor expressionFactor => expressionFactor.Expression.TryInferType(scope),
            BoolPrimary => new SuccessfulTypeInference(new ResolvedBoolType()),
            IntegerPrimary => new SuccessfulTypeInference(new ResolvedIntType()),
            RealPrimary => new SuccessfulTypeInference(new ResolvedRealType()),
            ModifiablePrimary modifiablePrimary => modifiablePrimary.TryInferModifiablePrimaryType(scope),
            RoutineCall routineCall =>
                scope.TryGetRoutine(routineCall.RoutineName, routineCall.LexLocation) switch
                {
                    DeclarationResolveFailure<DeclaredRoutine> declarationResolveFailure => declarationResolveFailure
                        .AsGeneralFailure(),
                    SuccessfulDeclarationResolve<DeclaredRoutine>(var declaredRoutine) => declaredRoutine.ReturnType ==
                        null
                            ? new OperationFailure(new[]
                            {
                                new TypeCheckerError(
                                    $"Usage of routine without return {routineCall.RoutineName} in expression",
                                    new[] { routineCall.LexLocation })
                            })
                            : new SuccessfulTypeInference(declaredRoutine.ReturnType),
                    _ => throw new ArgumentOutOfRangeException()
                },
            _ => throw new ArgumentOutOfRangeException(nameof(factor))
        };
    }

    [Pure]
    private static ITypeInferenceResult TryInferModifiablePrimaryType(
        this ModifiablePrimary modifiablePrimary,
        Scope scope)
    {
        return scope.TryGetVariable(modifiablePrimary.Identifier, modifiablePrimary.LexLocation) switch
        {
            DeclarationResolveFailure<DeclaredVariable> declarationResolveFailure => declarationResolveFailure
                .AsGeneralFailure(),
            SuccessfulDeclarationResolve<DeclaredVariable> successfulDeclarationResolve => successfulDeclarationResolve
                .DeclaredEntity.Type
                .TryInferModifiablePrimaryType(modifiablePrimary.Operations),
            _ => throw new ArgumentOutOfRangeException()
        };
    }

    [Pure]
    private static ITypeInferenceResult TryInferModifiablePrimaryType(this IResolvedType currentType,
        INodeList<IModifiablePrimaryOperation> operations)
    {
        return (currentType, operations) switch
        {
            (ResolvedArrayType type,
                NonEmptyNodeList<IModifiablePrimaryOperation>(ArrayIndexing, var otherOps, _)
                ) =>
                type.TryInferModifiablePrimaryType(otherOps),
            (ResolvedRecordType type,
                NonEmptyNodeList<IModifiablePrimaryOperation>(
                MemberCall(var memberName, _),
                var otherOps,
                var lexLocation)
                ) =>
                type.Variables.GetValueOrDefault(memberName) switch
                {
                    null => new OperationFailure(new[]
                    {
                        new TypeCheckerError($"Cannot get member {memberName} on type {type.GetTypeName()}",
                            new[] { lexLocation })
                    }),
                    { } someMemberType => someMemberType.TryInferModifiablePrimaryType(otherOps)
                },
            ({ } type, EmptyNodeList<IModifiablePrimaryOperation>) => new SuccessfulTypeInference(type),
            ({ } type,
                NonEmptyNodeList<IModifiablePrimaryOperation>(MemberCall, _, var lexLocation)
                ) => new OperationFailure(new[]
                {
                    new TypeCheckerError($"Cannot apply member call operation on type {type.GetTypeName()}",
                        new[] { lexLocation })
                }),
            ({ } type,
                NonEmptyNodeList<IModifiablePrimaryOperation>(ArrayIndexing, _, var lexLocation)
                ) => new OperationFailure(new[]
                {
                    new TypeCheckerError($"Cannot apply array indexing operation on type {type.GetTypeName()}",
                        new[] { lexLocation })
                }),
            _ => throw new ArgumentOutOfRangeException()
        };
    }

    [Pure]
    public static ITypeInferenceResult FlatMap(this ITypeInferenceResult first,
        Func<IResolvedType, ITypeInferenceResult> funcOnResolvedType)
    {
        return first switch
        {
            OperationFailure operationFailure => operationFailure,
            SuccessfulTypeInference(var inferredType) => funcOnResolvedType(inferredType) switch
            {
                OperationFailure operationFailure => operationFailure,
                SuccessfulTypeInference successfulTypeInference => successfulTypeInference,
                _ => throw new ArgumentOutOfRangeException()
            },
            _ => throw new ArgumentOutOfRangeException(nameof(first))
        };
    }

    [Pure]
    public static ITypeInferenceResult Map(this ITypeInferenceResult first,
        Func<IResolvedType, IResolvedType> funcOnResolvedType)
    {
        return first.FlatMap(type => new SuccessfulTypeInference(funcOnResolvedType(type)));
    }

    [Pure]
    public static ITypeInferenceResult CombineWith(this ITypeInferenceResult first, ITypeInferenceResult second,
        Func<IResolvedType, IResolvedType, ITypeInferenceResult> combiningFunc)
    {
        return (first, second) switch
        {
            (SuccessfulTypeInference(var type1), SuccessfulTypeInference(var type2)) => combiningFunc(type1, type2),
            (OperationFailure err1, var maybeError2) => err1.Add(maybeError2 as OperationFailure),
            (_, OperationFailure err1) => err1,
            _ => throw new ArgumentOutOfRangeException(nameof(first))
        };
    }

    [Pure]
    public static ITypeInferenceResult CombineWith(this ITypeInferenceResult first, ITypeInferenceResult second,
        Func<IResolvedType, IResolvedType, IResolvedType> combiningFunc)
    {
        return first.CombineWith(second, (type1, type2) => new SuccessfulTypeInference(combiningFunc(type1, type2)));
    }


    [Pure]
    private static string GetEntityTypeName<T>(T value) where T : IDeclaredEntity
    {
        return value switch
        {
            DeclaredRoutine => "routine",
            DeclaredType => "type",
            DeclaredVariable => "variable",
            _ => throw new ArgumentOutOfRangeException(nameof(value))
        };
    }

    [Pure]
    private static IDeclarationResolveResult<DeclaredRoutine> TryGetRoutine(this Scope scope, string identifier,
        CustomLexLocation lexLocation)
    {
        return scope.TryGetEntityOfType<DeclaredRoutine>(identifier, lexLocation, "routine");
    }

    [Pure]
    private static IDeclarationResolveResult<DeclaredVariable> TryGetVariable(this Scope scope, string identifier,
        CustomLexLocation lexLocation)
    {
        return scope.TryGetEntityOfType<DeclaredVariable>(identifier, lexLocation, "variable");
    }

    [Pure]
    private static IDeclarationResolveResult<DeclaredType> TryGetType(this Scope scope, string identifier,
        CustomLexLocation lexLocation)
    {
        return scope.TryGetEntityOfType<DeclaredType>(identifier, lexLocation, "type");
    }

    [Pure]
    private static IDeclarationResolveResult<T> TryGetEntityOfType<T>(this Scope scope, string identifier,
        CustomLexLocation lexLocation, string entityName) where T : IDeclaredEntity
    {
        var entity = scope.DeclaredEntities.GetValueOrDefault(identifier);
        if (entity == null)
        {
            return new DeclarationResolveFailure<T>(new[]
            {
                new TypeCheckerError($"Usage of undeclared {entityName} {identifier}",
                    new[] { lexLocation })
            });
        }

        return entity switch
        {
            T requiredEntity => new SuccessfulDeclarationResolve<T>(requiredEntity),
            _ => new DeclarationResolveFailure<T>(new[]
            {
                new TypeCheckerError(
                    $"Expected {identifier} to be {entityName}, but found {GetEntityTypeName(entity)} by this name",
                    new[]
                    {
                        lexLocation
                    })
            })
        };
    }
    
    [Pure]
    public static ITypeInferenceResult TryResolveType(this IType type, Scope scope)
    {
        return type switch
        {
            ArrayType arrayType => arrayType.UnderlyingType
                .TryResolveType(scope)
                .Map(resolvedUnderlyingType => new ResolvedArrayType(arrayType.SizeExpression, resolvedUnderlyingType)),
            BoolType => new SuccessfulTypeInference(new ResolvedBoolType()),
            IntType => new SuccessfulTypeInference(new ResolvedIntType()),
            RealType => new SuccessfulTypeInference(new ResolvedRealType()),
            RecordType recordType => TryResolveRecordType(recordType, scope),
            UserDefinedType userDefinedType => scope.TryGetType(userDefinedType.TypeName, type.LexLocation) switch
            {
                DeclarationResolveFailure<DeclaredType> declarationResolveFailure => declarationResolveFailure
                    .AsGeneralFailure(),
                SuccessfulDeclarationResolve<DeclaredType>(var declaredType) => new SuccessfulTypeInference(declaredType
                    .Type),
                _ => throw new ArgumentOutOfRangeException()
            },
            _ => throw new ArgumentOutOfRangeException(nameof(type))
        };
    }

    [Pure]
    private static ITypeInferenceResult TryResolveRecordType(RecordType recordType, Scope scope)
    {
        var maybeResolvedVariableDeclarations = recordType.Variables
            .Select(variableDeclaration => variableDeclaration.AsDeclaredEntity(scope))
            .ToArray();
        var resolvingErrors = OperationFailure.CombineErrors(
            maybeResolvedVariableDeclarations
                .OfSubType<IDeclarationResolveResult<DeclaredVariable>, DeclarationResolveFailure<DeclaredVariable>>()
                .Select(declarationFailure => declarationFailure.AsGeneralFailure()));
        // TODO: concat with conflicting keys
        if (resolvingErrors != null)
        {
            return resolvingErrors;
        }

        return new SuccessfulTypeInference(new ResolvedRecordType(
            maybeResolvedVariableDeclarations
                .OfSubType<
                    IDeclarationResolveResult<DeclaredVariable>,
                    SuccessfulDeclarationResolve<DeclaredVariable>>()
                .ToDictionaryIgnoringDuplicateKeys(
                    keySelector: resolvedVariable => resolvedVariable.DeclaredEntity.Identifier,
                    elementSelector: resolvedVariable => resolvedVariable.DeclaredEntity.Type
                )
        ));
    }

    [Pure]
    public static bool IsEquivalentTo(this IResolvedType type1, IResolvedType type2)
    {
        return (type1, type2) switch
        {
            (ResolvedArrayType el1, ResolvedArrayType el2) => el1.UnderlyingType.IsEquivalentTo(el2.UnderlyingType),
            (ResolvedBoolType, ResolvedBoolType) => true,
            (ResolvedIntType, ResolvedIntType) => true,
            (ResolvedRealType, ResolvedRealType) => true,
            (ResolvedRecordType el1, ResolvedRecordType el2) =>
                el1.Variables.Keys.Count() == el2.Variables.Keys.Count() &&
                el1.Variables
                    .All(keyValue =>
                        el2.Variables.TryGetValue(keyValue.Key, out var value2) &&
                        keyValue.Value.IsEquivalentTo(value2)),
            _ => false
        };
    }

    [Pure]
    public static bool IsConvertibleTo(this IResolvedType proposedInitialType, IResolvedType proposedDestinationType)
    {
        if (proposedInitialType.IsEquivalentTo(proposedDestinationType))
        {
            return true;
        }

        return (proposedInitialType, proposedDestinationType) switch
        {
            (ResolvedRealType, ResolvedIntType) => true,
            (ResolvedBoolType, ResolvedIntType) => true,
            (ResolvedIntType, ResolvedRealType) => true,
            (ResolvedBoolType, ResolvedRealType) => true,
            (ResolvedIntType, ResolvedBoolType) => true, // NOTE: Requires some runtime checks
            _ => false
        };
    }
}

/// <summary>
/// Resolved type (without UserDefined)
/// </summary>
public interface IResolvedType
{
    [Pure]
    string GetTypeName();
}

public record ResolvedIntType : IResolvedType
{
    [Pure]
    public string GetTypeName()
    {
        return "integer";
    }
}

public record ResolvedRealType : IResolvedType
{
    [Pure]
    public string GetTypeName()
    {
        return "real";
    }
}

public record ResolvedBoolType : IResolvedType
{
    [Pure]
    public string GetTypeName()
    {
        return "bool";
    }
}

public record ResolvedArrayType(Expression? SizeExpression, IResolvedType UnderlyingType) : IResolvedType
{
    // TODO: full typename
    [Pure]
    public string GetTypeName()
    {
        return "array";
    }
}

public record ResolvedRecordType(IReadOnlyDictionary<string, IResolvedType> Variables) : IResolvedType
{
    // TODO: full typename
    [Pure]
    public string GetTypeName()
    {
        return "record";
    }
}