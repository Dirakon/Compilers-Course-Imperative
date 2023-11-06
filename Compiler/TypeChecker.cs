using CommandLine;

namespace Compiler;

public static class TypeChecker
{
    public static TypeCheckerError[] TypeCheck(Program program)
    {
        // var allDeclarationIdentifiersWithLocations = program.Declarations.Select(declaration =>
        //     (Identifier: declaration switch
        //     {
        //         TypeDeclaration typeDeclaration => typeDeclaration.Name,
        //         VariableDeclaration variableDeclaration => variableDeclaration.Name,
        //         RoutineDeclaration routineDeclaration => routineDeclaration.RoutineName,
        //         _ => throw new ArgumentOutOfRangeException(nameof(declaration))
        //     }, LexLocation: declaration.LexLocation));
        // var declarationIdentifiersWithDuplicates = allDeclarationIdentifiersWithLocations
        //     .ToLookup(identifierWithLocation => identifierWithLocation.Identifier)
        //     .Where(similarIdentifiersWithLocations => similarIdentifiersWithLocations.Count() >= 2);
        // var conflictingGlobalDeclarationsErrors = declarationIdentifiersWithDuplicates.Select(
        //     similarIdentifiersWithLocations =>
        //         new TypeCheckerError(
        //             $"Conflicting global declarations of {similarIdentifiersWithLocations.Key} detected", 
        //             similarIdentifiersWithLocations.Select(identifier))
        // );
        //c
        
        var allGlobalNonVariableDeclarations = program
            .Declarations
            .OfSubType<IDeclaration, TypeDeclaration>()
            .Select(TypeCheckingAstExtensions.AsDeclaredEntity)
            .Cast<IDeclaredEntity>()
            .Concat(
                program
                    .Declarations
                    .OfType<RoutineDeclaration>()
                    .Select(TypeCheckingAstExtensions.AsDeclaredEntity))
            .ToArray();

        var currentScope = new Scope(allGlobalNonVariableDeclarations.ToDictionaryIgnoringDuplicateKeys(
            keySelector: declaredEntity => declaredEntity.Identifier,
            elementSelector: declaredEntity => declaredEntity            ));
        var allGlobalVariableDeclarations = new List<DeclaredVariable>();
        foreach (var variableDeclaration in program.Declarations.OfType<VariableDeclaration>())
        {
            var inferredTypeResult = (variableDeclaration.Type, variableDeclaration.Expresion) switch
            {
                (null, null) => throw new ArgumentOutOfRangeException(nameof(variableDeclaration)),
                (null, {} expression) => expression.TryInferType(currentScope),
                ({} type, null) => new SuccessfulTypeInference(type),
                ({} type, {} expression) => expression.TryInferType(currentScope) switch
                {
                    SuccessfulTypeInference successfulTypeInference => type.IsEquivalentTo(successfulTypeInference.InferredType) 
                        ? successfulTypeInference
                        : new OperationFailure(new []{new TypeCheckerError($"Wrong type specified on variable declaration. Specified: {type.GetTypeName()}, got : {successfulTypeInference.InferredType.GetTypeName()}", new[] {variableDeclaration.LexLocation})}),
                    OperationFailure typeInferenceFailure => typeInferenceFailure,
                    _ => throw new ArgumentOutOfRangeException()
                }
            };
        }
    }
    
    public static OperationFailure? TryAdd(this OperationFailure? error1Maybe, OperationFailure? error2Maybe)
    {
        return (error1Maybe, error2Maybe) switch
        {
            (null, null) => null,
            ({} error1, null) => error1,
            (null, {} error2) => error2,
            ({} error1, {} error2) => error1.Add(error2)
        };
    }
    
}

public record TypeCheckerError(string Message, CustomLexLocation[] Locations);

public record Scope(IReadOnlyDictionary<string, IDeclaredEntity> DeclaredEntities);

public interface IDeclaredEntity
{
    string Identifier { get; init; }
}

public record DeclaredVariable(string Identifier, IResolvedType Type) : IDeclaredEntity;

public record DeclaredType(string Identifier, IResolvedType Type) : IDeclaredEntity;

public record DeclaredRoutine(string Identifier, IResolvedType? ReturnType, IResolvedType[] ArgumentTypes) : IDeclaredEntity;

public interface ITypeInferenceResult{}

public record SuccessfulTypeInference(IResolvedType InferredType) : ITypeInferenceResult;

public record OperationFailure(TypeCheckerError[] Errors) : ITypeInferenceResult
{
    public OperationFailure Add(OperationFailure other)
    {
        return new OperationFailure(Errors.Concat(other.Errors).ToArray());
    }

    public static OperationFailure? CombineErrors(IEnumerable<OperationFailure> failures)
    {
        return failures.Aggregate<OperationFailure, OperationFailure?>(
            null,
            (accumulator, nextFailure) => accumulator == null
                ? nextFailure
                : accumulator.Add(nextFailure));
    }
    
    public DeclarationResolveFailure<T> AsDeclarationResolveFailure<T>() where T : IDeclaredEntity
    {
        return new DeclarationResolveFailure<T>(Errors);
    }
}

public interface IDeclarationResolveResult<T> where T: IDeclaredEntity{} 

public record SuccessfulDeclarationResolve<T>(T DeclaredEntity) : IDeclarationResolveResult<T> where T: IDeclaredEntity;

public record DeclarationResolveFailure<T>(TypeCheckerError[] Errors) : IDeclarationResolveResult<T> where T : IDeclaredEntity
{
    public OperationFailure AsGeneralFailure()
    {
        return new OperationFailure(Errors);
    }
}

public static class TypeCheckingAstExtensions
{
    // public static DeclaredType AsDeclaredEntity(this IDeclaration declaration)
    // {
    // }

    public static IDeclarationResolveResult<DeclaredVariable> AsDeclaredEntity(this VariableDeclaration variableDeclaration, Scope scope)
    {
        var inferredTypeMaybe = (variableDeclaration.Type, variableDeclaration.Expresion) switch
        {
            (null, null) => throw new ArgumentOutOfRangeException(nameof(variableDeclaration)),
            (null, {} expression) => expression.TryInferType(scope),
            ({} type, null) => type.TryResolveType(scope),
            ({} type, {} expression) => (type.TryResolveType(scope), expression.TryInferType(scope)) switch
            {
                (OperationFailure failure1, OperationFailure failure2) => failure1.Add(failure2),
                (OperationFailure failure, _ ) => failure,
                (_, OperationFailure failure) => failure,
                (SuccessfulTypeInference(var specifiedType), SuccessfulTypeInference(var inferredType)) => inferredType.IsConvertibleTo(specifiedType) 
                    ? new SuccessfulTypeInference(specifiedType)
                    : new OperationFailure(new []{new TypeCheckerError($"Cannot do type conversion for variable declaration. Specified: {specifiedType.GetTypeName()}, got : {inferredType.GetTypeName()}", new[] {variableDeclaration.LexLocation})}),
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

    public static IDeclarationResolveResult<DeclaredType> AsDeclaredEntity(this TypeDeclaration typeDeclaration, Scope scope)
    {
        return typeDeclaration.Type.TryResolveType(scope) switch
        {
            OperationFailure operationFailure => operationFailure.AsDeclarationResolveFailure<DeclaredType>(),
            SuccessfulTypeInference(var inferredType) => new SuccessfulDeclarationResolve<DeclaredType>(new DeclaredType(typeDeclaration.Name, inferredType)),
            _ => throw new ArgumentOutOfRangeException()
        };
    }
    
    public static IDeclarationResolveResult<DeclaredRoutine> AsDeclaredEntity(this RoutineDeclaration routineDeclaration, Scope scope)
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
    
    private static ITypeInferenceResult TryInferType(this Expression expression, Scope scope)
    {
        var firstInferredType = expression.First.TryInferType(scope);
        return firstInferredType switch
        {
            OperationFailure operationFailure => operationFailure,
            SuccessfulTypeInference success => expression.Operations.ValidateOperationsGivenType(success.InferredType, scope) switch
            {
                null => success,
                {} someError => someError
            },
            _ => throw new ArgumentOutOfRangeException(nameof(firstInferredType))
        };
    }
    
    private static OperationFailure? ValidateOperationsGivenType(this INodeList<RelationOperation> operations, IResolvedType requiredType, Scope scope)
    {
        return operations switch
        {
            EmptyNodeList<RelationOperation> => null,
            NonEmptyNodeList<RelationOperation>(var op, var otherOps, var lexLocation) => 
                op.Type.ValidateOperationSuitability(requiredType, lexLocation)
                    .TryAdd(op.Relation.TryInferType(scope) switch
                    {
                        OperationFailure operationFailure => operationFailure,
                        SuccessfulTypeInference(var inferredType) => inferredType.ValidateIsRequiredType(requiredType, lexLocation),
                        _ => throw new ArgumentOutOfRangeException()
                    })
                    .TryAdd(otherOps.ValidateOperationsGivenType(requiredType, scope)),
            _ => throw new ArgumentOutOfRangeException(nameof(operations))
        };
    }

    private static OperationFailure? ValidateOperationSuitability(this RelationOperationType operation, IResolvedType type, CustomLexLocation lexLocation)
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

        return new OperationFailure(new [] {new TypeCheckerError($"Cannot use operation {operation} on type {type.GetTypeName()}", new []{lexLocation})});
    }
    
    private static ITypeInferenceResult TryInferType(this Relation relation, Scope scope)
    {
        var firstInferredType = relation.First.TryInferType(scope);
        return firstInferredType switch
        {
            OperationFailure operationFailure => operationFailure,
            SuccessfulTypeInference success => relation.Operations.ValidateOperationsGivenType(success.InferredType, scope) switch
            {
                null => success,
                {} someError => someError
            },
            _ => throw new ArgumentOutOfRangeException(nameof(firstInferredType))
        };
    }
    
    private static OperationFailure? ValidateOperationsGivenType(this INodeList<SimpleOperation> operations, IResolvedType requiredType, Scope scope)
    {
        return operations switch
        {
            EmptyNodeList<SimpleOperation> => null,
            NonEmptyNodeList<SimpleOperation>(var op, var otherOps, var lexLocation) => 
                op.Type.ValidateOperationSuitability(requiredType, lexLocation)
                    .TryAdd(op.Simple.TryInferType(scope) switch
                    {
                        OperationFailure operationFailure => operationFailure,
                        SuccessfulTypeInference(var inferredType) => inferredType.ValidateIsRequiredType(requiredType, lexLocation),
                        _ => throw new ArgumentOutOfRangeException()
                    })
                    .TryAdd(otherOps.ValidateOperationsGivenType(requiredType, scope)),
            _ => throw new ArgumentOutOfRangeException(nameof(operations))
        };
    }

    private static OperationFailure? ValidateOperationSuitability(this SimpleOperationType operation, IResolvedType type, CustomLexLocation lexLocation)
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

        return new OperationFailure(new [] {new TypeCheckerError($"Cannot use operation {operation} on type {type.GetTypeName()}", new []{lexLocation})});
    }
    
    
    private static ITypeInferenceResult TryInferType(this Simple simple, Scope scope)
    {
        var firstInferredType = simple.First.TryInferType(scope);
        return firstInferredType switch
        {
            OperationFailure operationFailure => operationFailure,
            SuccessfulTypeInference success => simple.Operations.ValidateOperationsGivenType(success.InferredType, scope) switch
            {
                null => success,
                {} someError => someError
            },
            _ => throw new ArgumentOutOfRangeException(nameof(firstInferredType))
        };
    }
    
    private static OperationFailure? ValidateOperationsGivenType(this INodeList<SummandOperation> operations, IResolvedType requiredType, Scope scope)
    {
        return operations switch
        {
            EmptyNodeList<SummandOperation> => null,
            NonEmptyNodeList<SummandOperation>(var op, var otherOps, var lexLocation) => 
                op.Type.ValidateOperationSuitability(requiredType, lexLocation)
                    .TryAdd(op.Summand.TryInferType(scope) switch
                    {
                        OperationFailure operationFailure => operationFailure,
                        SuccessfulTypeInference(var inferredType) => inferredType.ValidateIsRequiredType(requiredType, lexLocation),
                        _ => throw new ArgumentOutOfRangeException()
                    })
                    .TryAdd(otherOps.ValidateOperationsGivenType(requiredType, scope)),
            _ => throw new ArgumentOutOfRangeException(nameof(operations))
        };
    }

    private static OperationFailure? ValidateOperationSuitability(this SummandOperationType operation, IResolvedType type, CustomLexLocation lexLocation)
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

        return new OperationFailure(new [] {new TypeCheckerError($"Cannot use operation {operation} on type {type.GetTypeName()}", new []{lexLocation})});
    }
    
    private static ITypeInferenceResult TryInferType(this Summand summand, Scope scope)
    {
        var firstInferredType = summand.First.TryInferType(scope);
        return firstInferredType switch
        {
            OperationFailure operationFailure => operationFailure,
            SuccessfulTypeInference success => summand.Operations.ValidateOperationsGivenType(success.InferredType, scope) switch
            {
                null => success,
                {} someError => someError
            },
            _ => throw new ArgumentOutOfRangeException(nameof(firstInferredType))
        };
    }
    
    private static OperationFailure? ValidateOperationsGivenType(this INodeList<FactorOperation> operations, IResolvedType requiredType, Scope scope)
    {
        return operations switch
        {
            EmptyNodeList<FactorOperation> => null,
            NonEmptyNodeList<FactorOperation>(var op, var otherOps, var lexLocation) => 
                op.Type.ValidateOperationSuitability(requiredType, lexLocation)
                    .TryAdd(op.Factor.TryInferType(scope) switch
                    {
                        OperationFailure operationFailure => operationFailure,
                        SuccessfulTypeInference(var inferredType) => inferredType.ValidateIsRequiredType(requiredType, lexLocation),
                        _ => throw new ArgumentOutOfRangeException()
                    })
                    .TryAdd(otherOps.ValidateOperationsGivenType(requiredType, scope)),
            _ => throw new ArgumentOutOfRangeException(nameof(operations))
        };
    }

    private static OperationFailure? ValidateOperationSuitability(this FactorOperationType operation, IResolvedType type, CustomLexLocation lexLocation)
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

        return new OperationFailure(new [] {new TypeCheckerError($"Cannot use operation {operation} on type {type.GetTypeName()}", new []{lexLocation})});
    }

    private static OperationFailure? ValidateIsRequiredType(this IResolvedType someType, IResolvedType requiredType,
        CustomLexLocation lexLocation)
    {
        if (someType.IsEquivalentTo(requiredType))
        {
            return null;
        }

        return new OperationFailure(new [] {new TypeCheckerError($"Different types used in one expression: {requiredType} and {someType}", new []{lexLocation})});

    }
    
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
                    DeclarationResolveFailure<DeclaredRoutine> declarationResolveFailure => declarationResolveFailure.AsGeneralFailure(),
                    SuccessfulDeclarationResolve<DeclaredRoutine>(var declaredRoutine) => declaredRoutine.ReturnType == null
                    ? new OperationFailure(new []{new TypeCheckerError($"Usage of routine without return {routineCall.RoutineName} in expression", new []{routineCall.LexLocation})} )
                    : new SuccessfulTypeInference(declaredRoutine.ReturnType),
                    _ => throw new ArgumentOutOfRangeException()
                },
            _ => throw new ArgumentOutOfRangeException(nameof(factor))
        };
    }

    private static ITypeInferenceResult TryInferModifiablePrimaryType(
        this ModifiablePrimary modifiablePrimary,
        Scope scope)
    {
        return scope.TryGetVariable(modifiablePrimary.Identifier, modifiablePrimary.LexLocation) switch
        {
            DeclarationResolveFailure<DeclaredVariable> declarationResolveFailure => declarationResolveFailure.AsGeneralFailure(),
            SuccessfulDeclarationResolve<DeclaredVariable> successfulDeclarationResolve => successfulDeclarationResolve.DeclaredEntity.Type
                .TryInferModifiablePrimaryType(modifiablePrimary.Operations),
            _ => throw new ArgumentOutOfRangeException()
        };
    }

    private static ITypeInferenceResult TryInferModifiablePrimaryType(this IResolvedType currentType, INodeList<IModifiablePrimaryOperation> operations)
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
                    null => new OperationFailure(new []{new TypeCheckerError($"Cannot get member {memberName} on type {type.GetTypeName()}", new []{lexLocation})}),
                    {} someMemberType => someMemberType.TryInferModifiablePrimaryType(otherOps)
                },
            ({} type, EmptyNodeList<IModifiablePrimaryOperation>) => new SuccessfulTypeInference(type),
            ({} type, 
                NonEmptyNodeList<IModifiablePrimaryOperation>(MemberCall, _, var lexLocation)
                ) => new OperationFailure(new []{new TypeCheckerError($"Cannot apply member call operation on type {type.GetTypeName()}", new []{lexLocation})}),
            ({} type, 
                NonEmptyNodeList<IModifiablePrimaryOperation>(ArrayIndexing, _, var lexLocation)
                ) => new OperationFailure(new []{new TypeCheckerError($"Cannot apply array indexing operation on type {type.GetTypeName()}", new []{lexLocation})}),
            _ => throw new ArgumentOutOfRangeException()
        };
    }


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
    
    private static IDeclarationResolveResult<DeclaredRoutine> TryGetRoutine(this Scope scope, string identifier, CustomLexLocation lexLocation)
    {
        return scope.TryGetEntityOfType<DeclaredRoutine>(identifier, lexLocation, "routine");
    }
    
    private static IDeclarationResolveResult<DeclaredVariable> TryGetVariable(this Scope scope, string identifier, CustomLexLocation lexLocation)
    {
        return scope.TryGetEntityOfType<DeclaredVariable>(identifier, lexLocation, "variable");
    }
    
    private static IDeclarationResolveResult<DeclaredType> TryGetType(this Scope scope, string identifier, CustomLexLocation lexLocation)
    {
        return scope.TryGetEntityOfType<DeclaredType>(identifier, lexLocation, "type");
    }

    private static IDeclarationResolveResult<T> TryGetEntityOfType<T>(this Scope scope, string identifier, CustomLexLocation lexLocation, string entityName) where T : IDeclaredEntity
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
            _ => new DeclarationResolveFailure<T>(new[]{
                new TypeCheckerError($"Expected {identifier} to be {entityName}, but found {GetEntityTypeName(entity)} by this name", new []
                {
                    lexLocation
                })} )
        };
    }
    


    public static ITypeInferenceResult TryResolveType(this IType type, Scope scope)
    {
        return type switch
        {
            ArrayType arrayType => arrayType.UnderlyingType.TryResolveType(scope) switch
            {
                SuccessfulTypeInference successfulTypeInference => new SuccessfulTypeInference(new ResolvedArrayType(arrayType.SizeExpression, successfulTypeInference.InferredType)),
                OperationFailure typeInferenceFailure => typeInferenceFailure,
                _ => throw new ArgumentOutOfRangeException()
            },
            BoolType => new SuccessfulTypeInference(new ResolvedBoolType()),
            IntType => new SuccessfulTypeInference(new ResolvedIntType()),
            RealType => new SuccessfulTypeInference(new ResolvedRealType()),
            RecordType recordType => TryResolveRecordType(recordType, scope),
            UserDefinedType userDefinedType => scope.TryGetType(userDefinedType.TypeName, type.LexLocation) switch
                {
                    DeclarationResolveFailure<DeclaredType> declarationResolveFailure => declarationResolveFailure.AsGeneralFailure(),
                    SuccessfulDeclarationResolve<DeclaredType>(var declaredType) => new SuccessfulTypeInference(declaredType.Type),
                    _ => throw new ArgumentOutOfRangeException()
                },
            _ => throw new ArgumentOutOfRangeException(nameof(type))
        };
    }

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

    public static bool IsEquivalentTo(this IResolvedType type1, IResolvedType type2)
    {
        return (type1, type2) switch {
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
    
    public static bool IsConvertibleTo(this IResolvedType proposedInitialType, IResolvedType proposedDestinationType)
    {
        if (proposedInitialType.IsEquivalentTo(proposedDestinationType))
        {
            return true;
        }
        return (proposedInitialType, proposedDestinationType) switch {
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
    string GetTypeName();
}


public record ResolvedIntType : IResolvedType
{
    public string GetTypeName()
    {
        return "integer";
    }
}

public record ResolvedRealType : IResolvedType
{
    public string GetTypeName()
    {
        return "real";
    }
}

public record ResolvedBoolType : IResolvedType
{
    public string GetTypeName()
    {
        return "bool";
    }
}

public record ResolvedArrayType(Expression? SizeExpression, IResolvedType UnderlyingType) : IResolvedType
{
    // TODO: full typename
    public string GetTypeName()
    {
        return "array";
    }
}

public record ResolvedRecordType(IReadOnlyDictionary<string, IResolvedType> Variables) : IResolvedType
{
    // TODO: full typename
    public string GetTypeName()
    {
        return "record";
    }
}