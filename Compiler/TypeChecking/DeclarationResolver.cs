using System.Diagnostics.Contracts;

namespace Compiler.TypeChecking;

public record DeclarationResolveResult<T>(T? DeclaredEntity, OperationFailure? PossibleError)
    where T : class, IDeclaredEntity;

public interface IDeclaredEntity
{
    string Identifier { get; }
}

public record DeclaredVariable(string Identifier, IResolvedType Type) : IDeclaredEntity;

public record DeclaredType(string Identifier, IResolvedType Type) : IDeclaredEntity;

public record DeclaredRoutine(
    string Identifier,
    IDeclaredRoutineReturnType ReturnType,
    (IDeclaredRoutineArgumentType Type, string Name)[] Arguments,
    INodeList<IBodyElement> Body) : IDeclaredEntity;

public interface IDeclaredRoutineReturnType
{
}

public record ResolvedDeclaredRoutineReturnType(IResolvedType? ReturnType) : IDeclaredRoutineReturnType;

public record UnresolvedDeclaredRoutineReturnType : IDeclaredRoutineReturnType;

public interface IDeclaredRoutineArgumentType
{
}

public record ResolvedDeclaredRoutineArgumentType(IResolvedType ArgumentType) : IDeclaredRoutineArgumentType;

public record UnresolvedDeclaredRoutineArgumentType : IDeclaredRoutineArgumentType;

public static class DeclarationResolver
{
    [Pure]
    public static TypeInferenceResult AsTypeInferenceResult(this DeclarationResolveResult<DeclaredType> declaredType)
        => new(declaredType.DeclaredEntity?.Type, declaredType.PossibleError);

    [Pure]
    public static DeclarationResolveResult<IDeclaredEntity> AsDeclaredEntity(this IDeclaration declaration, Scope scope)
        => declaration switch
        {
            TypeDeclaration typeDeclaration => typeDeclaration.AsDeclaredEntity(scope).ToGeneralDeclaration(),
            VariableDeclaration variableDeclaration => variableDeclaration.AsDeclaredEntity(scope)
                .ToGeneralDeclaration(),
            RoutineDeclaration routineDeclaration => routineDeclaration.AsDeclaredEntity(scope).ToGeneralDeclaration(),
            _ => throw new ArgumentOutOfRangeException(nameof(declaration))
        };

    public static string GetIdentifier(this IDeclaration declaration) =>
        declaration switch
        {
            TypeDeclaration typeDeclaration => typeDeclaration.Name,
            VariableDeclaration variableDeclaration => variableDeclaration.Name,
            RoutineDeclaration routineDeclaration => routineDeclaration.RoutineName,
            _ => throw new ArgumentOutOfRangeException(nameof(declaration))
        };

    [Pure]
    public static DeclarationResolveResult<IDeclaredEntity> ToGeneralDeclaration(
        this DeclarationResolveResult<DeclaredVariable> declaration)
        => new(declaration.DeclaredEntity, declaration.PossibleError);

    [Pure]
    public static DeclarationResolveResult<IDeclaredEntity> ToGeneralDeclaration(
        this DeclarationResolveResult<DeclaredType> declaration)
    {
        return new DeclarationResolveResult<IDeclaredEntity>(declaration.DeclaredEntity, declaration.PossibleError);
    }

    [Pure]
    public static DeclarationResolveResult<IDeclaredEntity> ToGeneralDeclaration(
        this DeclarationResolveResult<DeclaredRoutine> declaration)
        => new(declaration.DeclaredEntity, declaration.PossibleError);

    [Pure]
    public static DeclarationResolveResult<DeclaredVariable> AsDeclaredEntity(
        this VariableDeclaration variableDeclaration,
        Scope scope)
    {
        var inferredTypeMaybe = (variableDeclaration.Type, variableDeclaration.Expression) switch
        {
            (null, { } expression) => expression.TryInferType(scope),
            ({ } type, null) => type.TryResolveType(scope, isInFunctionSignature: false),
            ({ } type, { } expression) => new TypeInferenceResult(
                type.TryResolveType(scope, isInFunctionSignature: false).InferredType,
                (type.TryResolveType(scope, isInFunctionSignature: false),
                    expression.TryInferType(scope)).AddErrorOnSuccess(
                    (specifiedType, inferredType) =>
                        inferredType.TryGetInconvertibleTypesError(specifiedType, variableDeclaration.LexLocation))),
            // Should be impossible since AST requires at least one of the fields to be filled
            (null, null) => throw new ArgumentOutOfRangeException(nameof(variableDeclaration))
        };
        return new DeclarationResolveResult<DeclaredVariable>(inferredTypeMaybe.InferredType == null
                ? null
                : new DeclaredVariable(
                    variableDeclaration.Name,
                    inferredTypeMaybe.InferredType),
            inferredTypeMaybe.PossibleError);
    }

    [Pure]
    public static DeclarationResolveResult<DeclaredType> AsDeclaredEntity(
        this TypeDeclaration typeDeclaration,
        Scope scope)
    {
        var resolvedType = typeDeclaration.Type.TryResolveType(scope, isInFunctionSignature: false);
        return new DeclarationResolveResult<DeclaredType>(
            resolvedType.InferredType == null
                ? null
                : new DeclaredType(typeDeclaration.Name, resolvedType.InferredType),
            resolvedType.PossibleError
        );
    }

    [Pure]
    public static DeclarationResolveResult<DeclaredRoutine> AsDeclaredEntity(
        this RoutineDeclaration routineDeclaration, Scope scope)
    {
        var argumentInfos = routineDeclaration.Parameters
            .Select(parameter =>
                (TypeInferenceResult: parameter.Type.TryResolveType(scope, isInFunctionSignature: true),
                    parameter.Name))
            .ToArray();
        var argumentsInferenceError = OperationFailure.CombineErrors(
            argumentInfos
                .Select(parameterInfo => parameterInfo.TypeInferenceResult.PossibleError)
                .NotNull());

        var arguments = argumentInfos
            .Select<(TypeInferenceResult Result, string Name), (IDeclaredRoutineArgumentType, string)>(
                argumentInfo => argumentInfo.Result.InferredType == null
                    ? (new UnresolvedDeclaredRoutineArgumentType(), argumentInfo.Name)
                    : (new ResolvedDeclaredRoutineArgumentType(argumentInfo.Result.InferredType), argumentInfo.Name))
            .ToArray();

        return routineDeclaration.ReturnType?.TryResolveType(scope, isInFunctionSignature: true) switch
        {
            null => new DeclarationResolveResult<DeclaredRoutine>(
                new DeclaredRoutine(
                    routineDeclaration.RoutineName,
                    new ResolvedDeclaredRoutineReturnType(null),
                    arguments,
                    routineDeclaration.Body
                ),
                argumentsInferenceError),
            var (inferredType, returnInferenceError) => new DeclarationResolveResult<DeclaredRoutine>(
                new DeclaredRoutine(
                    routineDeclaration.RoutineName,
                    inferredType == null
                        ? new UnresolvedDeclaredRoutineReturnType()
                        : new ResolvedDeclaredRoutineReturnType(inferredType),
                    arguments,
                    routineDeclaration.Body
                ),
                argumentsInferenceError.TryAdd(returnInferenceError))
        };
    }
}