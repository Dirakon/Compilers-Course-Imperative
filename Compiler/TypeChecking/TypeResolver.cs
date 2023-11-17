using System.Diagnostics.Contracts;

namespace Compiler.TypeChecking;

public static class TypeResolver
{
    [Pure]
    public static TypeInferenceResult TryResolveType(this IType type, Scope scope, bool isInFunctionSignature) =>
        type switch
        {
            ArrayType arrayType =>
                arrayType.UnderlyingType
                    .TryResolveType(scope, isInFunctionSignature)
                    .Map(
                        resolvedUnderlyingType =>
                            new ResolvedArrayType(arrayType.SizeExpression, resolvedUnderlyingType))
                    .AppendError(
                        TypeCheckArraySize(
                            arrayType.SizeExpression,
                            isInFunctionSignature, scope,
                            arrayType.LexLocation)),
            BoolType => new TypeInferenceResult(new ResolvedBoolType(), null),
            IntType => new TypeInferenceResult(new ResolvedIntType(), null),
            RealType => new TypeInferenceResult(new ResolvedRealType(), null),
            RecordType recordType => TryResolveRecordType(recordType, scope),
            UserDefinedType userDefinedType => scope
                .TryGetType(userDefinedType.TypeName, type.LexLocation)
                .AsTypeInferenceResult(),
            _ => throw new ArgumentOutOfRangeException(nameof(type))
        };

    [Pure]
    public static TypeInferenceResult TryResolveRecordType(RecordType recordType, Scope scope)
    {
        var maybeResolvedVariableDeclarations = recordType.Variables
            .Select(variableDeclaration => variableDeclaration.AsDeclaredEntity(scope))
            .ToArray();
        var resolvingErrors = OperationFailure.CombineErrors(
                maybeResolvedVariableDeclarations
                    .Select(result => result.PossibleError)
                    .NotNull())
            .TryAdd(TypeChecker.TryGetConflictingDeclarationsError(recordType.Variables));

        return new TypeInferenceResult(
            new ResolvedRecordType(
                maybeResolvedVariableDeclarations
                    .Select(result => result.DeclaredEntity)
                    .NotNull()
                    .ToDictionaryIgnoringDuplicateKeys(
                        keySelector: declaredEntity => declaredEntity.Identifier,
                        elementSelector: declaredEntity => declaredEntity.Type
                    )),
            resolvingErrors
        );
    }

    public static OperationFailure? TypeCheckArraySize(
        Expression? arraySizeExpression,
        bool isInFunctionSignature,
        Scope scope, CustomLexLocation lexLocation)
    {
        var possibleInvalidPlaceFailure =
            // (isFunctionParameter && arraySizeExpression != null) || // TODO: is it valid for size to be in param?
            !isInFunctionSignature && arraySizeExpression == null
                ? new OperationFailure(new[]
                {
                    new TypeCheckerError("Arrays have to have size expression when not in function signature",
                        new[] { lexLocation })
                })
                : null;

        var possibleTypeFailure = arraySizeExpression
            ?.TryInferType(scope)
            .AddErrorOnSuccess(type => type.TryGetWrongTypeError(new ResolvedIntType(), lexLocation))
            .PossibleError;

        return possibleInvalidPlaceFailure.TryAdd(possibleTypeFailure);
    }


    [Pure]
    public static bool IsEquivalentTo(this IResolvedType type1, IResolvedType type2) =>
        (type1, type2) switch
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


    [Pure]
    public static OperationFailure? TryGetInconvertibleTypesError(this IResolvedType proposedInitialType,
        IResolvedType proposedDestinationType, CustomLexLocation lexLocation) =>
        proposedInitialType.IsConvertibleTo(proposedDestinationType)
            ? null
            : new TypeCheckerError(
                $"Cannot do type conversion. Specified: {proposedDestinationType.GetTypeName()}, got: {proposedInitialType.GetTypeName()}",
                new[] { lexLocation }).ToFailure();

    [Pure]
    public static OperationFailure? TryGetWrongTypeError(
        this IResolvedType someType,
        IResolvedType requiredType,
        CustomLexLocation lexLocation) =>
        someType.IsEquivalentTo(requiredType)
            ? null
            : new TypeCheckerError(
                $"Wrong type. Expected: {requiredType.GetTypeName()}, got: {someType.GetTypeName()}",
                new[] { lexLocation }).ToFailure();
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
    public string GetTypeName() => "integer";
}

public record ResolvedRealType : IResolvedType
{
    [Pure]
    public string GetTypeName() => "real";
}

public record ResolvedBoolType : IResolvedType
{
    [Pure]
    public string GetTypeName() => "bool";
}

public record ResolvedArrayType(Expression? SizeExpression, IResolvedType UnderlyingType) : IResolvedType
{
    [Pure]
    public string GetTypeName() => $"array of {UnderlyingType.GetTypeName()}";
}

public record ResolvedRecordType(IReadOnlyDictionary<string, IResolvedType> Variables) : IResolvedType
{
    [Pure]
    public string GetTypeName()
    {
        var variablesString = string.Join(", ",
            Variables
                .Order()
                .Select(keyValue => $"{keyValue.Key} : {keyValue.Value.GetTypeName()}"));
        return $"record of {{{variablesString}}}";
    }
}