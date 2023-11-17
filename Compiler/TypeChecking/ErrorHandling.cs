using System.Diagnostics.Contracts;

namespace Compiler.TypeChecking;

public static class ErrorHandling
{
    public static OperationFailure? ToFailure(this TypeCheckerError? error) => error == null
        ? null
        : new OperationFailure(new[] { error });

    public static DeclarationResolveResult<T> ToDeclarationResolveResult<T>(
        this OperationFailure? error,
        T? resolvedDeclaration = null) where T : class, IDeclaredEntity => new(resolvedDeclaration, error);

    public static DeclarationResolveResult<T> ToDeclarationResolveResult<T>(
        this TypeCheckerError? error,
        T? resolvedDeclaration = null) where T : class, IDeclaredEntity
        => error.ToFailure().ToDeclarationResolveResult(resolvedDeclaration);

    public static TypeInferenceResult ToTypeInferenceFailure(
        this OperationFailure? error,
        IResolvedType? type = null) => new(type, error);

    public static TypeInferenceResult ToTypeInferenceFailure(
        this TypeCheckerError? error,
        IResolvedType? type = null) => error.ToFailure().ToTypeInferenceFailure(type);

    [Pure]
    public static OperationFailure? TryAdd(this OperationFailure? error1Maybe, OperationFailure? error2Maybe) =>
        (error1Maybe, error2Maybe) switch
        {
            (null, null) => null,
            ({ } error1, null) => error1,
            (null, { } error2) => error2,
            ({ } error1, { } error2) => error1.Add(error2)
        };

    [Pure]
    public static TypeInferenceResult PrependError(this TypeInferenceResult result, OperationFailure? errorToPrepend) =>
        result with { PossibleError = errorToPrepend.TryAdd(result.PossibleError) };

    [Pure]
    public static TypeInferenceResult AppendError(this TypeInferenceResult result, OperationFailure? errorToAppend) =>
        result with { PossibleError = result.PossibleError.TryAdd(errorToAppend) };

    [Pure]
    public static TypeInferenceResult AddErrorOnSuccess(
        this TypeInferenceResult typeInferenceResult,
        Func<IResolvedType, OperationFailure?> errorAddingFunction)
        => typeInferenceResult.InferredType == null
            ? typeInferenceResult
            : typeInferenceResult.AppendError(errorAddingFunction(typeInferenceResult.InferredType));

    [Pure]
    public static OperationFailure? AddErrorOnSuccess(
        this (TypeInferenceResult, TypeInferenceResult) results,
        Func<IResolvedType, IResolvedType, OperationFailure?> errorAddingFunction)
    {
        var (first, second) = results;
        return first.PossibleError
            .TryAdd(second.PossibleError)
            .TryAdd((first.InferredType, second.InferredType) switch
            {
                ({ } type1, { } type2) => errorAddingFunction(type1, type2),
                _ => null
            });
    }

    [Pure]
    public static TypeInferenceResult FlatMap(
        this TypeInferenceResult typeInferenceResult,
        Func<IResolvedType, TypeInferenceResult> funcOnSuccess)
        => typeInferenceResult.InferredType == null
            ? typeInferenceResult
            : funcOnSuccess(typeInferenceResult.InferredType).PrependError(typeInferenceResult.PossibleError);

    [Pure]
    public static TypeInferenceResult Map(
        this TypeInferenceResult typeInferenceResult,
        Func<IResolvedType, IResolvedType> funcOnSuccessType)
        => typeInferenceResult.FlatMap(type => new TypeInferenceResult(funcOnSuccessType(type), null));

    [Pure]
    public static TypeInferenceResult CombineWith(
        this TypeInferenceResult first,
        TypeInferenceResult second,
        Func<IResolvedType, IResolvedType, TypeInferenceResult> combiningFunc)
        => (first.InferredType, second.InferredType) switch
        {
            ({ } type1, { } type2) => combiningFunc(type1, type2)
                .PrependError(second.PossibleError)
                .PrependError(first.PossibleError),
            _ => new TypeInferenceResult(null, first.PossibleError.TryAdd(second.PossibleError))
        };

    [Pure]
    public static TypeInferenceResult CombineWith(
        this TypeInferenceResult first,
        TypeInferenceResult second,
        Func<IResolvedType, IResolvedType, IResolvedType> combiningFunc)
        => first
            .CombineWith(
                second,
                (type1, type2) => new TypeInferenceResult(combiningFunc(type1, type2), null));
}

public record TypeCheckerError(string Message, CustomLexLocation[] Locations);

public record OperationFailure(TypeCheckerError[] Errors)
{
    [Pure]
    public OperationFailure Add(OperationFailure? other) =>
        new(Errors.Concat(other?.Errors ?? Array.Empty<TypeCheckerError>()).ToArray());

    [Pure]
    public static OperationFailure? CombineErrors(IEnumerable<OperationFailure> failures) =>
        failures.Aggregate<OperationFailure, OperationFailure?>(
            null,
            (accumulator, nextFailure) => accumulator.TryAdd(nextFailure));
}