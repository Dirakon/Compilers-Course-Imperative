using System.Collections.Immutable;
using System.Diagnostics.Contracts;
using Compiler.Utils;
using LLVMSharp;

namespace Compiler.TypeChecking;

public static class TypeChecker
{
    public static (Scope, OperationFailure?) TypeCheckAndGetGlobalScope(this Program program)
    {
        var globalScope = new Scope(ImmutableDictionary<string, IDeclaredEntity>.Empty);
        var possibleFailure = (OperationFailure?)null;

        var allGlobalTypeDeclarations = program.Declarations
            .OfSubType<IDeclaration, TypeDeclaration>();
        (globalScope, possibleFailure) =
            ExtractGlobalDeclarations(allGlobalTypeDeclarations, globalScope, possibleFailure);

        var allGlobalRoutineDeclarations = program.Declarations
            .OfSubType<IDeclaration, RoutineDeclaration>();
        (globalScope, possibleFailure) =
            ExtractGlobalDeclarations(allGlobalRoutineDeclarations, globalScope, possibleFailure);

        var allGlobalVariableDeclarations = program.Declarations
            .OfSubType<IDeclaration, VariableDeclaration>();
        (globalScope, possibleFailure) =
            ExtractGlobalDeclarations(allGlobalVariableDeclarations, globalScope, possibleFailure);

        return (globalScope, possibleFailure
            .TryAdd(TryGetConflictingDeclarationsError(program.Declarations))
            .TryAdd(OperationFailure.CombineErrors(globalScope
                .DeclaredEntities
                .Values
                .OfSubType<IDeclaredEntity, DeclaredRoutine>()
                .Select(routine => routine.Body.TypeCheck(
                    routine.ReturnType,
                    globalScope.AddOrOverwrite(
                        routine.Arguments
                            .Select(arg => arg.Type is ResolvedDeclaredRoutineArgumentType(var type)
                                ? new DeclaredVariable(arg.Name, type,
                                    LLVM.ConstInt(LLVMTypeRef.Int1Type(), 1, false)) // TODO: ??
                                : null)
                            .NotNull())))
                .NotNull())));
    }

    public static OperationFailure? TryGetConflictingDeclarationsError<T>(IEnumerable<T> declarations)
        where T : IDeclaration
    {
        var declarationLocationsGroupedByIdentifier = declarations
            .GroupBy(
                keySelector: declaration => declaration.GetIdentifier(),
                elementSelector: declaration => declaration.LexLocation);
        return
            OperationFailure.CombineErrors(
                declarationLocationsGroupedByIdentifier
                    .Where(lexLocationGroup => lexLocationGroup.Count() > 1)
                    .Select(lexLocationGroup =>
                        new OperationFailure(new[]
                        {
                            new TypeCheckerError(
                                $"Duplicate declarations of {lexLocationGroup.Key} found",
                                lexLocationGroup.ToArray())
                        })));
    }

    [Pure]
    public static (Scope currentScope, OperationFailure? failure) ExtractGlobalDeclarations<T>(
        IEnumerable<T> allGlobalVariableDeclarations, Scope currentScope, OperationFailure? failure = null)
        where T : IDeclaration
    {
        // Note that we iterate through type declarations from top to bottom.
        // There might be a smart way to rearrange them to allow out-of-order non-recursive type declarations.
        foreach (var declaredVariable in allGlobalVariableDeclarations)
        {
            var declaredEntity = declaredVariable.AsDeclaredEntity(currentScope);
            failure = failure.TryAdd(declaredEntity.PossibleError);
            if (declaredEntity.DeclaredEntity != null &&
                !currentScope.DeclaredEntities.ContainsKey(declaredVariable.GetIdentifier()))
            {
                currentScope = new Scope(DeclaredEntities: currentScope.DeclaredEntities.Add(
                    declaredVariable.GetIdentifier(),
                    declaredEntity.DeclaredEntity));
            }
        }

        return (currentScope, failure);
    }


    [Pure]
    public static OperationFailure? GetBadArgumentsProvidedError(
        IDeclaredRoutineArgumentType[] requiredArgumentTypes,
        IEnumerable<Expression> givenArguments,
        CustomLexLocation lexLocation,
        Scope scope)
    {
        var givenArgumentsArray = givenArguments.ToArray();
        var possibleArgumentCountFailure = requiredArgumentTypes.Length != givenArgumentsArray.Length
            ? new OperationFailure(new[]
            {
                new TypeCheckerError(
                    $"Wrong number of parameters in routine call. Required: {requiredArgumentTypes.Length}, got: {givenArgumentsArray.Length}",
                    new[] { lexLocation })
            })
            : null;
        var requiredArgumentTypesFilledToGiven = Enumerable
            .Range(0, givenArgumentsArray.Length)
            .Select(i =>
                i < requiredArgumentTypes.Length
                    ? requiredArgumentTypes[i]
                    : new UnresolvedDeclaredRoutineArgumentType());

        return possibleArgumentCountFailure
            .TryAdd(OperationFailure.CombineErrors(requiredArgumentTypesFilledToGiven
                .Zip(givenArgumentsArray)
                .Select(requiredAndGiven =>
                    requiredAndGiven switch
                    {
                        (UnresolvedDeclaredRoutineArgumentType, var expression) => expression
                            .TryInferType(scope)
                            .PossibleError,
                        (ResolvedDeclaredRoutineArgumentType(var requiredType), var expression) => expression
                            .TryInferType(scope)
                            .AddErrorOnSuccess(
                                givenType =>
                                    givenType.TryGetInconvertibleTypesError(requiredType, expression.LexLocation))
                            .PossibleError,
                        _ => throw new ArgumentOutOfRangeException(nameof(requiredAndGiven), requiredAndGiven, null)
                    })
                .NotNull())
            );
    }
}

public static class TypeCheckingAstExtensions
{
    public static OperationFailure? TypeCheck(
        this IEnumerable<IBodyElement> body,
        IDeclaredRoutineReturnType expectedReturnValue,
        Scope scope)
    {
        var possibleFailure = (OperationFailure?)null;

        foreach (var bodyElement in body)
        {
            var currentScope = scope;
            (scope, var newPossibleFailure) = bodyElement switch
            {
                TypeDeclaration typeDeclaration => typeDeclaration.AsDeclaredEntity(currentScope) switch
                {
                    (null, var error) => (currentScope, error),
                    ({ } declaredEntity, var error) => (
                        currentScope.AddOrOverwrite(declaredEntity), error)
                },
                VariableDeclaration variableDeclaration => variableDeclaration.AsDeclaredEntity(currentScope) switch
                {
                    (null, var error) => (currentScope, error),
                    ({ } declaredEntity, var error) => (
                        currentScope.AddOrOverwrite(declaredEntity), error)
                },
                ForLoop forLoop => (currentScope,
                    forLoop.Range
                        .TypeCheck(currentScope)
                        .TryAdd(
                            forLoop.Body
                                .TypeCheck(
                                    expectedReturnValue,
                                    currentScope.AddOrOverwrite(
                                        new DeclaredVariable(forLoop.IteratorName, new ResolvedIntType(),
                                            LLVM.ConstInt(LLVMTypeRef.Int1Type(), 1, false)))))),
                IfStatement ifStatement => (currentScope,
                    ifStatement.Condition
                        .TryInferType(currentScope)
                        .AddErrorOnSuccess(type => type
                            .TryGetWrongTypeError(new ResolvedBoolType(), ifStatement.Condition.LexLocation))
                        .PossibleError
                        .TryAdd(ifStatement.ThenBody.TypeCheck(expectedReturnValue, currentScope))
                        .TryAdd(ifStatement.ElseBody?.TypeCheck(expectedReturnValue, currentScope))),
                WhileLoop whileLoop => (currentScope,
                    whileLoop.Condition
                        .TryInferType(currentScope)
                        .AddErrorOnSuccess(type => type
                            .TryGetWrongTypeError(new ResolvedBoolType(), whileLoop.Condition.LexLocation))
                        .PossibleError
                        .TryAdd(whileLoop.Body.TypeCheck(expectedReturnValue, currentScope))),
                Assignment assignment => (currentScope,
                    (assignment.Target.TryInferModifiablePrimaryType(currentScope),
                        assignment.Expression.TryInferType(currentScope))
                    .AddErrorOnSuccess(
                        (modifiablePrimaryType, expressionType) =>
                            expressionType.TryGetInconvertibleTypesError(modifiablePrimaryType, assignment.LexLocation)
                    )),
                Return @return => (currentScope, (@return.ReturnValue, expectedReturnValue) switch
                {
                    (null, ResolvedDeclaredRoutineReturnType(null) or UnresolvedDeclaredRoutineReturnType) => null,
                    (null, ResolvedDeclaredRoutineReturnType({ } someExpectedType)) =>
                        new TypeCheckerError(
                            $"Cannot return without type when expecting {someExpectedType.GetTypeName()}",
                            new[] { @return.LexLocation }
                        ).ToFailure(),
                    ({ } someReturnExpression, UnresolvedDeclaredRoutineReturnType) =>
                        someReturnExpression.TryInferType(currentScope).PossibleError,
                    ({ } someReturnValue, ResolvedDeclaredRoutineReturnType(var expectedReturnType)) =>
                        expectedReturnType == null
                            ? new TypeCheckerError(
                                    "Cannot return with type when expecting no type",
                                    new[] { @return.LexLocation }
                                )
                                .ToFailure()
                                .TryAdd(someReturnValue.TryInferType(currentScope).PossibleError)
                            : new TypeInferenceResult(expectedReturnType, null)
                                .CombineWith(someReturnValue.TryInferType(currentScope),
                                    (expectedType, receivedType) =>
                                        receivedType.TryGetWrongTypeError(expectedType, @return.LexLocation)
                                            .ToTypeInferenceFailure())
                                .PossibleError,
                    _ => throw new ArgumentOutOfRangeException()
                }),
                RoutineCall routineCall => (currentScope,
                    currentScope.TryGetRoutine(routineCall.RoutineName, routineCall.LexLocation) switch
                    {
                        ({ } declaredRoutine, var errors) =>
                            errors
                                .TryAdd(
                                    TypeChecker.GetBadArgumentsProvidedError(
                                        declaredRoutine.Arguments.Select(arg => arg.Type).ToArray(),
                                        routineCall.Arguments,
                                        routineCall.LexLocation, currentScope)),
                        (null, var errors) =>
                            errors
                                .TryAdd(
                                    OperationFailure.CombineErrors(
                                        routineCall.Arguments
                                            .Select(arg => arg.TryInferType(currentScope).PossibleError)
                                            .NotNull()))
                    }),
                _ => throw new ArgumentOutOfRangeException(nameof(bodyElement))
            };
            possibleFailure = possibleFailure.TryAdd(newPossibleFailure);
        }

        return possibleFailure;
    }

    [Pure]
    private static OperationFailure? TypeCheck(this Range range, Scope scope)
    {
        return range.End
            .TryInferType(scope)
            .AddErrorOnSuccess(endType =>
                endType.TryGetWrongTypeError(new ResolvedIntType(), range.End.LexLocation))
            .PossibleError
            .TryAdd(range.Start
                .TryInferType(scope)
                .AddErrorOnSuccess(startType =>
                    startType.TryGetWrongTypeError(new ResolvedIntType(), range.Start.LexLocation))
                .PossibleError);
    }
}