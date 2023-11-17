using System.Collections.Immutable;
using System.Diagnostics.Contracts;

namespace Compiler.TypeChecking;

public readonly record struct Scope(ImmutableDictionary<string, IDeclaredEntity> DeclaredEntities)
{
    [Pure]
    public Scope AddOrOverwrite(IDeclaredEntity newEntity) =>
        new(DeclaredEntities.Remove(newEntity.Identifier).Add(newEntity.Identifier, newEntity));

    [Pure]
    public Scope AddOrOverwrite(IEnumerable<IDeclaredEntity> newEntities) =>
        newEntities.Aggregate(this, (scope, entity) => scope.AddOrOverwrite(entity));

    [Pure]
    public static string GetEntityTypeName<T>(T value) where T : IDeclaredEntity =>
        value switch
        {
            DeclaredRoutine => "routine",
            DeclaredType => "type",
            DeclaredVariable => "variable",
            _ => throw new ArgumentOutOfRangeException(nameof(value))
        };

    [Pure]
    public DeclarationResolveResult<DeclaredRoutine> TryGetRoutine(string identifier, CustomLexLocation lexLocation) =>
        TryGetEntityOfType<DeclaredRoutine>(identifier, lexLocation, "routine");

    [Pure]
    public DeclarationResolveResult<DeclaredVariable>
        TryGetVariable(string identifier, CustomLexLocation lexLocation) =>
        TryGetEntityOfType<DeclaredVariable>(identifier, lexLocation, "variable");

    [Pure]
    public DeclarationResolveResult<DeclaredType> TryGetType(string identifier, CustomLexLocation lexLocation) =>
        TryGetEntityOfType<DeclaredType>(identifier, lexLocation, "type");

    [Pure]
    public DeclarationResolveResult<T> TryGetEntityOfType<T>(
        string identifier,
        CustomLexLocation lexLocation,
        string entityName) where T : class, IDeclaredEntity
    {
        var entity = DeclaredEntities.GetValueOrDefault(identifier);
        if (entity == null)
        {
            return new TypeCheckerError(
                $"Usage of undeclared {entityName} {identifier}",
                new[] { lexLocation }).ToDeclarationResolveResult<T>();
        }

        return entity switch
        {
            T requiredEntity => new DeclarationResolveResult<T>(requiredEntity, null),
            _ => new TypeCheckerError(
                $"Expected {identifier} to be {entityName}, but found {GetEntityTypeName(entity)} by this name",
                new[]
                {
                    lexLocation
                }).ToDeclarationResolveResult<T>()
        };
    }
}