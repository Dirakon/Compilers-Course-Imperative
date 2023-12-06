using System.Collections.Immutable;
using CommandLine;
using Compiler.TypeChecking;
using Compiler.Utils;
using LLVMSharp;
using Compiler.Utils;

namespace Compiler.CodeGeneration;

public static class GenerateBitcode
{
    private static LLVMValueRef CreateExtern(
        LLVMModuleRef module,
        string name,
        LLVMTypeRef returnType,
        LLVMTypeRef[] parameters,
        bool isVarArg = true
    )
    {
        var functionType = LLVM.FunctionType(returnType, parameters, isVarArg);
        return LLVM.AddFunction(module, name, functionType);
    }

    private static LLVMValueRef DeclarePrintf(LLVMModuleRef module)
    {
        LLVMTypeRef returnType = LLVM.Int32Type();
        LLVMTypeRef[] parameters = { LLVM.PointerType(LLVM.Int8Type(), 0) };
        return CreateExtern(module, "printf", returnType, parameters);
    }

    private static (LLVMValueRef, LLVMBuilderRef) CreateFunction(
        LLVMModuleRef module,
        string name,
        LLVMTypeRef returnType,
        LLVMTypeRef[] parameters,
        bool isVarArgs = false
    )
    {
        var funcType = LLVM.FunctionType(returnType, parameters, isVarArgs);
        var func = LLVM.AddFunction(module, name, funcType);
        var entry = LLVM.AppendBasicBlock(func, "entry");
        var builder = LLVM.CreateBuilder();
        LLVM.PositionBuilderAtEnd(builder, entry);

        return (func, builder);
    }

    private static (LLVMValueRef, LLVMBuilderRef) CreateMain(LLVMModuleRef module)
    {
        return CreateFunction(module, "main", LLVM.Int32Type(), Array.Empty<LLVMTypeRef>());
    }

    private static (LLVMValueRef, LLVMBuilderRef) CreateEntryPoint(LLVMModuleRef module,
        ResolvedDeclaredRoutineReturnType returnType)
    {
        return returnType.ReturnType switch
        {
            ResolvedIntType => CreateFunction(module, "EntryPoint", LLVMTypeRef.Int32Type(),
                Array.Empty<LLVMTypeRef>()),
            ResolvedRealType => CreateFunction(module, "EntryPoint", LLVMTypeRef.DoubleType(),
                Array.Empty<LLVMTypeRef>()),
            ResolvedBoolType => CreateFunction(module, "EntryPoint", LLVMTypeRef.Int1Type(),
                Array.Empty<LLVMTypeRef>()),
            _ => (default, default) // TODO: error or specific message when unformattable type
        };
    }

    private static LLVMValueRef CreateFormatString(LLVMBuilderRef builder, IResolvedType type)
    {
        return type switch
        {
            ResolvedIntType => LLVM.BuildGlobalStringPtr(builder, "%d\n", "str"),
            ResolvedRealType => LLVM.BuildGlobalStringPtr(builder, "%f\n", "str"),
            ResolvedBoolType => LLVM.BuildGlobalStringPtr(builder, "%hhu\n", "str"),
            _ => LLVM.BuildGlobalStringPtr(builder, "%s\n", "str"),
        };
    }


    private static void RunMain(LLVMModuleRef module)
    {
        LLVM.LinkInMCJIT();
        LLVM.InitializeX86TargetMC();
        LLVM.InitializeX86Target();
        LLVM.InitializeX86TargetInfo();
        LLVM.InitializeX86AsmParser();
        LLVM.InitializeX86AsmPrinter();
        LLVM.CreateExecutionEngineForModule(out var engine, module, out var _);


        var main = LLVM.GetNamedFunction(module, "main");
        LLVM.RunFunction(engine, main, Array.Empty<LLVMGenericValueRef>());
    }


    public static LLVMTypeRef GetLlvmRepresentationOf(IResolvedType type, Scope currentScope)
    {
        return type switch
        {
            ResolvedBoolType => LLVMTypeRef.Int1Type(),
            ResolvedIntType => LLVMTypeRef.Int32Type(),
            ResolvedRealType => LLVMTypeRef.DoubleType(),
            ResolvedArrayType resolvedArrayType => LLVMTypeRef.PointerType(
                LLVMTypeRef.StructType(new[]
                {
                    LLVMTypeRef.Int32Type(),
                    LLVMTypeRef.PointerType(GetLlvmRepresentationOf(resolvedArrayType.UnderlyingType, currentScope), 0)
                }, false),
                0),
            ResolvedRecordType resolvedRecordType => ConstructLlvmRepresentationOfRecordType(resolvedRecordType,
                currentScope),
            _ => throw new ArgumentOutOfRangeException(nameof(type))
        };
    }

    private static LLVMTypeRef ConstructLlvmRepresentationOfRecordType(ResolvedRecordType resolvedRecordType,
        Scope currentScope)
    {
        var sortedNames = resolvedRecordType.SortedVariableNames;
        var sortedTypes = sortedNames.Select(name => resolvedRecordType.Variables[name]).ToArray();
        var sortedTypesInLlvm = sortedTypes.Select(type => GetLlvmRepresentationOf(type, currentScope)).ToArray();
        return LLVMTypeRef.PointerType(
            LLVMTypeRef.StructType(sortedTypesInLlvm, false),
            0);
    }

    public static LLVMModuleRef LlvmizeAst(this Scope globalScope, Program initialAst, LLVMModuleRef module)
    {
        var builder = LLVM.CreateBuilder();

        foreach (var declaredType in
                 globalScope.DeclaredEntities.Values.OfSubType<IDeclaredEntity, DeclaredType>().ToArray())
        {
            globalScope = globalScope.AddOrOverwrite(declaredType with
            {
                LlvmType = GetLlvmRepresentationOf(declaredType.Type, globalScope)
            });
        }

        foreach (var declaredRoutine in
                 globalScope.DeclaredEntities.Values.OfSubType<IDeclaredEntity, DeclaredRoutine>())
        {
            var returnTypeInAst = declaredRoutine.ReturnType.Cast<ResolvedDeclaredRoutineReturnType>().ReturnType;
            var returnTypeInLlvm = returnTypeInAst != null
                ? GetLlvmRepresentationOf(returnTypeInAst, globalScope)
                : LLVMTypeRef.VoidType();

            var parametersInAst = declaredRoutine
                .Arguments.Select(typeAndName => typeAndName.Type)
                .Cast<ResolvedDeclaredRoutineArgumentType>().ToArray();

            var parametersInLlvm = new List<LLVMTypeRef>();
            foreach (var parameterTypeInAst in parametersInAst)
            {
                var parameterTypeInLlvm = GetLlvmRepresentationOf(parameterTypeInAst.ArgumentType, globalScope);
                parametersInLlvm.Add(parameterTypeInLlvm);
            }

            var funcType = LLVM.FunctionType(returnTypeInLlvm, parametersInLlvm.ToArray(), false);

            LLVM.AddFunction(module, declaredRoutine.Identifier, funcType);
        }

        {
            var initFuncType = LLVM.FunctionType(LLVMTypeRef.VoidType(), new LLVMTypeRef[] { }, false);
            var initFunc = LLVM.AddFunction(module, "________INIT________", initFuncType);
            var entryBlock = LLVM.AppendBasicBlock(initFunc, "entry");
            LLVM.PositionBuilderAtEnd(builder, entryBlock);


            var allGlobalVariableDeclarations =
                initialAst.Declarations.OfSubType<IDeclaration, VariableDeclaration>().ToArray();
            foreach (var globalVariableDeclaration in
                     allGlobalVariableDeclarations)
            {
                (globalScope, _) = Visit(globalVariableDeclaration, globalScope, builder, module, useGlobal: true);
            }
            
            LLVM.BuildRetVoid(builder);
        }
        foreach (var declaredRoutine in
                 globalScope.DeclaredEntities.Values.OfSubType<IDeclaredEntity, DeclaredRoutine>())
        {
            VisitRoutineBody(globalScope, module, declaredRoutine, builder);
        }

        return module;
    }

    private static void VisitRoutineBody(Scope functionScope, LLVMModuleRef module, DeclaredRoutine declaredRoutine,
        LLVMBuilderRef builder)
    {
        var function = LLVM.GetNamedFunction(module, declaredRoutine.Identifier);
        var entry = LLVM.AppendBasicBlock(function, "entry");
        LLVM.PositionBuilderAtEnd(builder, entry);
        foreach (var ((type, paramIdentifier), i) in declaredRoutine.Arguments.Select((value, index) => (value, index)))
        {
            var typeAsResolved = type.Cast<ResolvedDeclaredRoutineArgumentType>().ArgumentType;
            var paramValueRef = LLVM.GetParam(function, (uint)i);
            var paramAsVariable = LLVM.BuildAlloca(builder,
                    GetLlvmRepresentationOf(typeAsResolved, functionScope),
                    $"{paramIdentifier}");
            LLVM.BuildStore(builder, paramValueRef, paramAsVariable);
            functionScope = functionScope.AddOrOverwrite(
                new DeclaredVariable(paramIdentifier, typeAsResolved, paramAsVariable)
            );

        }

        (var isTerminated, _) = VisitBody(functionScope, declaredRoutine.Body, builder, module, function);

        if (declaredRoutine.ReturnType.Cast<ResolvedDeclaredRoutineReturnType>().ReturnType == null)
        {
            // Void
            if (!isTerminated)
            {
                LLVM.BuildRetVoid(builder);
            }
        }
        else
        {
            // Non-void
            if (!isTerminated)
            {
                throw new Exception(
                    $"Routine {declaredRoutine.Identifier} is not terminated in one of the execution paths!");
            }
        }
    }

    private static (bool IsTerminated, LLVMBuilderRef builder) VisitBody(Scope functionScope,
        INodeList<IBodyElement> body, LLVMBuilderRef builder,
        LLVMModuleRef module, LLVMValueRef currentFunction)
    {
        foreach (var bodyElement in body)
        {
            ((functionScope, _), var justTerminated) = bodyElement switch
            {
                // Not update scope:
                Return @return => (Visit(@return, functionScope, builder, module), true),
                RoutineCall routineCall => (
                    VisitRoutineCallStatement(routineCall, functionScope, builder, module, currentFunction), false),
                ForLoop forLoop => Visit(forLoop, functionScope, builder, module, currentFunction),
                IfStatement ifStatement => Visit(ifStatement, functionScope, builder, module, currentFunction),
                Assignment assignment => (Visit(assignment, functionScope, builder, module, currentFunction), false),
                WhileLoop whileLoop => Visit(whileLoop, functionScope, builder, module, currentFunction),

                // Update scope:
                TypeDeclaration typeDeclaration => (Visit(typeDeclaration, functionScope, builder, module), false),
                VariableDeclaration variableDeclaration => (Visit(variableDeclaration, functionScope, builder, module),
                    false), // Visit(variableDeclaration),
                _ => throw new ArgumentOutOfRangeException(nameof(bodyElement))
            };
            if (justTerminated)
            {
                return (true, builder);
            }
        }

        return (false, builder);
    }

    private static (Scope functionScope, LLVMBuilderRef builder) VisitRoutineCallStatement(RoutineCall routineCall,
        Scope functionScope, LLVMBuilderRef builder, LLVMModuleRef module, LLVMValueRef currentFunction)
    {
        (_, _) = Visit(routineCall, functionScope, builder, module);
        return (functionScope, builder);
    }

    private static (Scope functionScope, LLVMBuilderRef builder) Visit(Assignment assignment,
        Scope functionScope, LLVMBuilderRef builder, LLVMModuleRef module, LLVMValueRef currentFunction)
    {
        var (expressionInLlvm, _) = Visit(assignment.Expression, functionScope, builder, module);
        
        // Not null because we type-checked
        var expressionType = assignment.Expression.TryInferType(functionScope).InferredType!;
        var expectedType = assignment.Target.TryInferType(functionScope).InferredType!;

        var (convertedExpression, _) = ConvertVariable(expressionInLlvm, expressionType, expectedType, functionScope,
            builder, module);
        var (variableReference, _) = GetVariableReferenceFrom(assignment.Target, functionScope, builder, module);

        LLVM.BuildStore(builder, convertedExpression, variableReference);
        return (functionScope, builder);
    }

    private static (LLVMValueRef, LLVMBuilderRef) GetVariableReferenceFrom(ModifiablePrimary modifiablePrimary,
        Scope functionScope, LLVMBuilderRef builder, LLVMModuleRef module)
    {
        // TODO: arrays and records
        var variableName = modifiablePrimary.Identifier;
        // Not null because type-check
        var currentReference = functionScope.DeclaredEntities[variableName].Cast<DeclaredVariable>().LlvmVariable!;
        var currentType = functionScope.DeclaredEntities[variableName].Cast<DeclaredVariable>().Type;

        return GetVariableReferenceFrom(modifiablePrimary.Operations,
            functionScope, builder, module, currentReference, currentType);
    }

    private static (LLVMValueRef, LLVMBuilderRef) GetVariableReferenceFrom(
        INodeList<IModifiablePrimaryOperation> operations,
        Scope functionScope, LLVMBuilderRef builder, LLVMModuleRef module,
        LLVMValueRef currentReference,
        IResolvedType currentType)
    {
        if (operations is NonEmptyNodeList<IModifiablePrimaryOperation>(var currentOperation, var otherOps, _))
        {
            switch (currentType)
            {
                case ResolvedArrayType arrayType when currentOperation is ArrayIndexing(var indexExpression, _):
                {
                    var loadedCurReference = LLVM.BuildLoad(builder, currentReference, "dereference array struct ptr");
                    var (indexExpressionInLlvm, _) = Visit(indexExpression, functionScope, builder, module);
                    var underlyingLlvmArray = LLVM.BuildLoad(builder,
                        LLVM.BuildStructGEP(builder, loadedCurReference, 1, "underlying array"),
                        "loading something idk");
                    var something = LLVM.BuildGEP(builder,
                        underlyingLlvmArray,
                        new[] { indexExpressionInLlvm },
                        "Array element");
                    return GetVariableReferenceFrom(otherOps, functionScope, builder, module,
                        something, arrayType.UnderlyingType);
                }
                case ResolvedRecordType recordType when currentOperation is MemberCall(var memberName, _):
                {
                    var loadedCurReference = LLVM.BuildLoad(builder, currentReference, "dereference record struct ptr");
                    var recordMemberIndex = recordType.SortedVariableNames
                        .Select((name, index) => (name, index))
                        .First(nameWithIndex => nameWithIndex.name == memberName)
                        .index;

                    var something = LLVM.BuildStructGEP(builder, loadedCurReference, (uint)recordMemberIndex,
                        $"record field {memberName}");


                    return GetVariableReferenceFrom(otherOps, functionScope, builder, module,
                        something, recordType.Variables[memberName]);
                }
                default:
                    throw new ArgumentOutOfRangeException((currentType, currentOperation).ToString());
            }
        }

        return (currentReference, builder);
    }

    private static (LLVMValueRef?, LLVMBuilderRef) RecursivelyInitComplexStructure(IResolvedType type,
        LLVMBuilderRef builder,
        Scope scope, LLVMModuleRef module)
    {
        switch (type)
        {
            case ResolvedArrayType resolvedArrayType:
                var structTypeToAllocate = LLVMTypeRef.StructType(new[]
                {
                    LLVMTypeRef.Int32Type(),
                    LLVMTypeRef.PointerType(
                        GetLlvmRepresentationOf(resolvedArrayType.UnderlyingType, scope),
                        0)
                }, false);
                var allocatedStructure = LLVM.BuildMalloc(builder, structTypeToAllocate, "allocate array struct");

                var arrayTypeToAllocate = GetLlvmRepresentationOf(resolvedArrayType.UnderlyingType, scope);
                var constArraySize = resolvedArrayType.ConstantArraySize!;

                var allocatedArray = LLVM.BuildArrayMalloc(
                    builder,
                    arrayTypeToAllocate,
                    LLVM.ConstInt(LLVMTypeRef.Int32Type(), (ulong)constArraySize.Value, false),
                    "allocate array");
                for (int index = 0; index < constArraySize.Value; ++index)
                {
                    (var element, _) =
                        RecursivelyInitComplexStructure(resolvedArrayType.UnderlyingType, builder, scope, module);
                    if (element == null)
                    {
                        continue;
                    }

                    var pointerToArrayElement = LLVM.BuildGEP(builder, allocatedArray,
                        new[] { LLVM.ConstInt(LLVMTypeRef.Int32Type(), (ulong)index, false) }, "array element");
                    LLVM.BuildStore(builder, element.Value, pointerToArrayElement);
                }

                var sizePointer = LLVM.BuildStructGEP(builder, allocatedStructure, 0, "Array size");
                LLVM.BuildStore(
                    builder,
                    LLVM.ConstInt(LLVMTypeRef.Int32Type(), (ulong)constArraySize.Value, false),
                    sizePointer);

                var arrayPointer = LLVM.BuildStructGEP(builder, allocatedStructure, 1, "Array itself");
                LLVM.BuildStore(
                    builder,
                    allocatedArray,
                    arrayPointer);

                return (allocatedStructure, builder);
            case ResolvedRecordType resolvedRecordType:

                var sortedNames = resolvedRecordType.SortedVariableNames;
                var sortedTypes = sortedNames.Select(name => resolvedRecordType.Variables[name]).ToArray();
                var sortedTypesInLlvm = sortedTypes.Select(memberType => GetLlvmRepresentationOf(memberType, scope))
                    .ToArray();
                var recordTypeToAllocate = LLVMTypeRef.StructType(sortedTypesInLlvm, false);
                var allocatedRecordType = LLVM.BuildMalloc(builder, recordTypeToAllocate, "allocate record struct");

                foreach (var (memberName, index) in resolvedRecordType
                             .SortedVariableNames
                             .Select((name, index) => (name, index)))
                {
                    var memberType = resolvedRecordType.Variables[memberName];
                    (var memberElementToInsert, _) =
                        RecursivelyInitComplexStructure(memberType, builder, scope, module);
                    if (memberElementToInsert == null)
                    {
                        continue;
                    }

                    var memberPointer = LLVM.BuildStructGEP(builder, allocatedRecordType, (uint)index,
                        $"Record member {memberName}");

                    LLVM.BuildStore(
                        builder,
                        memberElementToInsert.Value,
                        memberPointer);
                }

                return (allocatedRecordType, builder);
            case ResolvedBoolType resolvedBoolType:
                return (null, builder);
            case ResolvedIntType resolvedIntType:
                return (null, builder);
            case ResolvedRealType resolvedRealType:
                return (null, builder);
            default:
                throw new ArgumentOutOfRangeException(nameof(type));
        }
    }

    private static (LLVMValueRef, LLVMBuilderRef) ConvertVariable(LLVMValueRef initialValue, IResolvedType initialType,
        IResolvedType destinationType, Scope functionScope, LLVMBuilderRef builder, LLVMModuleRef module)
    {
        switch (initialType, destinationType)
        {
            case (ResolvedIntType, ResolvedIntType):
            case (ResolvedBoolType, ResolvedBoolType):
            case (ResolvedRealType, ResolvedRealType):
            case (ResolvedArrayType, ResolvedArrayType):
            case (ResolvedRecordType, ResolvedRecordType):
                return (initialValue, builder);
            case (ResolvedIntType, ResolvedRealType):
                return (
                    LLVM.BuildSIToFP(builder, initialValue, GetLlvmRepresentationOf(destinationType, functionScope),
                        "int->real"), builder);
            case (ResolvedIntType, ResolvedBoolType):
                return (
                    LLVM.BuildSelect(builder,
                        If: LLVM.BuildICmp(builder, LLVMIntPredicate.LLVMIntEQ, initialValue,
                            LLVM.ConstInt(LLVMTypeRef.Int32Type(), 1, false), "If int is 1"),
                        Then: LLVM.ConstInt(LLVM.Int1Type(), 1, false),
                        Else: LLVM.BuildSelect(builder,
                            If: LLVM.BuildICmp(builder, LLVMIntPredicate.LLVMIntEQ, initialValue,
                                LLVM.ConstInt(LLVMTypeRef.Int32Type(), 0, false), "If int is 0"),
                            Then: LLVM.ConstInt(LLVM.Int1Type(), 0, false),
                            Else: LLVM.ConstInt(LLVM.Int1Type(), 0, false), // TODO: throw error here
                            "int->bool"),
                        "int->bool"),
                    builder);
            case (ResolvedRealType, ResolvedIntType):
                return (
                    LLVM.BuildFPToSI(builder, initialValue, GetLlvmRepresentationOf(destinationType, functionScope),
                        "real->int"), builder);
            case (ResolvedBoolType, ResolvedIntType):
                return (
                    LLVM.BuildSelect(builder,
                        If: initialValue,
                        Then: LLVM.ConstInt(LLVM.Int32Type(), 1, false),
                        Else: LLVM.ConstInt(LLVM.Int32Type(), 0, false),
                        "bool->int"), builder);
            case (ResolvedBoolType, ResolvedRealType):
                return (
                    LLVM.BuildSelect(builder,
                        If: initialValue,
                        Then: LLVM.ConstReal(LLVM.DoubleType(), 1.0),
                        Else: LLVM.ConstReal(LLVM.DoubleType(), 0.0),
                        "bool->real"),
                    builder);
            default:
                throw new ArgumentOutOfRangeException((initialType, destinationType).ToString());
        }
    }

    private static LLVMValueRef GetDefaultForType(IResolvedType type, Scope scope)
        => type switch
        {
            ResolvedArrayType resolvedArrayType => LLVM.ConstNull(GetLlvmRepresentationOf(resolvedArrayType, scope)),
            ResolvedBoolType => LLVM.ConstInt(LLVMTypeRef.Int1Type(), 0, false),
            ResolvedIntType => LLVM.ConstInt(LLVMTypeRef.Int32Type(), 0, false),
            ResolvedRealType => LLVM.ConstReal(LLVMTypeRef.DoubleType(), 0.0),
            ResolvedRecordType resolvedRecordType => LLVM.ConstNull(GetLlvmRepresentationOf(resolvedRecordType, scope)),
            _ => throw new ArgumentOutOfRangeException(nameof(type))
        };

    private static (Scope, LLVMBuilderRef) Visit(VariableDeclaration variableDeclaration, Scope functionScope,
        LLVMBuilderRef builder, LLVMModuleRef module, bool useGlobal = false)
    {
        switch (variableDeclaration.Type, variableDeclaration.Expression)
        {
            // var a is 2;
            case (null, { } someExpression):
            {
                // Not null because we type-checked before
                var inferredType = someExpression.TryInferType(functionScope).InferredType!;
                var (llvmExpression, _) = Visit(someExpression, functionScope, builder, module);
                var inferredTypeInLlvm = GetLlvmRepresentationOf(inferredType, functionScope);
                LLVMValueRef variable;
                if (useGlobal)
                {
                    variable = LLVM.AddGlobal(module, inferredTypeInLlvm, $"global {variableDeclaration.Name}");
                    var initialValue = GetDefaultForType(inferredType, functionScope);
                    LLVM.SetInitializer(variable, initialValue);
                }
                else
                {
                    variable = LLVM.BuildAlloca(builder, inferredTypeInLlvm, $"allocating {variableDeclaration.Name}");
                }

                LLVM.BuildStore(builder, llvmExpression, variable);

                // Not null because we type-checked before
                var declaredEntity = variableDeclaration.AsDeclaredEntity(functionScope).DeclaredEntity!;
                functionScope = functionScope.AddOrOverwrite(
                    declaredEntity with
                    {
                        LlvmVariable = variable
                    });
                return (functionScope, builder);
            }
            // var a: int;
            case ({ } someType, null):
            {
                var resolvedType = someType.TryResolveType(functionScope, isInFunctionSignature: false)
                    .InferredType!;
                var resolvedTypeInLlvm = GetLlvmRepresentationOf(resolvedType, functionScope);
                var (initialValue, _) =
                    RecursivelyInitComplexStructure(resolvedType, builder, functionScope, module);
                LLVMValueRef variable;
                if (useGlobal)
                {
                    variable = LLVM.AddGlobal(module, resolvedTypeInLlvm, $"global {variableDeclaration.Name}");
                    LLVM.SetInitializer(variable, GetDefaultForType(resolvedType, functionScope));
                }
                else
                    variable = LLVM.BuildAlloca(builder, resolvedTypeInLlvm, $"allocating {variableDeclaration.Name}");

                if (initialValue != null)
                {
                    LLVM.BuildStore(builder, initialValue.Value, variable);
                }

                // Not null because we type-checked before
                var declaredEntity = variableDeclaration.AsDeclaredEntity(functionScope).DeclaredEntity!;
                functionScope = functionScope.AddOrOverwrite(
                    declaredEntity with
                    {
                        LlvmVariable = variable
                    });


                return (functionScope, builder);
            }
            // var a: int is 2;
            case ({ } someType, { } someExpression):
            {
                var resolvedType = someType.TryResolveType(functionScope, isInFunctionSignature: false)
                    .InferredType!;
                // Not null because type-check
                var expressionType = someExpression.TryInferType(functionScope).InferredType!;
                var (llvmExpression, _) = Visit(someExpression, functionScope, builder, module);
                var resolvedTypeInLlvm = GetLlvmRepresentationOf(resolvedType, functionScope);
                LLVMValueRef variable;
                if (useGlobal)
                {
                    variable = LLVM.AddGlobal(module, resolvedTypeInLlvm, $"global {variableDeclaration.Name}");
                    LLVM.SetInitializer(variable, GetDefaultForType(resolvedType, functionScope));
                }
                else
                {
                    variable = LLVM.BuildAlloca(builder, resolvedTypeInLlvm, $"allocating {variableDeclaration.Name}");
                }

                (var convertedExpression, _) = ConvertVariable(llvmExpression, 
                    expressionType, 
                    resolvedType,
                    functionScope,
                    builder,
                    module);

                LLVM.BuildStore(builder, convertedExpression, variable);

                // Not null because we type-checked before
                var declaredEntity = variableDeclaration.AsDeclaredEntity(functionScope).DeclaredEntity!;
                functionScope = functionScope.AddOrOverwrite(
                    declaredEntity with
                    {
                        LlvmVariable = variable
                    });
                return (functionScope, builder);
            }
            default:
                throw new ArgumentOutOfRangeException();
        }
    }

    private static (Scope functionScope, LLVMBuilderRef builder) Visit(
        TypeDeclaration typeDeclaration,
        Scope functionScope,
        LLVMBuilderRef builder,
        LLVMModuleRef module)
    {
        // Not null because we type-checked before
        var declaredEntity = typeDeclaration.AsDeclaredEntity(functionScope).DeclaredEntity!;
        return (functionScope.AddOrOverwrite(
            declaredEntity with
            {
                LlvmType = GetLlvmRepresentationOf(declaredEntity.Type, functionScope)
            }), builder);
    }
    
    private static ((Scope, LLVMBuilderRef), bool isTerminated) Visit(
        ForLoop forLoop,
        Scope currentScope,
        LLVMBuilderRef builder,
        LLVMModuleRef module,
        LLVMValueRef currentFunction)
    {
        var initialScope = currentScope with { };
        var preheaderBB = LLVM.GetInsertBlock(builder);
        var (startRange, _) = Visit(forLoop.Range.Start, currentScope, builder, module);
        var (endRange, _) = Visit(forLoop.Range.End, currentScope, builder, module);
        if (forLoop.Range.IsReversed)
        {
            (startRange, endRange) = (endRange, startRange);
        }
        var forHeadBlock = LLVM.AppendBasicBlock(currentFunction, "for.header");
        var exitBlock = LLVM.AppendBasicBlock(currentFunction, "for.end");
        LLVM.BuildBr(builder, forHeadBlock);
        LLVM.PositionBuilderAtEnd(builder, forHeadBlock);
        var variable = LLVM.BuildPhi(builder, LLVM.Int32Type(), forLoop.IteratorName);
        LLVM.AddIncoming(variable, new []{startRange}, new []{preheaderBB}, 1);
        var cond = forLoop.Range.IsReversed
            ? LLVM.BuildICmp(builder, LLVMIntPredicate.LLVMIntSGE, variable, endRange, "cond")
            : LLVM.BuildICmp(builder, LLVMIntPredicate.LLVMIntSLE, variable, endRange, "cond");
            
        var latch = LLVM.AppendBasicBlock(currentFunction, "for.latch");
        var forBodyBlock = LLVM.AppendBasicBlock(currentFunction, "for.body");
        var inc = LLVM.ConstInt(LLVMTypeRef.Int32Type(), 1, false);
        
        LLVM.BuildCondBr(builder, cond, forBodyBlock, exitBlock);
        LLVM.PositionBuilderAtEnd(builder, latch);
        LLVMValueRef nextVar = forLoop.Range.IsReversed
            ? LLVM.BuildSub(builder, variable, inc, "next")
            : LLVM.BuildAdd(builder, variable, inc, "next");
        LLVM.AddIncoming(variable, new []{nextVar}, new []{latch}, 1);
        LLVM.BuildBr(builder, forHeadBlock);
        LLVM.PositionBuilderAtEnd(builder, forBodyBlock);
        var inBodyIter = LLVM.BuildAlloca(builder, LLVMTypeRef.Int32Type(), ""); // добавялем итератор в бади
        LLVM.BuildStore(builder, variable, inBodyIter);
        currentScope = currentScope.AddOrOverwrite(
            new DeclaredVariable(forLoop.IteratorName, new ResolvedIntType(), inBodyIter)
        );
        var (isWhileBodyTerminated, _) = VisitBody(currentScope, forLoop.Body, builder, module, currentFunction);
        if (!isWhileBodyTerminated)
        {
            LLVM.BuildBr(builder, latch);
        }
        LLVM.PositionBuilderAtEnd(builder, exitBlock);
        return ((initialScope, builder), false);
    }

    private static ((Scope, LLVMBuilderRef), bool isTerminated) Visit(
        WhileLoop whileLoop,
        Scope currentScope,
        LLVMBuilderRef builder,
        LLVMModuleRef module,
        LLVMValueRef currentFunction)
    {
        var initialScope = currentScope with { };

        var whileCondBlock = LLVM.AppendBasicBlock(currentFunction, "while-cond");
        var exitBlock = LLVM.AppendBasicBlock(currentFunction, "while-end");
        LLVM.BuildBr(builder, whileCondBlock);
        LLVM.PositionBuilderAtEnd(builder, whileCondBlock);

        (var conditionInLlvm, _) = Visit(whileLoop.Condition, currentScope, builder, module);

        var whileBodyBlock = LLVM.AppendBasicBlock(currentFunction, "while-body");
        LLVM.BuildCondBr(builder, conditionInLlvm, whileBodyBlock, exitBlock);
        LLVM.PositionBuilderAtEnd(builder, whileBodyBlock);
        (var isWhileBodyTerminated, _) = VisitBody(currentScope, whileLoop.Body, builder, module, currentFunction);
        if (!isWhileBodyTerminated)
        {
            LLVM.BuildBr(builder, whileCondBlock);
        }
        LLVM.PositionBuilderAtEnd(builder, exitBlock);
        return ((initialScope, builder), false);
    }

    private static ((Scope, LLVMBuilderRef), bool isTerminated) Visit(
        IfStatement ifStatement,
        Scope currentScope,
        LLVMBuilderRef builder,
        LLVMModuleRef module,
        LLVMValueRef currentFunction)
    {
        var initialScope = currentScope with { };

        (var conditionInLlvm, _) = Visit(ifStatement.Condition, currentScope, builder, module);
        var exit = LLVM.AppendBasicBlock(currentFunction, "if-end");
        var ifBlock = LLVM.AppendBasicBlock(currentFunction, "if");
        var elseBlock = ifStatement.ElseBody == null
            ? null
            : (LLVMBasicBlockRef?)LLVM.AppendBasicBlock(currentFunction, "else");
        var isExitBlockUsed = false;
        LLVM.BuildCondBr(builder, conditionInLlvm, ifBlock, elseBlock ?? exit);
        {
            // then
            LLVM.PositionBuilderAtEnd(builder, ifBlock);
            var (isThenTerminated, thenBuilder) =
                VisitBody(currentScope, ifStatement.ThenBody, builder, module, currentFunction);

            if (!isThenTerminated)
            {
                LLVM.BuildBr(thenBuilder, exit);
            }

            isExitBlockUsed = !isThenTerminated;
        }
        if (ifStatement.ElseBody != null && elseBlock != null)
        {
            // else
            LLVM.PositionBuilderAtEnd(builder, elseBlock.Value);
            var (isElseTerminated, _) = VisitBody(currentScope, ifStatement.ElseBody, builder, module, currentFunction);

            if (!isElseTerminated)
            {
                LLVM.BuildBr(builder, exit);
            }

            isExitBlockUsed = isExitBlockUsed || !isElseTerminated;
        }
        else
        {
            isExitBlockUsed = true;
        }

        if (isExitBlockUsed)
        {
            LLVM.PositionBuilderAtEnd(builder, exit);
            return ((initialScope, builder), false);
        }
        else
        {
            // Means both branches terminate
            LLVM.RemoveBasicBlockFromParent(exit);
            return ((initialScope, builder), true);
        }
    }


    private static (Scope, LLVMBuilderRef) Visit(Return @return, Scope currentScope, LLVMBuilderRef builder,
        LLVMModuleRef module)
    {
        switch (@return.ReturnValue)
        {
            case null:
                LLVM.BuildRetVoid(builder);
                break;
            case not null:
                (var returnValue, _) = Visit(@return.ReturnValue, currentScope, builder, module);
                LLVM.BuildRet(builder, returnValue);
                break;
        }

        return (currentScope, builder);
    }

    private static (LLVMValueRef, LLVMBuilderRef) Visit(Expression expression, Scope currentScope,
        LLVMBuilderRef builder,
        LLVMModuleRef module)
    {
        (var currentValue, _) = Visit(expression.First, currentScope, builder, module);
        foreach (var (opType, relation, _) in expression.Operations)
        {
            (var nextRelation, _) = Visit(relation, currentScope, builder, module);
            currentValue = opType switch
            {
                RelationOperationType.And => LLVM.BuildAnd(builder, currentValue, nextRelation, ""),
                RelationOperationType.Or => LLVM.BuildOr(builder, currentValue, nextRelation, ""),
                RelationOperationType.Xor => LLVM.BuildXor(builder, currentValue, nextRelation, ""),
                _ => throw new ArgumentOutOfRangeException()
            };
        }

        return (currentValue, builder);
    }


    private static LLVMValueRef SimpleOpeartionForIntegerLLVM(SimpleOperationType operationType, LLVMBuilderRef builder,
        LLVMValueRef curr, LLVMValueRef next)
    {
        return operationType switch
        {
            SimpleOperationType.Less => LLVM.BuildICmp(builder, LLVMIntPredicate.LLVMIntSLT, curr,
                next, ""),
            SimpleOperationType.LessOrEqual => LLVM.BuildICmp(builder, LLVMIntPredicate.LLVMIntSLE, curr,
                next, ""),
            SimpleOperationType.Greater => LLVM.BuildICmp(builder, LLVMIntPredicate.LLVMIntSGT, curr,
                next, ""),
            SimpleOperationType.GreaterOrEqual => LLVM.BuildICmp(builder, LLVMIntPredicate.LLVMIntSGE, curr,
                next, ""),
            SimpleOperationType.Equal => LLVM.BuildICmp(builder, LLVMIntPredicate.LLVMIntEQ, curr,
                next, ""),
            SimpleOperationType.NotEqual => LLVM.BuildICmp(builder, LLVMIntPredicate.LLVMIntNE, curr,
                next, ""),
            _ => throw new ArgumentOutOfRangeException()
        };
    }

    private static LLVMValueRef SimpleOpeartionForRealLLVM(SimpleOperationType operationType, LLVMBuilderRef builder,
        LLVMValueRef curr, LLVMValueRef next)
    {
        return operationType switch
        {
            SimpleOperationType.Less => LLVM.BuildFCmp(builder, LLVMRealPredicate.LLVMRealULT, curr,
                next, ""),
            SimpleOperationType.LessOrEqual => LLVM.BuildFCmp(builder, LLVMRealPredicate.LLVMRealULE, curr,
                next, ""),
            SimpleOperationType.Greater => LLVM.BuildFCmp(builder, LLVMRealPredicate.LLVMRealUGT, curr,
                next, ""),
            SimpleOperationType.GreaterOrEqual => LLVM.BuildFCmp(builder, LLVMRealPredicate.LLVMRealUGE, curr,
                next, ""),
            SimpleOperationType.Equal => LLVM.BuildFCmp(builder, LLVMRealPredicate.LLVMRealUEQ, curr,
                next, ""),
            SimpleOperationType.NotEqual => LLVM.BuildFCmp(builder, LLVMRealPredicate.LLVMRealUNE, curr,
                next, ""),
            _ => throw new ArgumentOutOfRangeException()
        };
    }

    private static LLVMValueRef SimpleOpeartionForBooleanLLVM(SimpleOperationType operationType, LLVMBuilderRef builder,
        LLVMValueRef curr, LLVMValueRef next)
    {
        return operationType switch
        {
            SimpleOperationType.Equal => LLVM.BuildICmp(builder, LLVMIntPredicate.LLVMIntEQ, curr,
                next, ""),
            SimpleOperationType.NotEqual => LLVM.BuildICmp(builder, LLVMIntPredicate.LLVMIntNE, curr,
                next, ""),
            _ => throw new ArgumentOutOfRangeException()
        };
    }

    private static (LLVMValueRef, LLVMBuilderRef) Visit(Relation relation, Scope currentScope, LLVMBuilderRef builder,
        LLVMModuleRef module)
    {
        (var currentValue, _) = Visit(relation.First, currentScope, builder, module);
        if (relation.Operation is var (opType, simple, _))
        {
            var inferredType = simple.TryInferType(currentScope).InferredType;
            (var nextSimple, _) = Visit(simple, currentScope, builder, module);
            currentValue = inferredType switch
            {
                ResolvedIntType => SimpleOpeartionForIntegerLLVM(opType, builder, currentValue, nextSimple),
                ResolvedBoolType => SimpleOpeartionForBooleanLLVM(opType, builder, currentValue, nextSimple),
                ResolvedRealType => SimpleOpeartionForRealLLVM(opType, builder, currentValue, nextSimple),
                _ => throw new ArgumentOutOfRangeException()
            };
        }

        return (currentValue, builder);
    }

    private static LLVMValueRef SummandOpeartionForRealLLVM(SummandOperationType operationType, LLVMBuilderRef builder,
        LLVMValueRef curr, LLVMValueRef next)
    {
        return operationType switch
        {
            SummandOperationType.Plus => LLVM.BuildFAdd(builder, curr, next, ""),
            SummandOperationType.Minus => LLVM.BuildFSub(builder, curr, next, ""),
            _ => throw new ArgumentOutOfRangeException()
        };
    }

    private static LLVMValueRef SummandOpeartionForIntegerLLVM(SummandOperationType operationType,
        LLVMBuilderRef builder,
        LLVMValueRef curr, LLVMValueRef next)
    {
        return operationType switch
        {
            SummandOperationType.Plus => LLVM.BuildAdd(builder, curr, next, ""),
            SummandOperationType.Minus => LLVM.BuildSub(builder, curr, next, ""),
            _ => throw new ArgumentOutOfRangeException()
        };
    }

    private static (LLVMValueRef, LLVMBuilderRef) Visit(Simple simple, Scope currentScope, LLVMBuilderRef builder,
        LLVMModuleRef module)
    {
        (var currentValue, _) = Visit(simple.First, currentScope, builder, module);
        foreach (var (opType, summand, _) in simple.Operations)
        {
            var inferredType = simple.TryInferType(currentScope).InferredType;
            (var nextSummand, builder) = Visit(summand, currentScope, builder, module);
            currentValue = inferredType switch
            {
                ResolvedIntType => SummandOpeartionForIntegerLLVM(opType, builder, currentValue, nextSummand),
                ResolvedRealType => SummandOpeartionForRealLLVM(opType, builder, currentValue, nextSummand),
                _ => throw new ArgumentOutOfRangeException()
            };
        }

        return (currentValue, builder);
    }

    private static LLVMValueRef FactorOpeartionForIntegerLLVM(FactorOperationType operationType, LLVMBuilderRef builder,
        LLVMValueRef curr, LLVMValueRef next)
    {
        return operationType switch
        {
            FactorOperationType.Multiplication => LLVM.BuildMul(builder, curr, next, ""),
            FactorOperationType.Division => LLVM.BuildSDiv(builder, curr, next, ""),
            FactorOperationType.ModularDivision => LLVM.BuildSRem(builder, curr, next, ""),
            _ => throw new ArgumentOutOfRangeException()
        };
    }

    private static LLVMValueRef FactorOpeartionForRealLLVM(FactorOperationType operationType, LLVMBuilderRef builder,
        LLVMValueRef curr, LLVMValueRef next)
    {
        return operationType switch
        {
            FactorOperationType.Multiplication => LLVM.BuildFMul(builder, curr, next, ""),
            FactorOperationType.Division => LLVM.BuildFDiv(builder, curr, next, ""),
            FactorOperationType.ModularDivision => LLVM.BuildFRem(builder, curr, next, ""),
            _ => throw new ArgumentOutOfRangeException()
        };
    }

    private static (LLVMValueRef, LLVMBuilderRef) Visit(Summand summand, Scope currentScope, LLVMBuilderRef builder,
        LLVMModuleRef module)
    {
        (var currentValue, _) = Visit(summand.First, currentScope, builder, module);
        foreach (var (factorOperationType, factor, _) in summand.Operations)
        {
            var inferredType = summand.TryInferType(currentScope).InferredType;
            (var nextFactor, _) = Visit(factor, currentScope, builder, module);
            currentValue = inferredType switch
            {
                ResolvedIntType => FactorOpeartionForIntegerLLVM(factorOperationType, builder, currentValue,
                    nextFactor),
                ResolvedRealType => FactorOpeartionForRealLLVM(factorOperationType, builder, currentValue, nextFactor),
                _ => throw new ArgumentOutOfRangeException()
            };
        }

        return (currentValue, builder);
    }

    private static (LLVMValueRef, LLVMBuilderRef) Visit(IFactor factor, Scope currentScope, LLVMBuilderRef builder,
        LLVMModuleRef module)
    {
        switch (factor)
        {
            case ExpressionFactor expressionFactor:
                return Visit(expressionFactor.Expression, currentScope, builder, module);
                break;
            case BoolPrimary boolPrimary:
                return (boolPrimary.Value
                        ? LLVM.ConstInt(LLVMTypeRef.Int1Type(), 1, false)
                        : LLVM.ConstInt(LLVMTypeRef.Int1Type(), 0, false),
                    builder);
            case IntegerPrimary constInt:
                return (constInt.Literal >= 0
                        ? LLVM.ConstInt(LLVMTypeRef.Int32Type(), (ulong)constInt.Literal, false)
                        : LLVM.ConstNeg(LLVM.ConstInt(LLVMTypeRef.Int32Type(), (ulong)(-constInt.Literal), false)),
                    builder);
            case ModifiablePrimary modifiablePrimary:
                // TODO: get full name as string for LLVM IR
                var (variableReference, _) = GetVariableReferenceFrom(modifiablePrimary, currentScope, builder, module);
                return (LLVM.BuildLoad(builder, variableReference, $"var"), builder);
            case RealPrimary realPrimary:
                return (LLVM.ConstReal(LLVMTypeRef.DoubleType(), realPrimary.Literal), builder);
            case RoutineCall routineCall:
                if (routineCall.RoutineName == "LengthOf")
                {
                    var singleArrayExpression = routineCall.Arguments.First();
                    (var array, _) = Visit(singleArrayExpression, currentScope, builder, module);
                    return (LLVM.BuildLoad(builder, LLVM.BuildStructGEP(builder, array, 0, "array size"), "array size"),
                        builder);
                }

                if (routineCall.RoutineName == "Print")
                {
                    var singlePrimitiveTypeExpression = routineCall.Arguments.First();
                    var resolvedType = singlePrimitiveTypeExpression.TryInferType(currentScope).InferredType!;
                    LLVMValueRef primitiveValue;
                    switch (resolvedType)
                    {
                        case ResolvedArrayType:
                            primitiveValue = LLVM.BuildGlobalStringPtr(builder, "<array>", "");
                            break;
                        case ResolvedRecordType:
                            primitiveValue = LLVM.BuildGlobalStringPtr(builder, "<record>", "");
                            break;
                        default:
                            (primitiveValue, _) = Visit(singlePrimitiveTypeExpression, currentScope, builder, module);
                            break;
                    }
                    var printfFunc = LLVM.GetNamedFunction(module, "printf");
                    var formatString = CreateFormatString(builder, resolvedType);
                    return  (LLVM.BuildCall(builder, printfFunc, new LLVMValueRef[] {formatString, primitiveValue}, ""), builder);
                }

                var functionInLlvm = LLVM.GetNamedFunction(module, routineCall.RoutineName);
                var functionInAst = currentScope.DeclaredEntities[routineCall.RoutineName].Cast<DeclaredRoutine>();
                List<LLVMValueRef> argumentsInLlvm = new List<LLVMValueRef>();
                foreach (var (argExpression, expectedArgType) in routineCall.Arguments.Zip(
                             functionInAst.Arguments.Select(typeWithName => typeWithName.Type)))
                {
                    // Not null because we type-checked
                    var argExpressionType = argExpression.TryInferType(currentScope).InferredType!;
                    (var argInLlvm, _) = Visit(argExpression, currentScope, builder, module);
                    (var convertedArgExpressionInLlvm, _) = ConvertVariable(argInLlvm, argExpressionType,
                        expectedArgType.Cast<ResolvedDeclaredRoutineArgumentType>().ArgumentType,
                        currentScope, builder, module);
                    argumentsInLlvm.Add(convertedArgExpressionInLlvm);
                }

                var routineCallReturn = LLVM.BuildCall(builder, functionInLlvm, argumentsInLlvm.ToArray(), "");
                return (routineCallReturn, builder);
            default:
                throw new ArgumentOutOfRangeException(nameof(factor));
        }
    }


    public static void StartExecution(string outputFilePath, IDeclaredRoutineReturnType entryPointRetTp,
        Scope globalScope, Program initialAst)
    {
        var module = LLVM.ModuleCreateWithName("Imperative program");
        var printfFunc = DeclarePrintf(module);
        LlvmizeAst(globalScope, initialAst, module);
        // var context = LLVM.ContextCreate();

        var (_, builderMain) = CreateMain(module);
        var initGlobals = LLVM.GetNamedFunction(module, "________INIT________");

        LLVM.BuildCall(builderMain, initGlobals,
            new LLVMValueRef[]
            {
            }, "");
        
        var entryPoint = LLVM.GetNamedFunction(module, "EntryPoint");

        LLVM.BuildCall(builderMain, printfFunc,
            new LLVMValueRef[]
            {
                CreateFormatString(builderMain, (entryPointRetTp as ResolvedDeclaredRoutineReturnType)!.ReturnType!),
                LLVM.BuildCall(builderMain, entryPoint, Array.Empty<LLVMValueRef>(), "")
            }, "");

        var constInt0 = LLVM.ConstInt(LLVM.Int32Type(), 0, true);
        LLVM.BuildRet(builderMain, constInt0);
        LLVM.DumpModule(module);
        LLVM.VerifyModule(module, LLVMVerifierFailureAction.LLVMPrintMessageAction, out var message);
        LLVM.WriteBitcodeToFile(module, outputFilePath);
        RunMain(module);
    }
}