using System.Collections.Immutable;
using CommandLine;
using Compiler.TypeChecking;
using Compiler.Utils;
using LLVMSharp;
using Compiler.Utils;

namespace Compiler.CodeGeneration;

public static class GenerateBitcode
{
    private static List<(LLVMTypeRef, IResolvedType)> AllEncounteredTypes = new List<(LLVMTypeRef, IResolvedType)>();

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

    private static LLVMValueRef CreateFormatString(LLVMBuilderRef builder, ResolvedDeclaredRoutineReturnType returnType)
    {
        return returnType.ReturnType switch
        {
            ResolvedIntType => LLVM.BuildGlobalStringPtr(builder, "%d\n", "str"),
            ResolvedRealType => LLVM.BuildGlobalStringPtr(builder, "%f\n", "str"),
            ResolvedBoolType => LLVM.BuildGlobalStringPtr(builder, "%hhu\n", "str"),
            _ => (default) // TODO: error or specific message when unformattable type
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
            ResolvedRealType => LLVMTypeRef.FloatType(),
            ResolvedArrayType resolvedArrayType => throw new NotImplementedException(),
            ResolvedRecordType resolvedRecordType => throw new NotImplementedException(),
            _ => throw new ArgumentOutOfRangeException(nameof(type))
        };
    }

    public static LLVMModuleRef LlvmizeAst(this Scope globalScope)
    {
        // TODO: add all user defined type LLVM repr to scope
        var module = LLVM.ModuleCreateWithName("Hello World");

        var builder = LLVM.CreateBuilder();

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

        // TODO: add all LLVM variables reprs to scope

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
        foreach (var ((type, paramIdentifier), i) in declaredRoutine.Arguments.Select((value, index) => (value, index)))
        {
            var paramValueRef = LLVM.GetParam(function, (uint)i);
            // TODO: add all routine arguments to scope as variables
        }

        var entry = LLVM.AppendBasicBlock(function, "entry");
        LLVM.PositionBuilderAtEnd(builder, entry);
        VisitBody(functionScope, declaredRoutine.Body, builder, module);
    }

    private static void VisitBody(Scope functionScope, INodeList<IBodyElement> body, LLVMBuilderRef builder,
        LLVMModuleRef module)
    {
        foreach (var bodyElement in body)
        {
            (functionScope) = bodyElement switch
            {
                // Not update scope:
                Return @return => Visit(@return, functionScope, builder, module),
                RoutineCall routineCall => throw new NotImplementedException(),
                ForLoop forLoop => throw new NotImplementedException(),
                IfStatement ifStatement => throw new NotImplementedException(),
                Assignment assignment => throw new NotImplementedException(),
                WhileLoop whileLoop => throw new NotImplementedException(),

                // Update scope:
                TypeDeclaration typeDeclaration => throw new NotImplementedException(),
                VariableDeclaration variableDeclaration =>
                    throw new NotImplementedException(), // Visit(variableDeclaration),
                _ => throw new ArgumentOutOfRangeException(nameof(bodyElement))
            };
        }
    }

    private static Scope Visit(Return @return, Scope currentScope, LLVMBuilderRef builder, LLVMModuleRef module)
    {
        switch (@return.ReturnValue)
        {
            case null:
                LLVM.BuildRetVoid(builder);
                break;
            case not null:
                var returnValue = Visit(@return.ReturnValue, currentScope, builder, module);
                LLVM.BuildRet(builder, returnValue);
                break;
        }

        return currentScope;
    }

    private static LLVMValueRef Visit(Expression expression, Scope currentScope, LLVMBuilderRef builder,
        LLVMModuleRef module)
    {
        var currentValue = Visit(expression.First, currentScope, builder, module);
        foreach (var (opType, relation, _) in expression.Operations)
        {
            var nextRelation = Visit(relation, currentScope, builder, module);
            currentValue = opType switch
            {
                RelationOperationType.And => LLVM.BuildAnd(builder, currentValue, nextRelation, ""),
                RelationOperationType.Or => LLVM.BuildOr(builder, currentValue, nextRelation, ""),
                RelationOperationType.Xor => LLVM.BuildXor(builder, currentValue, nextRelation, ""),
                _ => throw new ArgumentOutOfRangeException()
            };
        }

        return currentValue;
    }

    private static LLVMValueRef Visit(Relation relation, Scope currentScope, LLVMBuilderRef builder,
        LLVMModuleRef module)
    {
        var currentValue = Visit(relation.First, currentScope, builder, module);
        if (relation.Operation is var (opType, simple, _))
        {
            var nextSimple = Visit(simple, currentScope, builder, module);
            currentValue = opType switch
            {
                SimpleOperationType.Less => LLVM.BuildICmp(builder, LLVMIntPredicate.LLVMIntSLT, currentValue,
                    nextSimple, ""),
                SimpleOperationType.LessOrEqual => LLVM.BuildICmp(builder, LLVMIntPredicate.LLVMIntSLE, currentValue,
                    nextSimple, ""),
                SimpleOperationType.Greater => LLVM.BuildICmp(builder, LLVMIntPredicate.LLVMIntSGT, currentValue,
                    nextSimple, ""),
                SimpleOperationType.GreaterOrEqual => LLVM.BuildICmp(builder, LLVMIntPredicate.LLVMIntSGE, currentValue,
                    nextSimple, ""),
                SimpleOperationType.Equal => LLVM.BuildICmp(builder, LLVMIntPredicate.LLVMIntEQ, currentValue,
                    nextSimple, ""),
                SimpleOperationType.NotEqual => LLVM.BuildICmp(builder, LLVMIntPredicate.LLVMIntNE, currentValue,
                    nextSimple, ""),
                _ => throw new ArgumentOutOfRangeException()
            };
        }

        return currentValue;
    }

    private static LLVMValueRef Visit(Simple simple, Scope currentScope, LLVMBuilderRef builder, LLVMModuleRef module)
    {
        var currentValue = Visit(simple.First, currentScope, builder, module);
        foreach (var (opType, summand, _) in simple.Operations)
        {
            var nextSummand = Visit(summand, currentScope, builder, module);
            currentValue = opType switch
            {
                SummandOperationType.Plus => LLVM.BuildAdd(builder, currentValue, nextSummand, ""),
                SummandOperationType.Minus => LLVM.BuildSub(builder, currentValue, nextSummand, ""),
                _ => throw new ArgumentOutOfRangeException()
            };
        }

        return currentValue;
    }

    private static LLVMValueRef Visit(Summand summand, Scope currentScope, LLVMBuilderRef builder, LLVMModuleRef module)
    {
        var currentValue = Visit(summand.First, currentScope, builder, module);
        foreach (var (factorOperationType, factor, _) in summand.Operations)
        {
            var nextFactor = Visit(factor, currentScope, builder, module);
            currentValue = factorOperationType switch
            {
                FactorOperationType.Multiplication => LLVM.BuildMul(builder, currentValue, nextFactor, ""),
                FactorOperationType.Division => LLVM.BuildSDiv(builder, currentValue, nextFactor, ""),
                FactorOperationType.ModularDivision => LLVM.BuildSRem(builder, currentValue, nextFactor, ""),
                _ => throw new ArgumentOutOfRangeException()
            };
        }

        return currentValue;
    }

    private static LLVMValueRef Visit(IFactor factor, Scope currentScope, LLVMBuilderRef builder, LLVMModuleRef module)
    {
        switch (factor)
        {
            case ExpressionFactor expressionFactor:
                return Visit(expressionFactor.Expression, currentScope, builder, module);
                break;
            case BoolPrimary boolPrimary:
                throw new NotImplementedException();
                break;
            case IntegerPrimary constInt:
                return constInt.Literal >= 0
                    ? LLVM.ConstInt(LLVMTypeRef.Int32Type(), (ulong)constInt.Literal, false)
                    : LLVM.ConstNeg(LLVM.ConstInt(LLVMTypeRef.Int32Type(), (ulong)(-constInt.Literal), false));
            case ModifiablePrimary modifiablePrimary:
                throw new NotImplementedException();
                break;
            case RealPrimary realPrimary:
                throw new NotImplementedException();
                break;
            case RoutineCall routineCall:
                var function = LLVM.GetNamedFunction(module, routineCall.RoutineName);
                var argumentsInLlvm = routineCall.Arguments.Select(arg => Visit(arg, currentScope, builder, module))
                    .ToArray();
                var routineCallReturn = LLVM.BuildCall(builder, function, argumentsInLlvm, "");
                return routineCallReturn;
            default:
                throw new ArgumentOutOfRangeException(nameof(factor));
        }
    }


    public static void StartExecution(string outputFilePath, IDeclaredRoutineReturnType entryPointRetTp,
        Scope globalScope)
    {
        var module = LlvmizeAst(globalScope);
        // var context = LLVM.ContextCreate();
        var printfFunc = DeclarePrintf(module);

        var (_, builderMain) = CreateMain(module);
        var entryPoint = LLVM.GetNamedFunction(module, "EntryPoint");

        LLVM.BuildCall(builderMain, printfFunc,
            new LLVMValueRef[]
            {
                CreateFormatString(builderMain, (ResolvedDeclaredRoutineReturnType)entryPointRetTp),
                LLVM.BuildCall(builderMain, entryPoint, Array.Empty<LLVMValueRef>(), "")
            }, "");

        var constInt0 = LLVM.ConstInt(LLVM.Int32Type(), 0, true);
        LLVM.BuildRet(builderMain, constInt0);
        LLVM.DumpModule(module);
        LLVM.VerifyModule(module, LLVMVerifierFailureAction.LLVMPrintMessageAction, out var message);
        RunMain(module);
        LLVM.WriteBitcodeToFile(module, outputFilePath);
    }
}