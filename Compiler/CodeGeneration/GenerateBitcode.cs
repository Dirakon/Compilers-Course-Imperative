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
            ResolvedRealType => LLVMTypeRef.DoubleType(),
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
                RoutineCall routineCall => throw new NotImplementedException(),
                ForLoop forLoop => throw new NotImplementedException(),
                IfStatement ifStatement => Visit(ifStatement, functionScope, builder, module, currentFunction),
                Assignment assignment => throw new NotImplementedException(),
                WhileLoop whileLoop => Visit(whileLoop, functionScope, builder, module, currentFunction),

                // Update scope:
                TypeDeclaration typeDeclaration => (Visit(typeDeclaration, functionScope, builder, module), false),
                VariableDeclaration variableDeclaration =>
                    throw new NotImplementedException(), // Visit(variableDeclaration),
                _ => throw new ArgumentOutOfRangeException(nameof(bodyElement))
            };
            if (justTerminated)
            {
                return (true, builder);
            }
        }

        return (false, builder);
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
    
    private static LLVMValueRef SummandOpeartionForIntegerLLVM(SummandOperationType operationType, LLVMBuilderRef builder,
        LLVMValueRef curr, LLVMValueRef next)
    {
        return operationType switch
        {
            SummandOperationType.Plus => LLVM.BuildAdd(builder, curr, next, ""),
            SummandOperationType.Minus => LLVM.BuildSub(builder, curr, next, ""),
            _ => throw new ArgumentOutOfRangeException()
        };
    }
    
    private static (LLVMValueRef, LLVMBuilderRef)  Visit(Simple simple, Scope currentScope, LLVMBuilderRef builder, LLVMModuleRef module)
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
                ResolvedIntType => FactorOpeartionForIntegerLLVM(factorOperationType, builder, currentValue, nextFactor),
                ResolvedRealType => FactorOpeartionForRealLLVM(factorOperationType, builder, currentValue, nextFactor),
                _ => throw new ArgumentOutOfRangeException()
            };
        }

        return (currentValue, builder);
    }
    
    private static (LLVMValueRef, LLVMBuilderRef)  Visit(IFactor factor, Scope currentScope, LLVMBuilderRef builder, LLVMModuleRef module)
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
                throw new NotImplementedException();
                break;
            case RealPrimary realPrimary:
                return (LLVM.ConstReal(LLVMTypeRef.DoubleType(), realPrimary.Literal), builder);
            case RoutineCall routineCall:
                var function = LLVM.GetNamedFunction(module, routineCall.RoutineName);
                List<LLVMValueRef> argumentsInLlvm = new List<LLVMValueRef>();
                foreach (var arg in routineCall.Arguments)
                {
                    (var argInLlvm, _) = Visit(arg, currentScope, builder, module);
                    argumentsInLlvm.Add(argInLlvm);
                }

                var routineCallReturn = LLVM.BuildCall(builder, function, argumentsInLlvm.ToArray(), "");
                return (routineCallReturn, builder);
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