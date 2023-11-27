using CommandLine;
using Compiler.TypeChecking;
using LLVMSharp;

namespace Compiler.CodeGeneration;

public static class GenerateBitcode
{
    private static LLVMValueRef CreateExtern(
        LLVMModuleRef module,
        string name,
        LLVMTypeRef returnType,
        LLVMTypeRef[] parameters,
        bool isVarArg = false
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
    
    private static (LLVMValueRef, LLVMBuilderRef) CreateEntryPoint(LLVMModuleRef module, ResolvedDeclaredRoutineReturnType returnType)
    {
        Console.WriteLine(returnType);
        return returnType.ReturnType switch
        {
            ResolvedIntType => CreateFunction(module, "EntryPoint", LLVMTypeRef.Int32Type(),
                Array.Empty<LLVMTypeRef>()),
            ResolvedRealType => CreateFunction(module, "EntryPoint", LLVMTypeRef.DoubleType(),
                Array.Empty<LLVMTypeRef>()),
            ResolvedBoolType => CreateFunction(module, "EntryPoint", LLVMTypeRef.Int8Type(),
                Array.Empty<LLVMTypeRef>()),
            _ => (default, default)
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


    private static LLVMModuleRef MakeDummyModuleWithEntryPoint(LLVMValueRef returnValue, LLVMTypeRef returnType)
    {
        LLVM.LinkInMCJIT();
        LLVM.InitializeX86TargetMC();
        LLVM.InitializeX86Target();
        LLVM.InitializeX86TargetInfo();
        LLVM.InitializeX86AsmParser();
        LLVM.InitializeX86AsmPrinter();
        var module = LLVM.ModuleCreateWithName("Sample module");

        var builder = LLVM.CreateBuilder();
        var entryPointFunctionType =
            LLVMTypeRef.FunctionType(returnType, Array.Empty<LLVMTypeRef>(), false);
        var entryPointFunction =
            LLVM.AddFunction(module, "EntryPoint", entryPointFunctionType);
        LLVM.SetLinkage(entryPointFunction, LLVMLinkage.LLVMExternalLinkage);
        var bb = entryPointFunction.AppendBasicBlock("entry");
        LLVM.PositionBuilderAtEnd(builder, bb);

        LLVM.BuildRet(builder, returnValue);
        LLVM.SetLinkage(entryPointFunction, LLVMLinkage.LLVMExternalLinkage);
        return module;
    }
    
    public static void StartExecution(string outputFilePath, IDeclaredRoutineReturnType entryPointRetTp)
    {
        var context = LLVM.ContextCreate();
        var module = LLVM.ModuleCreateWithNameInContext("Hello World", context);
        var printfFunc = DeclarePrintf(module);
        
        var (entryPoint, builderEntryPoint) = CreateEntryPoint(module, entryPointRetTp.Cast<ResolvedDeclaredRoutineReturnType>());
        var constInt = LLVM.ConstInt(LLVM.Int32Type(), 52, true);
        LLVM.BuildRet(builderEntryPoint, constInt);
   
        
        var (_, builderMain) = CreateMain(module);
        /*var entryPointOutput = LLVM.BuildCall(builderMain, entryPoint,
            Array.Empty<LLVMValueRef>(), "THIS IS ENTRY POINT OUTPUT");
        LLVM.BuildCall(builderMain, printfFunc, new []{entryPointOutput} , "");*/
        var constStr = LLVM.BuildGlobalStringPtr(builderMain, "Hello World!", "");
        LLVM.BuildCall(builderMain, printfFunc, new LLVMValueRef[] { constStr }, "");
        var constInt0 = LLVM.ConstInt(LLVM.Int32Type(), 0, true);
        LLVM.BuildRet(builderMain, constInt0);
        
        
        LLVM.DumpModule(module);
        LLVM.VerifyModule(module, LLVMVerifierFailureAction.LLVMPrintMessageAction, out var message);
        RunMain(module);
        LLVM.WriteBitcodeToFile(module, outputFilePath);
    }
}