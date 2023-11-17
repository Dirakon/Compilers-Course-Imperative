namespace Compiler;

using GiGraph.Dot.Entities.Labels;
using GiGraph.Dot.Extensions;
using GiGraph.Dot.Types.Nodes;
using GiGraph.Dot.Entities.Graphs;
using GiGraph.Dot.Entities.Nodes;

public static class AstVisualizer
{
    public static void VisualizeAst(Program rootNode, string outputFilePath)
    {
        var graph = new DotGraph(directed: false);
        var root = CreateNode(graph, "program", null);
        graph.Nodes.Add(root);
        AddDeclarationsToGraph(rootNode.Declarations, graph, root);
        graph.SaveToFile(outputFilePath);
    }

    private static string GenerateUniqueId()
    {
        var uniqueGuid = Guid.NewGuid();
        return uniqueGuid.ToString("N");
    }

    private static DotNode CreateNode(DotGraph graph, DotLabel label, DotNodeShape? shape)
    {
        var node = new DotNode(GenerateUniqueId())
        {
            Label = label,
            ExternalLabel = null,
            LabelAlignment = null,
            Tooltip = null,
            Color = null,
            FillColor = null,
            ColorScheme = null,
            GradientFillAngle = null,
            BorderWidth = null,
            Shape = shape,
            Padding = null,
            Comment = null,
            EdgeOrderingMode = null,
            GroupName = null,
            SortIndex = null,
            IsRoot = null,
            ObjectId = null,
            Annotation = null,
        };
        graph.Nodes.Add(node);
        return node;
    }

    private static void AddDeclarationsToGraph(INodeList<IDeclaration> declarations, DotGraph graph, DotNode parentNode)
    {
        foreach (var declaration in declarations)
        {
            switch (declaration)
            {
                case RoutineDeclaration routineDeclaration:
                {
                    var routineType = routineDeclaration.ReturnType is null
                        ? ""
                        : routineDeclaration.ReturnType.GetTypeName();
                    var node = CreateNode(graph, $"{routineDeclaration.RoutineName} : {routineType}", null);
                    graph.Edges.Add(parentNode.Id, node.Id);
                    if (routineDeclaration.Parameters is NonEmptyNodeList<Parameter>)
                    {
                        AddParametersToGraph(routineDeclaration.Parameters, graph, node);
                    }

                    AddBodyToGraph("body", routineDeclaration.Body, graph, node);
                    break;
                }
                case VariableDeclaration variableDeclaration:
                {
                    AddVariableToGraph(variableDeclaration, graph, parentNode);
                    break;
                }
                case TypeDeclaration typeDeclaration:
                {
                    var node = CreateNode(graph, $"{typeDeclaration.Name} : {typeDeclaration.Type.GetTypeName()}",
                        DotNodeShape.Cylinder);
                    graph.Edges.Add(parentNode.Id, node.Id);
                    AddTypeToGraph(typeDeclaration.Type, graph, node);
                    break;
                }
            }
        }
    }

    private static void AddTypeToGraph(IType typeDeclarationType, DotGraph graph, DotNode node)
    {
        switch (typeDeclarationType)
        {
            case RecordType recordType:
            {
                foreach (var variable in recordType.Variables)
                {
                    AddVariableToGraph(variable, graph, node);
                }

                break;
            }
            case ArrayType arrayType:
                if (arrayType.SizeExpression != null)
                {
                    AddExpressionToGraph(arrayType.SizeExpression, graph, node);
                }

                AddIdentifierToGraph(arrayType.UnderlyingType.GetTypeName(), graph, node);
                break;
        }
    }

    private static void AddVariableToGraph(VariableDeclaration variableDeclaration, DotGraph graph, DotNode parentNode)
    {
        var varType = variableDeclaration.Type is null ? " " : variableDeclaration.Type.GetTypeName();
        var variable = CreateNode(graph, $"{variableDeclaration.Name} : {varType}", null);
        graph.Edges.Add(parentNode.Id, variable.Id);
        if (variableDeclaration.Type is ArrayType or RecordType)
        {
            AddTypeToGraph(variableDeclaration.Type, graph, variable);
        }

        if (variableDeclaration.Expression != null)
            AddExpressionToGraph(variableDeclaration.Expression, graph, variable);
    }

    private static void AddParametersToGraph(IEnumerable<Parameter> parameters, DotGraph graph, DotNode parentNode)
    {
        var node = CreateNode(graph, "params", DotNodeShape.Box);
        graph.Edges.Add(parentNode.Id, node.Id);
        foreach (var parameter in parameters)
        {
            AddVariableToGraph(new VariableDeclaration(parameter.Name, parameter.Type, null, parameter.LexLocation),
                graph, node);
        }
    }

    private static void AddBodyToGraph(string bodyLabel, IEnumerable<IBodyElement> body, DotGraph graph,
        DotNode parentNode)
    {
        var node = CreateNode(graph, bodyLabel, DotNodeShape.Box);
        graph.Edges.Add(parentNode.Id, node.Id);
        foreach (var bodyEl in body)
        {
            switch (bodyEl)
            {
                case VariableDeclaration variableDeclaration:
                {
                    AddVariableToGraph(variableDeclaration, graph, node);
                    break;
                }
                case TypeDeclaration typeDeclaration:
                {
                    var type = CreateNode(graph, typeDeclaration.Name, null);
                    graph.Edges.Add(node.Id, type.Id);
                    break;
                }
                case Return returnStatement:
                {
                    var returnNode = CreateNode(graph, "return", DotNodeShape.Box);
                    graph.Edges.Add(node.Id, returnNode.Id);
                    if (returnStatement.ReturnValue != null)
                        AddExpressionToGraph(returnStatement.ReturnValue, graph, returnNode);
                    break;
                }
                case IfStatement ifStatement:
                {
                    var ifNode = CreateNode(graph, "if", DotNodeShape.Triangle);
                    graph.Edges.Add(node.Id, ifNode.Id);
                    AddExpressionToGraph(ifStatement.Condition, graph, ifNode);
                    AddBodyToGraph("then", ifStatement.ThenBody, graph, ifNode);
                    if (ifStatement.ElseBody != null)
                        AddBodyToGraph("else", ifStatement.ElseBody, graph, ifNode);
                    break;
                }
                case WhileLoop whileLoop:
                {
                    var whileNode = CreateNode(graph, "while", DotNodeShape.Hexagon);
                    graph.Edges.Add(node.Id, whileNode.Id);
                    AddExpressionToGraph(whileLoop.Condition, graph, whileNode);
                    AddBodyToGraph("body", whileLoop.Body, graph, whileNode);
                    break;
                }
                case Assignment assignment:
                {
                    var assignmentNode = CreateNode(graph, "=", null);
                    graph.Edges.Add(node.Id, assignmentNode.Id);
                    AddModifiablePrimaryToGraph(assignment.Target, graph, assignmentNode);
                    AddExpressionToGraph(assignment.Expression, graph, assignmentNode);
                    break;
                }
                case ForLoop forLoop:
                {
                    var forNode = CreateNode(graph, "for", DotNodeShape.Hexagon);
                    graph.Edges.Add(node.Id, forNode.Id);
                    AddIdentifierToGraph(forLoop.IteratorName, graph, forNode);
                    AddRangeToGraph(forLoop.Range, graph, forNode);
                    AddBodyToGraph("body", forLoop.Body, graph, forNode);
                    break;
                }
            }
        }
    }

    private static void AddRangeToGraph(Range forLoopRange, DotGraph graph, DotNode forNode)
    {
        var node = CreateNode(graph, "range", DotNodeShape.Trapezium);
        graph.Edges.Add(forNode.Id, node.Id);
        if (forLoopRange.IsReversed)
        {
            AddExpressionToGraph(forLoopRange.End, graph, node);
            AddExpressionToGraph(forLoopRange.Start, graph, node);
        }
        else
        {
            AddExpressionToGraph(forLoopRange.Start, graph, node);
            AddExpressionToGraph(forLoopRange.End, graph, node);
        }
    }

    private static DotNode AddExpressionToGraph(Expression exp, DotGraph graph, DotNode parentNode)
    {
        return exp.Operations is NonEmptyNodeList<RelationOperation> nonEmptyNodeList
            ? AddRelationOperationToGraph(exp.First, nonEmptyNodeList, graph,
                parentNode)
            : AddRelationToGraph(exp.First, graph, parentNode);
    }

    private static DotNode AddRelationToGraph(Relation rel, DotGraph graph, DotNode parentNode)
    {
        return rel.Operation == null
            ? AddSimpleToGraph(rel.First, graph, parentNode)
            : AddSimpleOperationToGraph(rel.First, rel.Operation, graph,
                parentNode);
    }

    private static DotNode AddSimpleOperationToGraph(Simple first, SimpleOperation operation,
        DotGraph graph, DotNode parentNode)
    {
        var node = CreateNode(graph, operation.GetType(), null);
        graph.Edges.Add(parentNode.Id, node.Id);
        AddSimpleToGraph(first, graph, node);
        first = operation.Simple;

        return AddSimpleToGraph(first, graph, node);
    }

    private static DotNode AddSimpleToGraph(Simple simple, DotGraph graph, DotNode parentNode)
    {
        return simple.Operations is NonEmptyNodeList<SummandOperation> nonEmptyNodeList
            ? AddSummandOperationToGraph(simple.First, nonEmptyNodeList, graph, parentNode)
            : AddSummandToGraph(simple.First, graph, parentNode);
    }

    private static DotNode AddSummandOperationToGraph(Summand first, NonEmptyNodeList<SummandOperation> operations,
        DotGraph graph, DotNode parentNode)
    {
        DotNode node = null!;
        DotNode previous = null;
        foreach (var op in operations)
        {
            node = CreateNode(graph, op.GetType(), null);
            if (previous != null)
            {
                graph.Edges.Add(node.Id, previous.Id);
                AddSummandToGraph(first, graph, previous);
            }
            else
            {
                AddSummandToGraph(first, graph, node);
            }
            first = op.Summand;
            previous = node;
        }
        graph.Edges.Add(parentNode.Id, node.Id);
        return AddSummandToGraph(first, graph, node);
    }

    private static DotNode AddModifiablePrimaryToGraph(ModifiablePrimary mod, DotGraph graph, DotNode parentNode)
    {
        return mod.Operations is NonEmptyNodeList<IModifiablePrimaryOperation> nonEmptyNodeList
            ? AddModifiablePrimaryOperationToGraph(nonEmptyNodeList, graph,
                AddIdentifierToGraph(mod.Identifier, graph, parentNode))
            : AddIdentifierToGraph(mod.Identifier, graph, parentNode);
    }

    private static DotNode AddModifiablePrimaryOperationToGraph(
        NonEmptyNodeList<IModifiablePrimaryOperation> modOperations, DotGraph graph, DotNode parentNode)
    {
        var parent = parentNode;
        foreach (var operation in modOperations)
        {
            switch (operation)
            {
                case MemberCall memberCall:
                {
                    var node = CreateNode(graph, memberCall.MemberName, null);
                    graph.Edges.Add(parent.Id, node.Id);
                    parent = node;
                    break;
                }
                case ArrayIndexing arrayCall:
                {
                    var node = AddExpressionToGraph(arrayCall.IndexExpression, graph, parent);
                    parent = node;
                    break;
                }
            }
        }

        return parent;
    }

    private static DotNode AddIntegerPrimaryToGraph(IntegerPrimary i, DotGraph graph, DotNode parentNode)
    {
        var node = CreateNode(graph, i.Literal.ToString(), null);
        graph.Edges.Add(parentNode.Id, node.Id);
        return node;
    }

    private static DotNode AddBoolPrimaryToGraph(BoolPrimary boolPrimary, DotGraph graph, DotNode parentNode)
    {
        var node = CreateNode(graph, boolPrimary.Value.ToString(), null);
        graph.Edges.Add(parentNode.Id, node.Id);
        return node;
    }


    private static DotNode AddRelationOperationToGraph(Relation rel, NonEmptyNodeList<RelationOperation> operations,
        DotGraph graph, DotNode parentNode)
    {
        DotNode node = null!;
        foreach (var op in operations)
        {
            node = CreateNode(graph, op.GetType(), null);
            graph.Edges.Add(parentNode.Id, node.Id);
            AddRelationToGraph(rel, graph, node);
            rel = op.Relation;
            parentNode = node;
        }

        return AddRelationToGraph(rel, graph, node);
    }

    private static DotNode AddIdentifierToGraph(string identifier, DotGraph graph, DotNode parentNode)
    {
        var node = CreateNode(graph, identifier, null);
        graph.Edges.Add(parentNode.Id, node.Id);
        return node;
    }

    private static DotNode AddSummandToGraph(Summand sum, DotGraph graph, DotNode parentNode)
    {
        return sum.Operations is NonEmptyNodeList<FactorOperation> nonEmptyNodeList
            ? AddFactorOperationToGraph(sum.First, nonEmptyNodeList, graph,
                parentNode)
            : AddFactorToGraph(sum.First, graph, parentNode);
    }

    private static DotNode AddFactorToGraph(IFactor factor, DotGraph graph, DotNode parentNode)
    {
        try
        {
            return factor switch
            {
                ModifiablePrimary modifiablePrimary =>
                    AddModifiablePrimaryToGraph(modifiablePrimary, graph, parentNode),
                IntegerPrimary integerPrimary => AddIntegerPrimaryToGraph(integerPrimary, graph, parentNode),
                BoolPrimary boolPrimary => AddBoolPrimaryToGraph(boolPrimary, graph, parentNode),
                RealPrimary realPrimary => AddRealPrimaryToGraph(realPrimary, graph, parentNode),
                ExpressionFactor expressionFactor => AddExpressionToGraph(expressionFactor.Expression, graph,
                    parentNode),
                RoutineCall routineCall => AddRoutineCallToGraph(routineCall, graph, parentNode),
                _ => throw new ArgumentOutOfRangeException(nameof(factor), factor, null)
            };
        }
        catch (Exception e)
        {
            Console.WriteLine("Oops, we forgot process smth :(");
            throw;
        }
    }

    private static DotNode AddRealPrimaryToGraph(RealPrimary realPrimary, DotGraph graph, DotNode parentNode)
    {
        var node = CreateNode(graph, realPrimary.Literal.ToString(), null);
        graph.Edges.Add(parentNode.Id, node.Id);
        return node;
    }

    private static DotNode AddRoutineCallToGraph(RoutineCall routineCall, DotGraph graph, DotNode parentNode)
    {
        var node = CreateNode(graph, routineCall.RoutineName, null);
        graph.Edges.Add(parentNode.Id, node.Id);
        if (routineCall.Arguments is not NonEmptyNodeList<Expression> callArguments) return node;
        foreach (var arg in callArguments)
        {
            AddExpressionToGraph(arg, graph, node);
        }

        return node;
    }

    private static DotNode AddFactorOperationToGraph(IFactor factor, NonEmptyNodeList<FactorOperation> operations,
        DotGraph graph, DotNode parentNode)
    {
        DotNode node = null!;
        DotNode previous = null;
        foreach (var op in operations)
        {
            node = CreateNode(graph, op.GetType(), null);
            if (previous != null)
            {
                graph.Edges.Add(node.Id, previous.Id);
                AddFactorToGraph(factor, graph, previous);
            }
            else
            {
                AddFactorToGraph(factor, graph, node);
            }
            factor = op.Factor;
            previous = node;
        }

        graph.Edges.Add(parentNode.Id, node.Id);
        return AddFactorToGraph(factor, graph, node);
    }
}