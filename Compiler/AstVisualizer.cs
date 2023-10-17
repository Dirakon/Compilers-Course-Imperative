namespace Compiler;

using GiGraph.Dot.Extensions;
using GiGraph.Dot.Types.Nodes;
using GiGraph.Dot.Entities.Graphs;
using GiGraph.Dot.Entities.Nodes;

public static class AstVisualizer
{
    public static void VisualizeAst(Program rootNode, string outputFilePath)
    {
        var graph = new DotGraph();
        var root = new DotNode("program");
        graph.Nodes.Add(root);
        AddDeclarationsToGraph(rootNode.Declarations, graph, root);
        graph.SaveToFile(outputFilePath);
    }

    private static void AddDeclarationsToGraph(INodeList<IDeclaration> declarations, DotGraph graph, DotNode parentNode)
    {
        foreach (var declaration in declarations)
        {
            switch (declaration)
            {
                case RoutineDeclaration routineDeclaration:
                {
                    var node = new DotNode(routineDeclaration.RoutineName);
                    node.Attributes.Collection.SetEnum("shape", DotNodeShape.Circle);
                    graph.Edges.Add(parentNode.Id, node.Id);
                    AddParametersToGraph(routineDeclaration.Parameters, graph, node);
                    AddBodyToGraph(routineDeclaration.Body, graph, node);
                    break;
                }
                case VariableDeclaration variableDeclaration:
                {
                    var node = new DotNode(variableDeclaration.Name);
                    node.Attributes.Collection.SetEnum("shape", DotNodeShape.Circle);
                    graph.Nodes.Add(node);
                    break;
                }
                case TypeDeclaration typeDeclaration:
                {
                    var node = new DotNode(typeDeclaration.Name);
                    node.Attributes.Collection.SetEnum("shape", DotNodeShape.Circle);
                    graph.Nodes.Add(node);
                    break;
                }
            }
        }
    }
    private static void AddParametersToGraph(INodeList<Parameter> parameters, DotGraph graph, DotNode parentNode)
    {
        var node = new DotNode(parentNode.Id + ": params");
        graph.Edges.Add(parentNode.Id, node.Id);
        foreach (var parameter in parameters)
        {
            var pr = new DotNode(parameter.Name + ":" + parameter.Type.GetTypeName());
            graph.Edges.Add(node.Id, pr.Id);
        }
    }
    private static void AddBodyToGraph(INodeList<IBodyElement> body, DotGraph graph, DotNode parentNode)
    {
        var node = new DotNode(parentNode.Id + ": body");
        graph.Edges.Add(parentNode.Id, node.Id);
        foreach (INode bodyEl in body)
        {
            if (bodyEl is VariableDeclaration variableDeclaration)
            {
                var variable = new DotNode($"{variableDeclaration.Name}:${variableDeclaration.Type}");
                graph.Edges.Add(node.Id, variable.Id);
                if (variableDeclaration.Expresion != null)
                    AddExpressionToGraph(variableDeclaration.Expresion, graph, variable);
            }
            else if (bodyEl is TypeDeclaration typeDeclaration)
            {
                var type = new DotNode(typeDeclaration.Name);
                graph.Edges.Add(node.Id, type.Id);
            }
            else if (bodyEl is Return returnStatement)
            {
                var rtrn = new DotNode("return");
                graph.Edges.Add(node.Id, rtrn.Id);
            }
            
        }
    }
    private static void AddExpressionToGraph(Expression exp, DotGraph graph, DotNode parentNode)
    {
        AddRelationToGraph(exp.First, graph, parentNode);
        AddRelationOperationToGraph(exp.Operations, graph, parentNode);

    }
    private static void AddRelationToGraph(Relation rel, DotGraph graph, DotNode parentNode)
    {
     
        
    }
    private static void AddRelationOperationToGraph(INodeList<RelationOperation> operations, DotGraph graph, DotNode parentNode)
    {
     
        
    }
    private static void AddSimpleToGraph(Simple simple, DotGraph graph, DotNode parentNode)
    {
     
        
    }
    private static void AddSummandToGraph(Summand sum, DotGraph graph, DotNode parentNode)
    {
     
        
    }
    private static void AddFactorToGraph(IFactor fact, DotGraph graph, DotNode parentNode)
    {
     
        
    }
}