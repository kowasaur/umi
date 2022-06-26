using System;
using System.IO;
using System.Collections.Generic;

class Location {

    uint line;
    uint column;

    public Location(uint line, uint column) {
        this.line = line;
        this.column = column;
    }

    public void Increase(char new_char) {
        if (new_char == '\n') {
            column = 1;
            line++;
        } else column++;
    }

    public Location Copy() => new Location(line, column);
    
    public static implicit operator string(Location l) => $"{l.line}:{l.column}";

    // Probably should consider column as well
    public static bool operator >(Location a, Location b) => a.line > b.line;
    public static bool operator <(Location a, Location b) => a.line < b.line;

}

class Token {

    public readonly Type type;
    public readonly string value;
    public readonly Location location;

    public Token(Type type, Location loc, string value) {
        this.type = type;
        location = loc;
        this.value = value;
    }

    public enum Type {
        IDENTIFIER,
        STRING,
        INTEGER,
        LPARAN,
        RPARAN,
        LCURLY,
        RCURLY,
        STATEMENT_END,
        COMMA,
        EQUAL
    }

}

class AstNode {

    public readonly Location location;

    public AstNode(Location loc) {
        location = loc;
    }

    public virtual void GenIl(Dictionary<string, VarDef> _) => throw new NotImplementedException();

    public class Value : AstNode {
        public readonly Token token;
        public Value(Token token) : base(token.location) => this.token = token;

        public override void GenIl(Dictionary<string, VarDef> local_vars) {
            switch (token.type) {
                case Token.Type.STRING:
                    Output.WriteLine($"ldstr \"{token.value}\"");
                    break;
                case Token.Type.INTEGER:
                    Output.WriteLine($"ldc.i4 {token.value}");
                    break;
                case Token.Type.IDENTIFIER:
                    VarDef variable = local_vars[token.value];
                    if (variable.location > location) {
                        Umi.Crash($"Can not use variable `{token.value}` before it is defined", location);
                    }
                    Output.WriteLine($"ldloc {variable.index}");
                    break;
                default:
                    Umi.Crash("No Value IL generation for " + token.type, token.location);
                    break;
            }
        }

        public string Type(Dictionary<string, VarDef> local_vars) {
            switch (token.type) {
                case Token.Type.STRING:
                    return "string";
                case Token.Type.INTEGER:
                    return "int32";
                case Token.Type.IDENTIFIER:
                    VarDef variable;
                    if (!local_vars.TryGetValue(token.value, out variable)) {
                        Umi.Crash($"Variable `{token.value}` not defined", location);
                    };
                    return variable.type;
                default:
                    Umi.Crash("No Value type for " + token.type, token.location);
                    return "unreachable";
            }
        }
    }

    public class Arguments : AstNode {
        public readonly Value[] args;
        public Arguments(Location loc, Value[] args) : base(loc) => this.args = args;
    }

    public class Parameters : AstNode {
        public readonly string[] types;
        public readonly string[] names;
        public Parameters(Location loc, string[] types, string[] names) : base(loc) {
            this.types = types;
            this.names = names;
        }
    }

    public class FuncCall : AstNode {
        readonly string name;
        readonly Value[] arguments;

        public FuncCall(Location loc, string name, Arguments args) : base(loc) {
            this.name = name;
            arguments = args.args;
        }

        public override void GenIl(Dictionary<string, VarDef> local_vars) {
            string[] expected_types = Umi.function_args[name];
            if (expected_types.Length != arguments.Length) {
                Umi.Crash("Incorrect number of arguments", location);
            }
            for (int i = 0; i < arguments.Length; i++) {
                Value arg = arguments[i];
                string expected = expected_types[i];
                string arg_type = arg.Type(local_vars);
                if (arg_type != expected) {
                    Umi.Crash($"Incorrect type of argument. Expected {expected} but found {arg_type}", arg.location);
                }
                arg.GenIl(local_vars);
            }
            string args = String.Join(", ", expected_types);
            Output.WriteLine($"call void {name} ({args})");
        }
    }
    
    public class VarAssign : AstNode {
        public readonly string name;
        readonly Value value;

        public VarAssign(Location loc, string name, Value value) : base(loc) {
            this.name = name;
            this.value = value;
        }

        public override void GenIl(Dictionary<string, VarDef> local_vars) {
            VarDef variable = local_vars[name];
            if (variable.location > location) {
                Umi.Crash($"Can not assign to variable `{name}` before it is defined", location);
            }
            value.GenIl(local_vars);
            Output.WriteLine($"stloc {local_vars[name].index}");
        }
    }

    public class VarDef : AstNode {
        public readonly VarAssign assignment;
        // public readonly string name;
        public readonly string type;
        public int index; // in the function

        public VarDef(Location loc, string type, VarAssign assignment) : base(loc) {
            // this.name = assignment.name;
            this.type = type;
            this.assignment = assignment;
        }
    }

    public class FuncDef : AstNode {
        readonly string name;
        readonly string return_type;
        readonly AstNode[] statements;
        readonly Parameters parameters;
        readonly Dictionary<string, VarDef> local_vars = new Dictionary<string, VarDef>();

        public FuncDef(Location loc, List<AstNode> nodes) : base(loc) {
            return_type = ((Value)nodes[0]).token.value;
            name = ((Value)nodes[1]).token.value;

            parameters = (Parameters)nodes[2];
            if (!Umi.function_args.TryAdd(name, parameters.types)) {
                Umi.Crash($"Function `{name}` already defined", loc);
            }

            int statements_amount = nodes.Count - 5;
            statements = new AstNode[statements_amount];
            for (int j = 4; j < nodes.Count - 1; j++) {
                if (nodes[j] is VarDef) {
                    VarDef variable = (VarDef)nodes[j];
                    string var_name = variable.assignment.name;
                    if (!local_vars.TryAdd(var_name, variable)) {
                        Umi.Crash($"Local variable `{var_name}` already defined", variable.location);
                    };
                    statements[j-4] = variable.assignment;
                } else {
                    statements[j-4] = nodes[j];
                }
            }
        }

        public override void GenIl(Dictionary<string, VarDef> _) {
            Output.WriteLine($".method static void {name}()");
            Output.WriteLine("{");
            Output.Indent();
            if (name == "main") Output.WriteLine(".entrypoint");
            if (local_vars.Count > 0) {
                Output.WriteLine(".locals init (");
                Output.Indent();
                VarDef[] vars = new VarDef[local_vars.Count];
                local_vars.Values.CopyTo(vars, 0);
                for (int i = 0; i < vars.Length - 1; i++) {
                    VarDef variable = vars[i];
                    variable.index = i;
                    Output.WriteLine($"{variable.type} {variable.assignment.name},");
                }
                VarDef last_var = vars[vars.Length - 1];
                last_var.index = vars.Length - 1;
                Output.WriteLine($"{last_var.type} {last_var.assignment.name}");
                Output.Unindent();
                Output.WriteLine(")");
            }

            foreach (var statement in statements) statement.GenIl(local_vars);

            Output.WriteLine("ret");
            Output.Unindent();
            Output.WriteLine("}\n");
        }
    }

}

class Pattern {

    protected Pattern[] tails;
    readonly Token.Type token_type;

    protected Pattern[] subpattern_heads;
    readonly string name;

    Pattern(Pattern[] tails, string name) {
        this.tails = tails;
        this.name = name;
    }

    public Pattern(Token.Type token_type, Pattern[] tails = null) {
        this.token_type = token_type;
        this.tails = tails;
    }

    public static implicit operator string(Pattern p) => p.name == null ? p.token_type.ToString() : p.name;

    protected virtual AstNode GenAst(List<Token> tokens, ref int i) {
        if (tokens[i].type == token_type) return new AstNode.Value(tokens[i++]);
        return null;
    } 

    public List<AstNode> ParseTokens(List<Token> tokens, ref int i) {
        if (subpattern_heads == null) Umi.Crash("ParseTokens being called on primitive", tokens[i].location);
        
        List<AstNode> nodes = new List<AstNode>();
        Pattern[] tails = subpattern_heads;

        while (tails != null && i < tokens.Count) {
            bool matched = false;
            foreach (var child_node in tails) {
                var ast_node = child_node.GenAst(tokens, ref i);
                if (ast_node != null) {
                    nodes.Add(ast_node);
                    tails = child_node.tails;
                    matched = true;
                    break;
                }
            }
            if (!matched) return null;
        }
        
        return nodes;
    }

    public abstract class NotToken : Pattern {
        // TODO: Make a better name lol
        protected abstract AstNode CreateAstNode(Location loc, List<AstNode> nodes);
        protected NotToken(Pattern[] tails, string name) : base(tails, name) {}

        protected override AstNode GenAst(List<Token> tokens, ref int i) {
            Location loc = tokens[i].location;
            int old_i = i;
            List<AstNode> nodes = ParseTokens(tokens, ref i);
            if (nodes == null) {
                i = old_i;
                return null;
            }
            return CreateAstNode(loc, nodes);
        }
    }

    // This is basically literals or variables
    // Maybe I should think of a better name
    class Value : NotToken {
        public Value(Pattern[] tails) : base(tails, "VALUE") {
            var str = new Pattern(Token.Type.STRING);
            var integer = new Pattern(Token.Type.INTEGER);
            var identifier = new Pattern(Token.Type.IDENTIFIER);
            subpattern_heads = new Pattern[] {str, integer, identifier};
        }

        protected override AstNode CreateAstNode(Location _, List<AstNode> nodes) => nodes[0];
    }

    class Arguments : NotToken {
        public Arguments(Pattern[] tails) : base(tails, "ARGUMENTS") {
            var r_paran = new Pattern(Token.Type.RPARAN);
            var additional_argument = new Value(new Pattern[] {r_paran, null});
            var comma = new Pattern(Token.Type.COMMA, new Pattern[] {additional_argument});
            additional_argument.tails[1] = comma;
            var first_argument = new Value(new Pattern[] {r_paran, comma});
            var l_paran = new Pattern(Token.Type.LPARAN, new Pattern[] {r_paran, first_argument});
            subpattern_heads = new Pattern[] {l_paran};
        }

        protected override AstNode CreateAstNode(Location loc, List<AstNode> nodes) {
            AstNode.Value[] args = new AstNode.Value[(nodes.Count - 1)/2];
            // Ignore the brackets and commas
            for (int j = 1; j < nodes.Count - 1; j += 2) {
                args[(j-1)/2] = ((AstNode.Value)nodes[j]);
            }
            return new AstNode.Arguments(loc, args);
        }
    }

    class FuncCall : NotToken {
        public FuncCall(Pattern[] tails) : base(tails, "FUNC_CALL") {
            var arguments = new Arguments(null);
            var name = new Pattern(Token.Type.IDENTIFIER, new Pattern[] {arguments});
            subpattern_heads = new Pattern[] {name};
        }

        protected override AstNode CreateAstNode(Location loc, List<AstNode> nodes) {
            string name = ((AstNode.Value)nodes[0]).token.value;
            AstNode.Arguments args = (AstNode.Arguments)nodes[1];
            return new AstNode.FuncCall(loc, name, args);
        }
    }

    class VarAssign : NotToken {
        public VarAssign(Pattern[] tails) : base(tails, "VAR_ASSIGN") {
            var value = new Value(null);
            var equal = new Pattern(Token.Type.EQUAL, new Pattern[] {value});
            var name = new Pattern(Token.Type.IDENTIFIER, new Pattern[] {equal});
            subpattern_heads = new Pattern[] {name};
        }

        protected override AstNode CreateAstNode(Location loc, List<AstNode> nodes) {
            string name = ((AstNode.Value)nodes[0]).token.value;
            AstNode.Value value = (AstNode.Value)nodes[2];
            return new AstNode.VarAssign(loc, name, value);
        }
    }

    class VarDef : NotToken {
        public VarDef(Pattern[] tails) : base(tails, "VAR_DEF") {
            var assign = new VarAssign(null);
            var type = new Pattern(Token.Type.IDENTIFIER, new Pattern[] {assign});
            subpattern_heads = new Pattern[] {type};
        }

        protected override AstNode CreateAstNode(Location loc, List<AstNode> nodes) {
            string type = ((AstNode.Value)nodes[0]).token.value;
            AstNode.VarAssign assignment = (AstNode.VarAssign)nodes[1];
            return new AstNode.VarDef(loc, type, assignment);
        }
    }

    // Only in function body
    class Statement : NotToken {
        public Statement(Pattern[] tails) : base(tails, "STATEMENT") {
            var semicolon = new Pattern(Token.Type.STATEMENT_END);
            Pattern[] semicolon_array = new Pattern[] {semicolon};
            var func_call = new FuncCall(semicolon_array);
            var var_assign = new VarAssign(semicolon_array);
            var var_def = new VarDef(semicolon_array);
            subpattern_heads = new Pattern[] {func_call, var_assign, var_def};
        }

        protected override AstNode CreateAstNode(Location _, List<AstNode> nodes) => nodes[0];
    }

    class Parameters : NotToken {
        public Parameters(Pattern[] tails) : base(tails, "PARAMETERS") {
            var r_paran = new Pattern(Token.Type.RPARAN);

            var addit_param_var = new Pattern(Token.Type.IDENTIFIER);
            var addit_param_type = new Pattern(Token.Type.IDENTIFIER, new Pattern[] {addit_param_var});
            var comma = new Pattern(Token.Type.COMMA, new Pattern[] {addit_param_type});
            addit_param_var.tails = new Pattern[] {r_paran, comma};

            var first_param_var = new Pattern(Token.Type.IDENTIFIER, new Pattern[] {r_paran, comma});
            var first_param_type = new Pattern(Token.Type.IDENTIFIER, new Pattern[] {first_param_var});
            var l_paran = new Pattern(Token.Type.LPARAN, new Pattern[] {r_paran, first_param_type});
            subpattern_heads = new Pattern[] {l_paran};
        }

        protected override AstNode CreateAstNode(Location loc, List<AstNode> nodes) {
            // TODO: check this maths and other arrays
            string[] types = new string[(nodes.Count - 1)/3];
            string[] names = new string[(nodes.Count - 1)/3];
            // Ignore the brackets and commas
            for (int j = 1; j < nodes.Count - 1; j += 3) {
                types[(j-1)/3] = ((AstNode.Value)nodes[j]).token.value;
                // TODO: pretty sure this should be j + 1
                names[(j-1)/3] = ((AstNode.Value)nodes[j]).token.value;
            }
            return new AstNode.Parameters(loc, types, names);
        }
    }

    class FuncDef : NotToken {
        public FuncDef(Pattern[] tails) : base(tails, "FUNC_DEF") {
            var end_block = new Pattern(Token.Type.RCURLY);
            var statements = new Statement(new Pattern[] {end_block, null});
            statements.tails[1] = statements;
            var start_block = new Pattern(Token.Type.LCURLY, new Pattern[] {statements});
            var parameters = new Parameters(new Pattern[] {start_block});
            var func_def_name = new Pattern(Token.Type.IDENTIFIER, new Pattern[] {parameters});
            var type = new Pattern(Token.Type.IDENTIFIER, new Pattern[] {func_def_name});
            subpattern_heads = new Pattern[] {type};
        }

        protected override AstNode CreateAstNode(Location loc, List<AstNode> nodes) {
            return new AstNode.FuncDef(loc, nodes);
        }
    }

    // Workaround to allow multiple functions
    public class Program : NotToken {
        public Program() : base(null, "PROGRAM") {
            var func_def = new FuncDef(new Pattern[1]);
            func_def.tails[0] = func_def;
            subpattern_heads = new Pattern[] {func_def};
        }

        // This shouldn't be called
        protected override AstNode CreateAstNode(Location loc, List<AstNode> nodes) {
            throw new NotImplementedException();
        }
    }
    
}

class Output {
    static int indentation = 0;

    public static void WriteLine(string il_code) {
        // TODO allow the output file to be changed
        File.AppendAllText("output.il", new string(' ', indentation) + il_code + "\n");
    }
    public static void Indent() => indentation += 4;
    public static void Unindent() => indentation -= 4;
}

class Umi {

    // TODO: allow function overloading
    public static Dictionary<string, string[]> function_args = new Dictionary<string, string[]>();

    public static void Crash(string message, Location loc) {
        Console.WriteLine(loc + ": " + message);
        Environment.Exit(1);
    }

    static List<Token> Lex(string file) {
        Location position = new Location(1, 1);
        List<Token> tokens = new List<Token>();

        for (int i = 0; i < file.Length; i++) {
            char c = file[i];
            Token.Type? type = null;
            string value = null;
            Location loc = position.Copy();

            if (Char.IsWhiteSpace(c)) {
            } else if (Char.IsLetter(c) || c  == '_') {
                type = Token.Type.IDENTIFIER;
                string content = c.ToString();
                while (Char.IsLetterOrDigit(file[i + 1]) || file[i + 1] == '_') {
                    i++;
                    position.Increase(file[i]);
                    content += file[i];
                }
                value = content;
            } else if (Char.IsDigit(c)) {
                type = Token.Type.INTEGER;
                string content = c.ToString();
                while (Char.IsDigit(file[i + 1])) {
                    i++;
                    position.Increase(file[i]);
                    content += file[i];
                }
                value = content;
            } else switch (c) {
                case '(':
                    type = Token.Type.LPARAN;
                    break;
                case ')':
                    type = Token.Type.RPARAN;
                    break;
                case '{':
                    type = Token.Type.LCURLY;
                    break;
                case '}':
                    type = Token.Type.RCURLY;
                    break;
                case ';':
                    type = Token.Type.STATEMENT_END;
                    break;
                case '=':
                    type = Token.Type.EQUAL;
                    break;
                case ',':
                    type = Token.Type.COMMA;
                    break;
                case '"':
                    type = Token.Type.STRING;
                    string content = "";
                    while (file[i + 1] != '"') {
                        i++;
                        position.Increase(file[i]);
                        content += file[i];
                    }
                    i++;
                    position.Increase(file[i]);
                    value = content;
                    break;
                case '/':
                    if (file[i + 1] != '/') Crash("Division not implemented", loc);
                    while (file[i + 1] != '\n') {
                        i++;
                        position.Increase(file[i]);
                    }
                    break;
                default:
                    Umi.Crash($"Unknown character: {c}", loc);
                    break;
            }

            if (type != null) {
                tokens.Add(new Token(type.Value, loc, value));
            }
            position.Increase(c);
        }

        return tokens;
    }

    static void GenIlForProgram(List<AstNode> ast) {
        File.Delete("output.il");
        Output.WriteLine(".assembly UmiProgram {}\n");
        
        // TODO: use something like an alias instead
        Umi.function_args["print"] = new string[] {"string"};
        Output.WriteLine(".method static void print(string)");
        Output.WriteLine("{");
        Output.Indent();
        Output.WriteLine("ldarg 0");
        Output.WriteLine("call void [mscorlib]System.Console::WriteLine(string)");
        Output.WriteLine("ret");
        Output.Unindent();
        Output.WriteLine("}\n");

        // TODO: make an overload of print
        Umi.function_args["printn"] = new string[] {"int32"};
        Output.WriteLine(".method static void printn(int32)");
        Output.WriteLine("{");
        Output.Indent();
        Output.WriteLine("ldarg 0");
        Output.WriteLine("call void [mscorlib]System.Console::WriteLine(int32)");
        Output.WriteLine("ret");
        Output.Unindent();
        Output.WriteLine("}\n");

        foreach (var node in ast) node.GenIl(null);
    }

    static void Main(string[] args) {
        if (args.Length < 1) {
            Console.WriteLine("You must specify the path to the file");
            Environment.Exit(1);
        }

        List<Token> tokens = Lex(File.ReadAllText(args[0]));
        int i = 0;
        List<AstNode> ast = new Pattern.Program().ParseTokens(tokens, ref i);
        // TODO: better error message (currently this isn't even correct: it points to the start of the function)
        if (ast == null) Crash($"Unexpected token: {tokens[i].type}", tokens[i].location);
        GenIlForProgram(ast);
    }

}
