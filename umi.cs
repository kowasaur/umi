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

abstract class Grammar {

    protected abstract AstNode GenAst();

    static List<Token> tokens;
    static int i = 0; // the cursor in tokens

    class Tok : Grammar {
        readonly Token.Type token_type;

        public Tok(Token.Type token_type) => this.token_type = token_type;

        protected override AstNode GenAst() {
            if (i == tokens.Count || tokens[i].type != token_type) return null;
            return new AstNode.Value(tokens[i++]);
        }

        public static implicit operator string(Tok t) => t.token_type.ToString();
    }

    class Pattern : Grammar {
        public readonly Grammar[][] possible_patterns;
        public delegate AstNode NodeCreate(Location loc, List<AstNode> nodes);
        NodeCreate CreateAstNode;
        readonly string name;

        public Pattern(string name, Grammar[][] posible_patterns, NodeCreate CreateAstNode) {
            this.name = name;
            this.possible_patterns = posible_patterns;
            this.CreateAstNode = CreateAstNode;
        }
        public Pattern(string name, Grammar[] pattern, NodeCreate CreateAstNode) {
            this.name = name;
            this.possible_patterns = new Grammar[][] {pattern};
            this.CreateAstNode = CreateAstNode;
        }

        // When the pattern is only 1 thing long but there's different possibilites
        public Pattern(string name, Grammar[] options) {
            this.name = name;
            possible_patterns = new Grammar[options.Length][];
            for (int i = 0; i < options.Length; i++) {
                possible_patterns[i] = new Grammar[] {options[i]}; 
            }
            this.CreateAstNode = (_, nodes) => nodes[0];
        }

        public static implicit operator string(Pattern p) => p.name;

        protected override AstNode GenAst() {
            if (i == tokens.Count) return null;
            Location loc = tokens[i].location;
            List<AstNode> nodes = Parse();
            if (nodes == null) return null;
            return CreateAstNode(loc, nodes);
        }

        public List<AstNode> Parse() {
            List<AstNode> nodes = new List<AstNode>();

            foreach (Grammar[] pattern in possible_patterns) {
                bool matched = true;
                int old_i = i;
                foreach (Grammar grammar in pattern) {
                    var ast_node = grammar.GenAst();
                    if (ast_node == null) {
                        matched = false;
                        break;
                    }
                    nodes.Add(ast_node);
                }
                if (matched) return nodes;
                i = old_i;
                nodes = new List<AstNode>();
            }

            return null;
        }
    }

    class Multiple<T> : Pattern where T : AstNode {
        // grammar must end with null
        public Multiple(string name, Grammar[] grammar) : base(name, 
            new Grammar[][] {grammar, new Grammar[] {grammar[0]}},
            (_, nodes) => {
                var new_value = (T)nodes[0];
                if (nodes.Count == 1) return new AstNode.Multiple<T>(new_value);
                var values = (AstNode.Multiple<T>)nodes[nodes.Count - 1];
                values.list.Add(new_value);
                return values;
            }
        ) {
            possible_patterns[0][grammar.Length - 1] = this;
        }
    }

    static Pattern CreateProgramGrammar() {
        var LPARAN = new Tok(Token.Type.LPARAN);
        var RPARAN = new Tok(Token.Type.RPARAN);
        var LCURLY = new Tok(Token.Type.LCURLY);
        var RCURLY = new Tok(Token.Type.RCURLY);
        var COMMA = new Tok(Token.Type.COMMA);
        var IDENTIFIER = new Tok(Token.Type.IDENTIFIER);
        var STRING = new Tok(Token.Type.STRING);
        var INTEGER = new Tok(Token.Type.INTEGER);
        var EQUAL = new Tok(Token.Type.EQUAL);
        var SEMICOLON = new Tok(Token.Type.STATEMENT_END);

        var VALUE = new Pattern("VALUE", new Grammar[] {STRING, INTEGER, IDENTIFIER});

        var ARG_LIST = new Multiple<AstNode.Value>("ARG_LIST", new Grammar[] {VALUE, COMMA, null});
        var ARGUMENTS = new Pattern("ARGUMENTS", new Grammar[][] {
            new Grammar[] {LPARAN, RPARAN},
            new Grammar[] {LPARAN, ARG_LIST, RPARAN}
        }, (loc, nodes) => {
            if (nodes.Count == 2) return new AstNode.Arguments(loc, new AstNode.Value[0]);
            var values = ((AstNode.Multiple<AstNode.Value>)nodes[1]).ToArray();
            return new AstNode.Arguments(loc, values);
        });

        var FUNC_CALL = new Pattern("FUNC_CALL", new Grammar[] {IDENTIFIER, ARGUMENTS}, (loc, nodes) => {
            string name = ((AstNode.Value)nodes[0]).token.value;
            return new AstNode.FuncCall(loc, name, (AstNode.Arguments)nodes[1]);
        });

        var VAR_ASSIGN = new Pattern("VAR_ASSIGN", new Grammar[] {IDENTIFIER, EQUAL, VALUE}, (loc, nodes) => {
            string name = ((AstNode.Value)nodes[0]).token.value;
            return new AstNode.VarAssign(loc, name, (AstNode.Value)nodes[2]);
        });

        var VAR_DEF = new Pattern("VAR_DEF", new Grammar[] {IDENTIFIER, VAR_ASSIGN}, (loc, nodes) => {
            string type = ((AstNode.Value)nodes[0]).token.value;
            return new AstNode.VarDef(loc, type, (AstNode.VarAssign)nodes[1]);
        });

        var STATEMENT = new Pattern("STATEMENT", new Grammar[][] {
            new Grammar[] {FUNC_CALL, SEMICOLON},
            new Grammar[] {VAR_DEF, SEMICOLON},
            new Grammar[] {VAR_ASSIGN, SEMICOLON}
        }, (_, nodes) => nodes[0]);

        var STATEMENTS = new Multiple<AstNode>("STATEMENTS", new Grammar[] {STATEMENT, null}); 

        var PARAM = new Pattern("PARAM", new Grammar[] {IDENTIFIER, IDENTIFIER}, 
            (loc, nodes) => new AstNode.Param(loc, nodes)
        );
        var PARAM_LIST = new Multiple<AstNode.Param>("PARAM_LIST", new Grammar[] {PARAM, COMMA, null});
        var PARAMETERS = new Pattern("PARAMETERS", new Grammar[][] {
            new Grammar[] {LPARAN, RPARAN},
            new Grammar[] {LPARAN, PARAM_LIST, RPARAN}
        }, (loc, nodes) => {
            if (nodes.Count == 2) return new AstNode.Parameters(loc, new AstNode.Param[0]);
            var param_list = ((AstNode.Multiple<AstNode.Param>)nodes[1]).ToArray();
            return new AstNode.Parameters(loc, param_list);
        });

        var FUNC_DEF = new Pattern("FUNC_DEF", 
            new Grammar[] {IDENTIFIER, IDENTIFIER, PARAMETERS, LCURLY, STATEMENTS, RCURLY}, 
            (loc, nodes) => new AstNode.FuncDef(loc, nodes)
        );

        var FUNC_DEFS = new Multiple<AstNode.FuncDef>("FUNC_DEFS", new Grammar[] {FUNC_DEF, null});
        var PROGRAM = new Pattern("PROGRAM", new Grammar[] {FUNC_DEFS}, null);
        return PROGRAM;
    }

    public static AstNode.Program ParseTokens(List<Token> toks) {
        tokens = toks;
        List<AstNode> ast = CreateProgramGrammar().Parse();
        if (ast == null) return null;
        return new AstNode.Program(((AstNode.Multiple<AstNode.FuncDef>)ast[0]).ToArray());
    }

}

abstract class AstNode {

    public readonly Location location;

    public AstNode(Location loc) {
        location = loc;
    }

    public virtual Name CreateNameInfo() => throw new NotImplementedException();
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
                    if (variable.is_param) {
                        Output.WriteLine($"ldarg {variable.index}");
                    } else {
                        if (variable.location > location) {
                            Umi.Crash($"Can not use variable `{token.value}` before it is defined", location);
                        }
                        Output.WriteLine($"ldloc {variable.index}");
                    }
                    break;
                default:
                    Umi.Crash("No Value IL generation for " + token.type, token.location);
                    break;
            }
        }

        string Type(Dictionary<string, VarDef> local_vars) {
            switch (token.type) {
                case Token.Type.STRING:
                    return "string";
                case Token.Type.INTEGER:
                    return "int32";
                case Token.Type.IDENTIFIER:
                    VarDef variable;
                    if (!local_vars.TryGetValue(token.value, out variable)) {
                        Umi.Crash($"Variable `{token.value}` is not defined", location);
                    }
                    return variable.type;
                default:
                    Umi.Crash("No Value type for " + token.type, token.location);
                    return "unreachable";
            }
        }

        public void ExpectType(string expected, Dictionary<string, VarDef> local_vars) {
            string actual_type = Type(local_vars);
            if (actual_type != expected) {
                Umi.Crash($"Incorrect type. Expected {expected} but found {actual_type}", location);
            }
        }
    }

    public class Multiple<T> : AstNode where T : AstNode {
        public readonly List<T> list = new List<T>();
        public Multiple(T thing): base(thing.location) => list.Add(thing);
        // This should only be called once since it mutates list
        public T[] ToArray() {
            list.Reverse();
            return list.ToArray();
        }
    }

    public class Arguments : AstNode {
        public readonly Value[] args;
        public Arguments(Location loc, Value[] args) : base(loc) => this.args = args;
    }

    public class Param : AstNode {
        public readonly string type;
        public readonly string name;
        public Param(Location loc, List<AstNode> nodes) : base(loc) {
            this.type = ((AstNode.Value)nodes[0]).token.value;
            this.name = ((AstNode.Value)nodes[1]).token.value;
        }
    }

    public class Parameters : AstNode {
        public readonly Param[] parameters;
        public Parameters(Location loc, Param[] paras) : base(loc) => parameters = paras;
    }

    public class FuncCall : AstNode {
        readonly string name;
        readonly Value[] arguments;

        public FuncCall(Location loc, string name, Arguments args) : base(loc) {
            this.name = name;
            arguments = args.args;
        }

        public override void GenIl(Dictionary<string, VarDef> local_vars) {
            string[] expected_types = ((Name.FuncDef)Umi.global_namespace[name]).param_types;
            if (expected_types.Length != arguments.Length) {
                Umi.Crash("Incorrect number of arguments", location);
            }
            for (int i = 0; i < arguments.Length; i++) {
                Value arg = arguments[i];
                arg.ExpectType(expected_types[i], local_vars);
                arg.GenIl(local_vars);
            }
            string args = String.Join(", ", expected_types);
            // TODO: use real type
            Output.WriteLine($"call void {name} ({args})");
        }
    }
    
    // TODO: prevent assigning to parameters or make it work properly
    public class VarAssign : AstNode {
        public readonly string name;
        readonly Value value;

        public VarAssign(Location loc, string name, Value value) : base(loc) {
            this.name = name;
            this.value = value;
        }

        public override void GenIl(Dictionary<string, VarDef> local_vars) {
            VarDef variable;
            if (!local_vars.TryGetValue(name, out variable)) {
                Umi.Crash($"Variable `{name}` is not defined", location);
            }
            if (variable.location > location) {
                Umi.Crash($"Can not assign to variable `{name}` before it is defined", location);
            }
            value.ExpectType(variable.type, local_vars);
            value.GenIl(local_vars);
            Output.WriteLine($"stloc {variable.index}");
        }
    }
    
    // Probably shouldn't be an AstNode
    public class VarDef : AstNode {
        public readonly VarAssign assignment;
        public readonly string name;
        public readonly string type;
        public readonly bool is_param = false; // local variable or parameter
        public int index; // in the function

        public VarDef(Location loc, string type, VarAssign assignment) : base(loc) {
            this.name = assignment.name;
            this.type = type;
            this.assignment = assignment;
        }

        public VarDef(Location loc, string type, string name, int index) : base(loc) {
            this.name = name;
            this.type = type;
            this.index = index;
            this.is_param = true;
        }
    }

    public class FuncDef : AstNode {
        public readonly string name;
        readonly string return_type;
        readonly AstNode[] statements;
        readonly Param[] parameters;
        readonly Dictionary<string, VarDef> local_vars = new Dictionary<string, VarDef>();

        public FuncDef(Location loc, List<AstNode> nodes) : base(loc) {
            return_type = ((Value)nodes[0]).token.value;
            name = ((Value)nodes[1]).token.value;

            parameters = ((Parameters)nodes[2]).parameters;
            for (int i = 0; i < parameters.Length; i++) {
                string param_type = parameters[i].type;
                string param_name = parameters[i].name;
                VarDef variable = new VarDef(parameters[i].location, param_type, param_name, i);
                if (!local_vars.TryAdd(param_name, variable)) {
                    Umi.Crash($"Parameter with name `{param_name}` already exists", parameters[i].location);
                };
            }

            statements = ((Multiple<AstNode>)nodes[4]).ToArray();
            for (int i = 0; i < statements.Length; i++) {
                if (statements[i] is VarDef) {
                    VarDef variable = (VarDef)statements[i];
                    if (!local_vars.TryAdd(variable.name, variable)) {
                        Umi.Crash($"Local variable `{variable.name}` already defined", variable.location);
                    };
                    statements[i] = variable.assignment;
                }
            }
        }

        public override Name CreateNameInfo() => new Name.FuncDef(Array.ConvertAll(parameters, p => p.type));

        public override void GenIl(Dictionary<string, VarDef> _) {
            string param = String.Join(", ", Array.ConvertAll(parameters, p => p.type));
            Output.WriteLine($".method static {return_type} {name}({param})");
            Output.WriteLine("{");
            Output.Indent();
            if (name == "main") Output.WriteLine(".entrypoint");
            
            Output.WriteLine(".locals init (");
            Output.Indent();
            int local_count = 0;
            foreach (VarDef variable in local_vars.Values) if (!variable.is_param) {
                variable.index = local_count++;
                if (local_count == local_vars.Count - parameters.Length) {
                    Output.WriteLine($"{variable.type} {variable.name}");
                } else {
                    Output.WriteLine($"{variable.type} {variable.name},");
                }   
            }
            Output.Unindent();
            Output.WriteLine(")");
            
            foreach (var statement in statements) statement.GenIl(local_vars);

            Output.WriteLine("ret");
            Output.Unindent();
            Output.WriteLine("}\n");
        }
    }

    public class Program : AstNode {
        FuncDef[] func_defs;

        public Program(FuncDef[] func_defs) : base(func_defs[0].location) => this.func_defs = func_defs;

        public override Name CreateNameInfo() {
            foreach(FuncDef func_def in func_defs) {
                if (!Umi.global_namespace.TryAdd(func_def.name, func_def.CreateNameInfo())) {
                    Umi.Crash($"Function `{func_def.name}` already defined", func_def.location);
                }
            }
            return null;
        }

        public override void GenIl(Dictionary<string, VarDef> _) {
            File.Delete("output.il");
            Output.WriteLine(".assembly UmiProgram {}\n");
            
            // TODO: use something like an alias instead
            Umi.global_namespace["print"] = new Name.FuncDef(new string[] {"string"});
            Output.WriteLine(".method static void print(string)");
            Output.WriteLine("{");
            Output.Indent();
            Output.WriteLine("ldarg 0");
            Output.WriteLine("call void [mscorlib]System.Console::WriteLine(string)");
            Output.WriteLine("ret");
            Output.Unindent();
            Output.WriteLine("}\n");

            // TODO: make an overload of print
            Umi.global_namespace["printn"] = new Name.FuncDef(new string[] {"int32"});
            Output.WriteLine(".method static void printn(int32)");
            Output.WriteLine("{");
            Output.Indent();
            Output.WriteLine("ldarg 0");
            Output.WriteLine("call void [mscorlib]System.Console::WriteLine(int32)");
            Output.WriteLine("ret");
            Output.Unindent();
            Output.WriteLine("}\n");

            foreach(FuncDef func_def in func_defs) func_def.GenIl(null);
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

abstract class Name {
    public class FuncDef : Name {
        // TODO: allow function overloading
        public string[] param_types;
        public FuncDef(string[] arg_types) => this.param_types = arg_types;
    }
}

class Umi {
    
    public static readonly Dictionary<string, Name> global_namespace = new Dictionary<string, Name>();

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

    static void Main(string[] args) {
        if (args.Length < 1) {
            Console.WriteLine("You must specify the path to the file");
            Environment.Exit(1);
        }

        List<Token> tokens = Lex(File.ReadAllText(args[0]));
        AstNode.Program ast = Grammar.ParseTokens(tokens);
        ast.CreateNameInfo();
        ast.GenIl(null);

        // TODO: better error messages
    }

}
