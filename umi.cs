using System;
using System.IO;
using System.Collections.Generic;
using System.Linq;

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
    
    public override string ToString() => $"{line}:{column}";

    public static bool operator >(Location a, Location b) {
        if (a.line != b.line) return a.line > b.line;
        return a.column > b.column;
    }

    public static bool operator <(Location a, Location b) {
        if (a.line != b.line) return a.line < b.line;
        return a.column < b.column;
    }

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
        NOTHING, // for error reporting
        IDENTIFIER,
        OPERATOR,
        STRING,
        INTEGER,
        LPARAN,
        RPARAN,
        LCURLY,
        RCURLY,
        NEWLINE,
        COMMA,
        EQUAL,
        IL
    }

}

abstract class Grammar {

    protected abstract AstNode GenAst();

    static List<Token> tokens;
    static int i = 0; // the cursor in tokens

    // For basic error reporting
    static Token furthest_got;
    static Token.Type furthest_expected;

    class Tok : Grammar {
        readonly Token.Type token_type;

        public Tok(Token.Type token_type) => this.token_type = token_type;

        protected override AstNode GenAst() {
            if (i == tokens.Count) {
                var loc = tokens[i - 1].location.Copy();
                loc.Increase(' '); // to move it to the non existent token
                furthest_got = new Token(Token.Type.NOTHING, loc, null);
                furthest_expected = token_type;
            } else if (tokens[i].type == token_type) {
                return new AstNode.Value(tokens[i++]);
            } else if (tokens[i].location > furthest_got.location) {
                furthest_got = tokens[i];
                furthest_expected = token_type;
            }
            return null;
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


    // Mandatory newlines (at least 1)
    static readonly Multiple<AstNode> MNL = new Multiple<AstNode>("MNL", new Grammar[] {
        new Tok(Token.Type.NEWLINE), null
    });
    // Optional newlines
    static readonly Pattern ONL = new Pattern("ONL", new Grammar[][] {
        new Grammar[] {MNL}, new Grammar[0]
    }, (loc, _) => new AstNode(loc));

    static Pattern NewStatements(string space, Grammar[] possibilities) {
        string name = space + "_STATEMENT";
        var statement = new Pattern(name, possibilities);
        var statements = new Multiple<AstNode>(name + "S", new Grammar[] {statement, MNL, null});
        return new Pattern(name + "S+NL", new Grammar[] {ONL, statements, ONL}, (_, nodes) => nodes[1]);
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
        var IL = new Tok(Token.Type.IL);

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

        var LOCAL_STATEMENTS = NewStatements("LOCAL", new Grammar[] {FUNC_CALL, VAR_DEF, VAR_ASSIGN});

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
            new Grammar[] {IDENTIFIER, IDENTIFIER, PARAMETERS, LCURLY, LOCAL_STATEMENTS, RCURLY}, 
            (loc, nodes) => new AstNode.FuncDef(loc, nodes)
        );

        var TYPE_LIST = new Multiple<AstNode.Value>("TYPE_LIST", new Grammar[] {IDENTIFIER, COMMA, null});
        // TODO: allow having no parameters
        var IL_FUNC = new Pattern("IL_FUNC", 
            new Grammar[] {IL, IDENTIFIER, IDENTIFIER, LPARAN, TYPE_LIST, RPARAN, STRING},
            (loc, nodes) => {
                string name = ((AstNode.Value)nodes[2]).token.value;
                string il = ((AstNode.Value)nodes[6]).token.value;
                var values = ((AstNode.Multiple<AstNode.Value>)nodes[4]).ToArray();
                string[] param_types = Array.ConvertAll(values, v => v.token.value);
                return new AstNode.IlFunc(loc, name, il, param_types);
            }
        );

        var GLOBAL_STATEMENTS = NewStatements("GLOBAL", new Grammar[] {FUNC_DEF, IL_FUNC});

        var PROGRAM = new Pattern("PROGRAM", new Grammar[] {GLOBAL_STATEMENTS}, null);
        return PROGRAM;
    }

    public static AstNode.Program ParseTokens(List<Token> toks) {
        tokens = toks;
        furthest_got = toks[0];
        List<AstNode> ast = CreateProgramGrammar().Parse();
        if (ast == null || i != tokens.Count) {
            Umi.Crash($"Expected `{furthest_expected}` but found `{furthest_got.type}`", furthest_got.location);
        }
        return new AstNode.Program(((AstNode.Multiple<AstNode>)ast[0]).ToArray());
    }

}

class AstNode {

    public readonly Location location;

    public AstNode(Location loc) {
        location = loc;
    }

    public virtual void CreateNameInfo(Scope scope) => throw new NotImplementedException();
    public virtual void GenIl(Scope scope) => throw new NotImplementedException();

    public class Value : AstNode {
        public readonly Token token;
        public Value(Token token) : base(token.location) => this.token = token;

        public override void GenIl(Scope scope) {
            switch (token.type) {
                case Token.Type.STRING:
                    Output.WriteLine($"ldstr \"{token.value}\"");
                    break;
                case Token.Type.INTEGER:
                    Output.WriteLine($"ldc.i4 {token.value}");
                    break;
                case Token.Type.IDENTIFIER:
                    Name.Var variable = (Name.Var)scope.LookUp(token.value, location);
                    if (variable.defined_at > location) {
                        Umi.Crash($"Can not use variable `{token.value}` before it is defined", location);
                    }
                    Output.WriteLine($"{(variable.is_param ? "ldarg" : "ldloc")} {variable.index}");
                    break;
                default:
                    Umi.Crash("No Value IL generation for " + token.type, token.location);
                    break;
            }
        }

        string Type(Scope scope) {
            switch (token.type) {
                case Token.Type.STRING:
                    return "string";
                case Token.Type.INTEGER:
                    return "int32";
                case Token.Type.IDENTIFIER:
                    Name.Var variable = (Name.Var)scope.LookUp(token.value, location);
                    return variable.type;
                default:
                    Umi.Crash("No Value type for " + token.type, token.location);
                    return "unreachable";
            }
        }

        public void ExpectType(string expected, Scope scope) {
            string actual_type = Type(scope);
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

        public override void GenIl(Scope scope) {
            Name.Func func = (Name.Func)scope.LookUp(name, location);
            string[] expected_types = func.param_types;
            if (expected_types.Length != arguments.Length) {
                Umi.Crash("Incorrect number of arguments", location);
            }
            for (int i = 0; i < arguments.Length; i++) {
                Value arg = arguments[i];
                arg.ExpectType(expected_types[i], scope);
                arg.GenIl(scope);
            }
            func.GenIl(name);
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

        public override void GenIl(Scope scope) {
            Name.Var variable = (Name.Var)scope.LookUp(name, location);
            if (variable.defined_at > location) {
                Umi.Crash($"Can not assign to variable `{name}` before it is defined", location);
            }
            value.ExpectType(variable.type, scope);
            value.GenIl(scope);
            Output.WriteLine($"stloc {variable.index}");
        }
    }
    
    public class VarDef : AstNode {
        public readonly string type;
        public readonly VarAssign assignment;

        public VarDef(Location loc, string type, VarAssign assignment) : base(loc) {
            this.type = type;
            this.assignment = assignment;
        }

        public override void GenIl(Scope scope) => assignment.GenIl(scope);
    }

    public class FuncDef : AstNode {
        public readonly string name;
        readonly string return_type;
        readonly AstNode[] statements;
        readonly Param[] parameters;

        public FuncDef(Location loc, List<AstNode> nodes) : base(loc) {
            return_type = ((Value)nodes[0]).token.value;
            name = ((Value)nodes[1]).token.value;
            parameters = ((Parameters)nodes[2]).parameters;
            statements = ((Multiple<AstNode>)nodes[4]).ToArray();
        }

        public override void CreateNameInfo(Scope scope) {
            var func = new Name.OrdFunc(location, Array.ConvertAll(parameters, p => p.type), scope);
            scope.Add(name, func);
            
            for (int i = 0; i < parameters.Length; i++) {
                Param p = parameters[i];
                var par = new Name.Var(p.type, p.name, true, i, p.location);
                func.local_variables.Add(p.name, par);
            }

            var vardefs = Array.FindAll(statements, s => s is AstNode.VarDef);
            for (int i = 0; i < vardefs.Length; i++) {
                var vardef = (AstNode.VarDef)vardefs[i];
                var name = vardef.assignment.name;
                var variable = new Name.Var(vardef.type, name, false, i, vardef.location);
                func.local_variables.Add(name, variable);
            }
        }

        public override void GenIl(Scope scope) {
            string param = String.Join(", ", Array.ConvertAll(parameters, p => p.type));
            Output.WriteLine($".method static {return_type} {name}({param})");
            Output.WriteLine("{");
            Output.Indent();
            if (name == "main") Output.WriteLine(".entrypoint");
            
            Name.OrdFunc func = (Name.OrdFunc)scope.LookUp(name, location);
            var all_scope_vars = func.local_variables.names.Values.Cast<Name.Var>().ToArray();
            var local_vars = Array.FindAll(all_scope_vars, v => !v.is_param);
            if (local_vars.Length > 0) {
                Array.Sort(local_vars, (x, y) => x.index.CompareTo(y.index));
                Output.WriteLine(".locals init (");
                Output.Indent();
                for (int i = 0; i < local_vars.Length - 1; i++) {
                    Name.Var variable = local_vars[i];
                    Output.WriteLine($"{variable.type} {variable.name},");
                }
                Name.Var last_local = local_vars[local_vars.Length - 1];
                Output.WriteLine($"{last_local.type} {last_local.name}");
                Output.Unindent();
                Output.WriteLine(")");
            }
            
            foreach (var statement in statements) statement.GenIl(func.local_variables);

            Output.WriteLine("ret");
            Output.Unindent();
            Output.WriteLine("}\n");
        }
    }

    public class IlFunc : AstNode {
        // TODO: return type
        readonly string name;
        readonly string il;
        readonly string[] param_types;

        public IlFunc(Location loc, string name, string il, string[] param_types) : base(loc) {
            this.name = name;
            this.il = il;
            this.param_types = param_types;
        }

        public override void CreateNameInfo(Scope scope) {
            scope.Add(name, new Name.IlFunc(location, param_types, il));
        }

        public override void GenIl(Scope _) {}
    }

    public class Program : AstNode {
        AstNode[] global_statements;

        public Program(AstNode[] s) : base(s[0].location) => global_statements = s;

        public override void CreateNameInfo(Scope scope) {
            foreach(AstNode statement in global_statements) statement.CreateNameInfo(scope);
        }

        public override void GenIl(Scope scope) {
            File.Delete("output.il");
            Output.WriteLine(".assembly UmiProgram {}\n");
            foreach(AstNode statement in global_statements) statement.GenIl(scope);
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

    public readonly Location defined_at;

    Name(Location defined_at) => this.defined_at = defined_at;

    public abstract class Func : Name {
        // TODO: allow function overloading
        public readonly string[] param_types;
        public Func(Location defined_at, string[] param_types) : base(defined_at) {
            this.param_types = param_types;
        }
        public abstract void GenIl(string name);
    }

    // Ordinary function (as opposed to IL function)
    public class OrdFunc : Func {
        public readonly Scope local_variables;
        public OrdFunc(Location loc, string[] types, Scope parent) : base(loc, types) {
            local_variables = new Scope(parent);
        }
        public override void GenIl(string name) {
            // TODO: use real type
            Output.WriteLine($"call void {name}({String.Join(", ", param_types)})");
        }
    }

    public class IlFunc : Func {
        string il;
        public IlFunc(Location loc, string[] types, string il) : base(loc, types) => this.il = il;
        public override void GenIl(string _) => Output.WriteLine(il);
    }

    public class Var : Name {
        public readonly string type;
        public readonly string name;
        public readonly bool is_param; // local variable or parameter
        public readonly int index;
        public Var(string type, string name, bool is_param, int index, Location defined_at) : base(defined_at) {
            this.type = type;
            this.name = name;
            this.is_param = is_param;
            this.index = index;
        }
    }

}

class Scope {
    public readonly Dictionary<string, Name> names = new Dictionary<string, Name>();
    public readonly Scope parent;

    public Scope(Scope parent) => this.parent = parent;

    public Name LookUp(string name, Location loc) {
        Name result;
        if (names.TryGetValue(name, out result)) return result;
        if (parent == null) Umi.Crash($"`{name}` is not defined in the current scope", loc);
        return parent.LookUp(name, loc);
    } 

    public void Add(string name, Name value) {
        if (!names.TryAdd(name, value)) {
            Umi.Crash($"`{name}` already defined at {names[name].defined_at}", value.defined_at);
        }
    }

}

class Umi {
    
    public static void Crash(string message, Location loc) {
        Console.WriteLine(loc + ": " + message);
        Environment.Exit(1);
    }

    delegate bool ContinueConsuming(char next);

    static string ConsumeWhile(string file, ref int i, Location position, ContinueConsuming continueConsuming) {
        string content = "";
        while (continueConsuming(file[i + 1])) {
            i++;
            position.Increase(file[i]);
            content += file[i];
        }
        return content;
    }

    static string ConsumeWhile(char start, string f, ref int i, Location p, ContinueConsuming cc) {
        return start.ToString() + ConsumeWhile(f, ref i, p, cc);
    }

    static bool IsOperatorChar(char c) => "+-*/".Contains(c);
    
    static List<Token> Lex(string file) {
        Location position = new Location(1, 1);
        List<Token> tokens = new List<Token>();

        for (int i = 0; i < file.Length; i++) {
            char c = file[i];
            Token.Type? type = null;
            string value = null;
            Location loc = position.Copy();

            if (Char.IsWhiteSpace(c)) {
                if (c == '\n') type = Token.Type.NEWLINE;
            } else if (Char.IsLetter(c) || c  == '_') {
                string content = ConsumeWhile(c, file, ref i, position, ch => Char.IsLetterOrDigit(ch) || ch == '_');
                if (content == "il") {
                    type = Token.Type.IL;
                } else {
                    type = Token.Type.IDENTIFIER;
                    value = content;
                }
            } else if (Char.IsDigit(c)) {
                type = Token.Type.INTEGER;
                value = ConsumeWhile(c, file, ref i, position, ch => Char.IsDigit(ch));
            } else if (IsOperatorChar(c)) {
                type = Token.Type.OPERATOR;
                value = ConsumeWhile(c, file, ref i, position, IsOperatorChar);
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
                case '=':
                    type = Token.Type.EQUAL;
                    break;
                case ',':
                    type = Token.Type.COMMA;
                    break;
                case '"':
                    type = Token.Type.STRING;
                    value = ConsumeWhile(file, ref i, position, ch => ch != '"');
                    i++;
                    position.Increase(file[i]);
                    break;
                case '#':
                    ConsumeWhile(file, ref i, position, ch => ch != '\n');
                    break;
                default:
                    Umi.Crash($"Unknown character: {c}", loc);
                    break;
            }

            if (type != null) tokens.Add(new Token(type.Value, loc, value));
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
        Scope global_namespace = new Scope(null);
        ast.CreateNameInfo(global_namespace);
        ast.GenIl(global_namespace);
    }

}
