using System;
using System.IO;
using System.Collections.Generic;
using System.Linq;
using System.Diagnostics;

class Location {

    readonly int index; // index in the tokens list
    public readonly int line;
    public readonly int column;
    // ? Maybe a reference to the Lexer should be stored instead
    public readonly string file_path;

    public Location(int line, int column, string file_path, int index) {
        this.line = line;
        this.column = column;
        this.file_path = file_path;
        this.index = index;
    }
    
    public override string ToString() => $"{file_path}({line},{column})";
    public string Label() => $"L{line}C{column}";

    public static bool operator >(Location a, Location b) => a.index > b.index;
    public static bool operator <(Location a, Location b) => a.index < b.index;

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
        BOOLEAN,
        LPARAN,
        RPARAN,
        LCURLY,
        RCURLY,
        NEWLINE,
        COMMA,
        EQUAL,
        IL,
        IF,
        ELSE,
        WHILE
    }

}

class Lexer {
    
    readonly string file;
    readonly string file_path;

    int i = 0;
    int line = 1;
    int column = 1;

    delegate bool ContinueConsuming(char next);

    public Lexer(string file_path) {
        try {
            file = File.ReadAllText(file_path);
        }
        catch (FileNotFoundException) {
            Console.WriteLine(file_path + " does not exist");
            Environment.Exit(1);
        }
        this.file_path = file_path;
    }
    
    void Increment() {
        if (file[i] == '\n') {
            column = 1;
            line++;
        } else column++;
        i++;
    }

    string ConsumeWhile(ContinueConsuming continueConsuming) {
        string content = "";
        while (continueConsuming(file[i + 1])) {
            Increment();
            content += file[i];
        }
        return content;
    }

    string ConsumeWhile(char start, ContinueConsuming cc) => start.ToString() + ConsumeWhile(cc);
    
    static bool IsOperatorChar(char c) => "+-*/%&|=><!".Contains(c);
    
    public List<Token> Lex(List<Token> tokens) {
        while (i < file.Length) {
            char c = file[i];
            Token.Type type = Token.Type.NOTHING;
            string value = null;
            Location loc = new Location(line, column, file_path, tokens.Count);

            if (Char.IsWhiteSpace(c)) {
                if (c == '\n') type = Token.Type.NEWLINE;
            } else if (Char.IsLetter(c) || c  == '_') {
                string content = ConsumeWhile(c, ch => Char.IsLetterOrDigit(ch) || ch == '_');
                switch (content) {
                    case "il":
                        type = Token.Type.IL;
                        break;
                    case "true":
                        type = Token.Type.BOOLEAN;
                        value = "1";
                        break;
                    case "false":
                        type = Token.Type.BOOLEAN;
                        value = "0";
                        break;
                    case "if":
                        type = Token.Type.IF;
                        break;
                    case "else":
                        type = Token.Type.ELSE;
                        break;
                    case "while":
                        type = Token.Type.WHILE;
                        break;
                    default:
                        type = Token.Type.IDENTIFIER;
                        value = content;
                        break;
                }
            } else if (Char.IsDigit(c)) {
                type = Token.Type.INTEGER;
                value = ConsumeWhile(c, Char.IsDigit);
            } else if (IsOperatorChar(c)) {
                string content = ConsumeWhile(c, IsOperatorChar);
                if (content == "=") {
                    type = Token.Type.EQUAL;
                } else {
                    type = Token.Type.OPERATOR;
                    value = content;
                }
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
                case ',':
                    type = Token.Type.COMMA;
                    break;
                case '"':
                    type = Token.Type.STRING;
                    value = ConsumeWhile(ch => ch != '"');
                    Increment();
                    break;
                case '#':
                    ConsumeWhile(ch => ch != '\n');
                    break;
                default:
                    Umi.Crash($"Unknown character: {c}", loc);
                    break;
            }

            if (type != Token.Type.NOTHING) tokens.Add(new Token(type, loc, value));
            Increment();
        }

        return tokens;
    }

}

abstract class Grammar {

    readonly bool optional;
    protected abstract AstNode GenAst();
    public Grammar(bool optional) => this.optional = optional; 

    static List<Token> tokens;
    static int i = 0; // the cursor in tokens

    // For basic error reporting
    static Token furthest_got;
    static Token.Type furthest_expected;

    class Tok : Grammar {
        readonly Token.Type token_type;

        public Tok(Token.Type token_type, bool optional = false): base(optional) => this.token_type = token_type;

        protected override AstNode GenAst() {
            if (i == tokens.Count) {
                var last_loc = tokens[i - 1].location;
                var loc = new Location(last_loc.line, last_loc.column + 1, last_loc.file_path, 999999999);
                furthest_got = new Token(Token.Type.NOTHING, loc, null);
                furthest_expected = token_type;
            } else if (tokens[i].type == token_type) {
                return new AstNode.Tok(tokens[i++]);
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

        public Pattern(string name, Grammar[][] posible_patterns, NodeCreate CreateAstNode, bool optional = false): base(optional) {
            this.name = name;
            this.possible_patterns = posible_patterns;
            this.CreateAstNode = CreateAstNode;
        }
        public Pattern(string name, Grammar[] pattern, NodeCreate CreateAstNode, bool optional = false): base(optional) {
            this.name = name;
            this.possible_patterns = new Grammar[][] {pattern};
            this.CreateAstNode = CreateAstNode;
        }

        public static implicit operator string(Pattern p) => p.name;
        public Pattern NewOptional(string name) => new Pattern(name, possible_patterns, CreateAstNode, true);

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
                    if (ast_node == null && !grammar.optional) {
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

    // When the pattern is only 1 thing long but there's different possibilites
    static Pattern OneOf(string name, Grammar[] options) {
        var pat = new Pattern(name, new Grammar[options.Length][], (_, nodes) => nodes[0]);
        for (int i = 0; i < options.Length; i++) {
            pat.possible_patterns[i] = new Grammar[] {options[i]}; 
        }
        return pat;
    }

    static Pattern Multiple<T>(string name, Grammar[] grammar, bool optional = false) where T : AstNode {
        var g = new Grammar[grammar.Length + 1]; // last is null
        grammar.CopyTo(g, 0);
        var pat = new Pattern(name, new Grammar[][] {g, new Grammar[] {g[0]}},
            (_, nodes) => {
                var new_value = (T)nodes[0];
                if (nodes.Count == 1) return new AstNode.Multiple<T>(new_value);
                var values = (AstNode.Multiple<T>)nodes[nodes.Count - 1];
                values.list.Add(new_value);
                return values;
            }, optional
        );
        g[g.Length - 1] = pat;
        return pat;
    }

    // Mandatory newlines (at least 1)
    static readonly Pattern MNL = Multiple<AstNode>("MNL", new Grammar[] {
        new Tok(Token.Type.NEWLINE)
    });
    // Optional newlines
    static readonly Pattern ONL = MNL.NewOptional("ONL");

    static Pattern NewStatements(string space, Grammar[] possibilities) {
        string name = space + "_STATEMENT";
        var statement = OneOf(name, possibilities);
        var statements = Multiple<AstNode>(name + "S", new Grammar[] {statement, MNL});
        return new Pattern(name + "S+NL", new Grammar[] {ONL, statements, ONL}, (_, nodes) => nodes[1]);
    }

    static AstNode[] MultiArray(List<AstNode> nodes, int i) => ((AstNode.Multiple<AstNode>)nodes[i]).ToArray();
    
    static readonly string[][] OPERATOR_PRECEDENCE = {
        new string[] {"**"},
        new string[] {"*", "/", "%"},
        new string[] {"+", "-"},
        new string[] {"<<", ">>", "&", "|", "^"},
        new string[] {"==", "!=", ">", "<", ">=", "<="},
        new string[] {"&&", "||"}
    };

    static Pattern CreateProgramGrammar() {
        var LPARAN = new Tok(Token.Type.LPARAN);
        var RPARAN = new Tok(Token.Type.RPARAN);
        var LCURLY = new Tok(Token.Type.LCURLY);
        var RCURLY = new Tok(Token.Type.RCURLY);
        var COMMA = new Tok(Token.Type.COMMA);
        var EQUAL = new Tok(Token.Type.EQUAL);
        var IL = new Tok(Token.Type.IL);
        var IF = new Tok(Token.Type.IF);
        var ELSE = new Tok(Token.Type.ELSE);

        var STRING = new Pattern("STRING", new Grammar[] {new Tok(Token.Type.STRING)}, 
            (_, nodes) => new AstNode.StringLiteral((AstNode.Tok)nodes[0])
        );
        var INTEGER = new Pattern("INTEGER", new Grammar[] {new Tok(Token.Type.INTEGER)}, 
            (_, nodes) => new AstNode.IntegerLiteral((AstNode.Tok)nodes[0])
        );
        var BOOLEAN = new Pattern("BOOLEAN", new Grammar[] {new Tok(Token.Type.BOOLEAN)},
            (_, nodes) => new AstNode.BooleanLiteral((AstNode.Tok)nodes[0])
        );
        var IDENTIFIER = new Pattern("IDENTIFIER", new Grammar[] {new Tok(Token.Type.IDENTIFIER)}, 
            (_, nodes) => new AstNode.Identifier((AstNode.Tok)nodes[0])
        );
        var OPERATOR = new Pattern("OPERATOR", new Grammar[] {new Tok(Token.Type.OPERATOR)},
            (_, nodes) => new AstNode.Identifier((AstNode.Tok)nodes[0])
        );

        var SUBEXPRESSION = new Pattern ("SUBEXPRESSION", new Grammar[] {LPARAN, null, RPARAN}, 
            (_, nodes) => nodes[1]
        );

        // A single term in an expresion
        var TERM = OneOf("TERM", new Grammar[] {STRING, INTEGER, BOOLEAN, null, IDENTIFIER, SUBEXPRESSION});

        var POSTFIX = new Pattern("POSTFIX", new Grammar[] {TERM, OPERATOR}, (loc, nodes) => {
            string name = ((AstNode.Identifier)nodes[1]).content;
            return new AstNode.FuncCall(loc, name, new AstNode[] {nodes[0], new AstNode.Nothing()});
        });
        var PREFIX = new Pattern("PREFIX", new Grammar[] {OPERATOR, TERM}, (loc, nodes) => {
            string name = ((AstNode.Identifier)nodes[0]).content;
            return new AstNode.FuncCall(loc, name, new AstNode[] {new AstNode.Nothing(), nodes[1]});
        });
        var UNARY = OneOf("UNARY", new Grammar[] {POSTFIX, PREFIX});

        // Expression Part
        var EXP_PART = new Pattern("EXP_PART", new Grammar[][] {
            new Grammar[] {UNARY, OPERATOR, null},
            new Grammar[] {TERM, OPERATOR, null},
            new Grammar[] {UNARY},
            new Grammar[] {TERM}
        }, (_, nodes) => {
            if (nodes.Count == 1) return new AstNode.Multiple<AstNode>(nodes[0]);
            var values = (AstNode.Multiple<AstNode>)nodes[nodes.Count - 1];
            values.list.Add(nodes[1]);
            values.list.Add(nodes[0]);
            return values;
        });
        EXP_PART.possible_patterns[0][2] = EXP_PART;
        EXP_PART.possible_patterns[1][2] = EXP_PART;

        var EXPRESSION = new Pattern("EXPRESSION", new Grammar[] {EXP_PART}, (loc, nodes) => {
            List<AstNode> parts = MultiArray(nodes, 0).ToList();

            while (parts.Count > 1) {
                var ops = parts.Where((_, i) => i % 2 == 1).Select(o => ((AstNode.Identifier)o).content).ToList();
                int x = 1;
                foreach (string[] operators in OPERATOR_PRECEDENCE) {
                    int ops_i = ops.FindIndex(o => operators.Contains(o));
                    if (ops_i != -1) {
                        x = 2 * ops_i + 1;
                        break;
                    }
                }

                var left = parts[x - 1];
                var right = parts[x + 1];
                var op = ((AstNode.Identifier)parts[x]).content;
                parts.RemoveRange(x, 2);
                parts[x - 1] = new AstNode.FuncCall(loc, op, new AstNode[] {left, right});
            }

            return parts[0];
        });
        SUBEXPRESSION.possible_patterns[0][1] = EXPRESSION;

        var ARGUMENTS = Multiple<AstNode>("ARGUMENTS", new Grammar[] {EXPRESSION, COMMA}, optional: true);
        var FUNC_CALL = new Pattern("FUNC_CALL", new Grammar[] {IDENTIFIER, LPARAN, ARGUMENTS, RPARAN}, (loc, nodes) => {
            string name = ((AstNode.Identifier)nodes[0]).content;
            AstNode[] args = nodes[2] == null ? new AstNode[0] : MultiArray(nodes, 2);
            return new AstNode.FuncCall(loc, name, args);
        });
        TERM.possible_patterns[3][0] = FUNC_CALL;

        var VAR_ASSIGN = new Pattern("VAR_ASSIGN", new Grammar[] {IDENTIFIER, EQUAL, EXPRESSION}, (loc, nodes) => {
            string name = ((AstNode.Identifier)nodes[0]).content;
            return new AstNode.VarAssign(loc, name, nodes[2]);
        });

        var VAR_DEF = new Pattern("VAR_DEF", new Grammar[] {IDENTIFIER, VAR_ASSIGN}, (loc, nodes) => {
            string type = ((AstNode.Identifier)nodes[0]).content;
            return new AstNode.VarDef(loc, type, (AstNode.VarAssign)nodes[1]);
        });

        var LOCAL_BLOCK = new Pattern("LOCAL_BLOCK", new Grammar[] {LCURLY, null, RCURLY}, (_, nodes) => nodes[1]);

        var ELSE_PART = new Pattern("ELSE", new Grammar[][] {
            new Grammar[] {ONL, ELSE, LOCAL_BLOCK}, new Grammar[] {ONL, ELSE, null}
        }, (_, nodes) => nodes[2], optional: true);
        var IF_STMT = new Pattern("IF_STMT", new Grammar[] {IF, EXPRESSION, LOCAL_BLOCK, ELSE_PART}, (loc, nodes) => {
            AstNode[] if_stmts = MultiArray(nodes, 2);
            if (nodes[3] == null) return new AstNode.If(loc, nodes[1], if_stmts);
            if (nodes[3] is AstNode.If || nodes[3] is AstNode.IfElse) {
                return new AstNode.IfElse(loc, nodes[1], if_stmts, new AstNode[] {nodes[3]});
            }
            return new AstNode.IfElse(loc, nodes[1], if_stmts, MultiArray(nodes, 3));
        });
        ELSE_PART.possible_patterns[1][2] = IF_STMT;

        var WHILE = new Pattern("WHILE", new Grammar[] {new Tok(Token.Type.WHILE), EXPRESSION, LOCAL_BLOCK}, 
            (loc, nodes) => new AstNode.While(loc, nodes[1], MultiArray(nodes, 2))
        );

        var LOCAL_STMTS = NewStatements("LOCAL", new Grammar[] {VAR_DEF, VAR_ASSIGN, EXPRESSION, IF_STMT, WHILE});
        LOCAL_BLOCK.possible_patterns[0][1] = LOCAL_STMTS;

        // function definition identifier
        var FUNC_IDEN = OneOf("FUNC_IDEN", new Grammar[] {IDENTIFIER, OPERATOR});

        var PARAM = new Pattern("PARAM", new Grammar[] {IDENTIFIER, IDENTIFIER}, 
            (loc, nodes) => new AstNode.Param(loc, nodes)
        );
        var PARAM_LIST = Multiple<AstNode.Param>("PARAM_LIST", new Grammar[] {PARAM, COMMA}, optional: true);
        var FUNC_DEF = new Pattern("FUNC_DEF", 
            new Grammar[] {IDENTIFIER, FUNC_IDEN, LPARAN, PARAM_LIST, RPARAN, LOCAL_BLOCK},
            (loc, nodes) => new AstNode.FuncDef(loc, nodes)
        );

        var TYPE_LIST = Multiple<AstNode.Identifier>("TYPE_LIST", new Grammar[] {IDENTIFIER, COMMA}, optional: true);
        var IL_FUNC = new Pattern("IL_FUNC", 
            new Grammar[] {IL, IDENTIFIER, FUNC_IDEN, LPARAN, TYPE_LIST, RPARAN, STRING},
            (loc, nodes) => {
                string type = ((AstNode.Identifier)nodes[1]).content;
                string name = ((AstNode.Identifier)nodes[2]).content;
                string il = ((AstNode.StringLiteral)nodes[6]).content;
                string[] param_types;
                if (nodes[4] == null) {
                    param_types = new string[0];
                } else {
                    var values = ((AstNode.Multiple<AstNode.Identifier>)nodes[4]).ToArray();
                    param_types = Array.ConvertAll(values, v => v.content);
                }
                return new AstNode.IlFunc(loc, name, il, param_types, type);
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
        return new AstNode.Program(MultiArray(ast, 0));
    }

}

class AstNode {

    public readonly Location location;

    public AstNode(Location loc) {
        location = loc;
    }

    public virtual string Type(Scope scope) => throw new NotImplementedException();
    public virtual void CreateNameInfo(Scope scope) => throw new NotImplementedException();
    public virtual void GenIl(Scope scope) => throw new NotImplementedException();

    public void ExpectType(string expected, Scope scope) {
        string actual_type = Type(scope);
        if (actual_type != expected) {
            Umi.Crash($"Incorrect type. Expected {expected} but found {actual_type}", location);
        }
    }

    public class Tok : AstNode {
        public readonly Token token;
        public Tok(Token token) : base(token.location) => this.token = token;
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

    // For prefix and postfix operators
    public class Nothing : AstNode {
        public Nothing() : base(new Location(-1, -1, null, -1)) {}
        public override string Type(Scope _) => "void";
        public override void GenIl(Scope _) {}
    }

    public abstract class Value : AstNode {
        public readonly string content;
        public Value(Tok tok) : base(tok.location) => content = tok.token.value;
    }

    public class StringLiteral : Value {
        public StringLiteral(Tok tok): base(tok) {}
        public override string Type(Scope _) => "string";
        public override void GenIl(Scope _) => Output.WriteLine($"ldstr \"{content}\"");
    }

    public class IntegerLiteral : Value {
        public IntegerLiteral(Tok tok): base(tok) {}
        public override string Type(Scope _) => "int32";
        public override void GenIl(Scope _) => Output.WriteLine($"ldc.i4 {content}");
    }

    public class BooleanLiteral : IntegerLiteral {
        public BooleanLiteral(Tok tok): base(tok) {}
        public override string Type(Scope _) => "bool";
    }

    public class Identifier : Value {
        public Identifier(Tok tok): base(tok) {}

        public override string Type(Scope scope) => ((Name.Var)scope.LookUp(content, location)).type;

        public override void GenIl(Scope scope) {
            Name.Var variable = (Name.Var)scope.LookUp(content, location);
            if (variable.defined_at > location) {
                Umi.Crash($"Can not use variable `{content}` before it is defined", location);
            }
            Output.WriteLine($"{(variable.is_param ? "ldarg" : "ldloc")} {variable.index}");
        }
    }

    public class Param : AstNode {
        public readonly string type;
        public readonly string name;
        public Param(Location loc, List<AstNode> nodes) : base(loc) {
            this.type = ((AstNode.Identifier)nodes[0]).content;
            this.name = ((AstNode.Identifier)nodes[1]).content;
        }
    }

    public class FuncCall : AstNode {
        readonly string name;
        readonly AstNode[] arguments;

        public FuncCall(Location loc, string name, AstNode[] args) : base(loc) {
            this.name = name;
            arguments = args;
        }

        FuncInfo GetFuncInfo(Scope scope) {
            Name.Func func = (Name.Func)scope.LookUp(name, location);
            string[] arg_types = Array.ConvertAll(arguments, arg => arg.Type(scope));

            FuncInfo func_info = func.BestFit(arg_types);
            if (func_info == null) {
                string types = String.Join("`, `", arg_types);
                Umi.Crash($"Overload with types `{types}` does not exist for `{name}`", location);
            }

            return func_info;
        }

        public override string Type(Scope scope) => GetFuncInfo(scope).return_type;

        public override void GenIl(Scope scope) {
            FuncInfo func_info = GetFuncInfo(scope);
            foreach (var arg in arguments) arg.GenIl(scope);
            func_info.GenIl();
        }
    }
    
    // TODO: prevent assigning to parameters or make it work properly
    public class VarAssign : AstNode {
        public readonly string name;
        readonly AstNode value;

        public VarAssign(Location loc, string name, AstNode value) : base(loc) {
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
    
    public abstract class Block : AstNode {
        protected readonly AstNode[] statements;
        protected Scope block_scope;
        public Block(Location loc, AstNode[] statements) : base(loc) => this.statements = statements;

        // This mainly exists as a separate function for IfElse
        protected static Scope SubScope(Scope scope, List<string> local_var_types, AstNode[] stmts) {
            var new_scope = new Scope(scope);
            foreach (var stmt in stmts) {
                if (stmt is AstNode.VarDef) {
                    var vardef = (AstNode.VarDef)stmt;
                    string name = vardef.assignment.name;
                    int i = local_var_types.Count;
                    var variable = new Name.Var(vardef.type, name, false, i, vardef.location);
                    local_var_types.Add(variable.type);
                    new_scope.Add(name, variable);
                } else if (stmt is AstNode.Block) {
                    ((AstNode.Block)stmt).DeclareVariables(new_scope, local_var_types);
                }
            }
            return new_scope;
        }

        protected virtual void DeclareVariables(Scope scope, List<string> local_var_types) {
            block_scope = SubScope(scope, local_var_types, statements);
        }

        protected static void GenStatements(AstNode[] stmts, Scope scope) {
            foreach (var statement in stmts) statement.GenIl(scope);
        }

        protected void GenStatements() => GenStatements(statements, block_scope);
    }

    public class If : Block {
        protected readonly AstNode condition;

        public If(Location loc, AstNode condition, AstNode[] statements) : base(loc, statements) {
            this.condition = condition;
        }

        // The stuff that's common in If and IfElse and return the if_end label
        protected string IfIl(Scope scope) {
            condition.ExpectType("bool", scope);
            condition.GenIl(scope);
            // This is the first location of the last statement
            // Idk if this will cause errors later
            string if_end = statements.Last().location.Label();
            Output.WriteLine($"brfalse {if_end}");
            GenStatements();
            return if_end;
        }

        public override void GenIl(Scope scope) {
            string end = IfIl(scope);
            Output.WriteLine($"{end}:");
        }
    }

    public class IfElse : If {
        readonly AstNode[] else_statements;
        Scope else_scope;

        public IfElse(Location loc, AstNode cond, AstNode[] if_stmts, AstNode[] else_stmts) : base(loc, cond, if_stmts) {
            else_statements = else_stmts;
        }

        protected override void DeclareVariables(Scope scope, List<string> local_var_types) {
            base.DeclareVariables(scope, local_var_types);
            else_scope = SubScope(scope, local_var_types, else_statements);
        }

        public override void GenIl(Scope scope) {
            string if_end = IfIl(scope);
            string else_end = else_statements.Last().location.Label();
            Output.WriteLine($"br {else_end}");
            Output.WriteLine($"{if_end}:");
            GenStatements(else_statements, else_scope);
            Output.WriteLine($"{else_end}:");
        }
    }

    public class While : If {
        public While(Location loc, AstNode cond, AstNode[] stmts) : base(loc, cond, stmts) {}

        public override void GenIl(Scope scope) {
            string cond_label = condition.location.Label();
            Output.WriteLine($"{cond_label}:");
            string end = IfIl(scope);
            Output.WriteLine($"br {cond_label}");
            Output.WriteLine($"{end}:");
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

    public class FuncDef : Block {
        readonly string name;
        readonly string return_type;
        readonly Param[] parameters = new Param[0];
        readonly string il_signature; // in definition and calling

        readonly List<string> local_var_types = new List<string>();

        public FuncDef(Location loc, List<AstNode> nodes) : base(loc, ((Multiple<AstNode>)nodes[5]).ToArray()) {
            return_type = ((Identifier)nodes[0]).content;
            name = ((Identifier)nodes[1]).content;
            if (nodes[3] != null) {
                parameters = ((AstNode.Multiple<AstNode.Param>)nodes[3]).ToArray();
            }

            string[] param_types = Array.ConvertAll(parameters, p => p.type);
            string il_name = $"'{name}'";
            if (param_types.Length == 2) {
                if (param_types[0] == "void") {
                    il_name = $"'PREFIX{name}'";
                    param_types = new string[] {param_types[1]};
                } else if (param_types[1] == "void") {
                    il_name = $"'POSTFIX{name}'";
                    param_types = new string[] {param_types[0]};
                }
            }
            il_signature = $"{return_type} {il_name}({String.Join(", ", param_types)})";
        }

        public override void CreateNameInfo(Scope scope) {
            string[] types = Array.ConvertAll(parameters, p => p.type);
            FuncInfo func_info = new FuncInfo(types, return_type, "call " + il_signature);
            scope.AddFunction(location, name, func_info);

            DeclareVariables(scope, local_var_types);
            
            // Handle prefix operator functions
            if (parameters.Length == 2 && parameters[0].type == "void") {
                Param p = parameters[1];
                block_scope.Add(p.name, new Name.Var(p.type, p.name, true, 0, p.location));
            } else for (int i = 0; i < parameters.Length; i++) {
                Param p = parameters[i];
                var par = new Name.Var(p.type, p.name, true, i, p.location);
                block_scope.Add(p.name, par);
            }
        }

        public override void GenIl(Scope scope) {
            Output.WriteLine($".method static {il_signature}");
            Output.WriteLine("{");
            Output.Indent();
            if (name == "main") Output.WriteLine(".entrypoint");

            if (local_var_types.Count > 0) {
                Output.WriteLine(".locals init (");
                Output.Indent();
                for (int i = 0; i < local_var_types.Count - 1; i++) {
                    string type = local_var_types[i];
                    Output.WriteLine($"{type} VAR_{i},");
                }
                string last_type = local_var_types.Last();
                Output.WriteLine($"{last_type} VAR_{local_var_types.Count - 1}");
                Output.Unindent();
                Output.WriteLine(")");
            }
            
            GenStatements();
            Output.WriteLine("ret");
            Output.Unindent();
            Output.WriteLine("}\n");
        }
    }

    public class IlFunc : AstNode {
        readonly string name;
        readonly string il;
        readonly string[] param_types;
        readonly string return_type;

        public IlFunc(Location loc, string name, string il, string[] param_types, string rt) : base(loc) {
            this.name = name;
            this.il = il;
            this.param_types = param_types;
            return_type = rt;
        }

        public override void CreateNameInfo(Scope scope) {
            scope.AddFunction(location, name, new FuncInfo(param_types, return_type, il));
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

    public class Func : Name {
        // The various definitions for the different overloads
        public readonly List<FuncInfo> functions = new List<FuncInfo>();
        public Func(Location defined_at) : base(defined_at) {}

        // Get the FuncInfo that fits the types the best
        // Currently this means that it's exactly correct
        // Returns null if nothing matches
        public FuncInfo BestFit(string[] types) {
            foreach (var func_info in functions) {
                if (Enumerable.SequenceEqual(func_info.param_types, types)) {
                    return func_info;
                }
            }
            return null;
        }
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

class FuncInfo {
    public readonly string[] param_types;
    public readonly string return_type;
    readonly string il;

    public FuncInfo(string[] param_types, string return_type, string il) {
        this.param_types = param_types;
        this.return_type = return_type;
        this.il = il;
    }

    public void GenIl() => Output.WriteLine(il);
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

    public void AddFunction(Location location, string name, FuncInfo func_info) {
        Name.Func func;
        if (names.TryGetValue(name, out Name possibly_func)) {
            // TODO: handle error properly
            func = (Name.Func)possibly_func;
        } else {
            func = new Name.Func(location);
            Add(name, func);
        }

        // Check that the overload doesn't already exist
        foreach (var fi in func.functions) {
            if (Enumerable.SequenceEqual(func_info.param_types, fi.param_types)) {
                Umi.Crash($"This overload for `{name}` already exists", location);
            }
        }

        func.functions.Add(func_info);
    }
}

class Umi {
    
    public static void Crash(string message, Location loc) {
        Console.WriteLine(loc + ": " + message);
        Environment.Exit(1);
    }

    static void Main(string[] args) {
        if (args.Length < 1) {
            Console.WriteLine("You must specify the path to the file");
            Environment.Exit(1);
        }

        List<Token> tokens = new Lexer("std.umi").Lex(new List<Token>());
        tokens = new Lexer(args[0]).Lex(tokens);
        AstNode.Program ast = Grammar.ParseTokens(tokens);

        Scope global_namespace = new Scope(null);
        ast.CreateNameInfo(global_namespace);
        ast.GenIl(global_namespace);

        using (Process ilasm = new Process()) {
            ilasm.StartInfo.FileName = "ilasm";
            ilasm.StartInfo.Arguments = "output.il";
            ilasm.StartInfo.UseShellExecute = false;
            ilasm.StartInfo.RedirectStandardOutput = true;
            ilasm.Start();
            ilasm.WaitForExit();
            if (ilasm.ExitCode != 0) {
                Console.WriteLine("ilasm failed to assemble output.il");
                Environment.Exit(1);
            }
        }
    }

}
