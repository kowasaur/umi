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
        IDENTIFIER, OPERATOR,
        STRING, CHAR, INTEGER, BOOLEAN,
        LPARAN, RPARAN, LCURLY, RCURLY,
        NEWLINE, COMMA, DOT, EQUAL,
        IL, ILF, ALIAS, CLASS,
        IF, ELSE, WHILE
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

    char NextChar() {
        Increment();
        return file[i];
    }

    string ConsumeWhile(ContinueConsuming continueConsuming) {
        string content = "";
        while (continueConsuming(file[i + 1])) content += NextChar();
        return content;
    }

    string ConsumeWhile(char start, ContinueConsuming cc) => start.ToString() + ConsumeWhile(cc);
    
    string QuoteConsume(char q) {
        string content = "";
        while(file[i + 1] != q) {
            content += NextChar();
            if (file[i] == '\\' && (file[i + 1] == q || file[i + 1] == '\\')) content += NextChar();
        }
        Increment();
        return content;
    }

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
                    case "class":
                        type = Token.Type.CLASS;
                        break;
                    case "il":
                        type = Token.Type.IL;
                        break;
                    case "ilf":
                        type = Token.Type.ILF;
                        break;
                    case "alias":
                        type = Token.Type.ALIAS;
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
                case '.':
                    type = Token.Type.DOT;
                    break;
                case '"':
                    type = Token.Type.STRING;
                    value = QuoteConsume('"');
                    break;
                case '\'':
                    type = Token.Type.CHAR;
                    value = QuoteConsume('\'');
                    if (value.Length == 1) value = ((int)value[0]).ToString();
                    else if (value.Length == 2 && value[0] == '\\') value = ((int)value[1]).ToString();
                    else Umi.Crash("Chars may only be one character long", loc);
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

    static readonly Tok LCURLY = new Tok(Token.Type.LCURLY);
    static readonly Tok RCURLY = new Tok(Token.Type.RCURLY);
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

    static Pattern NewBlock(string name, Pattern statements) {
        return new Pattern(name, new Grammar[] {LCURLY, statements, RCURLY}, (_, nodes) => nodes[1]);
    }

    // Shorthands
    static AstNode[] MultiArray(List<AstNode> nodes, int i) => ((AstNode.Multiple<AstNode>)nodes[i]).ToArray();
    static string IdentifierText(List<AstNode> nodes, int i) => ((AstNode.Identifier)nodes[i]).content;
    
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
        var COMMA = new Tok(Token.Type.COMMA);
        var DOT = new Tok(Token.Type.DOT);
        var EQUAL = new Tok(Token.Type.EQUAL);
        var IL = new Tok(Token.Type.IL);
        var ILF = new Tok(Token.Type.ILF);
        var IF = new Tok(Token.Type.IF);
        var ELSE = new Tok(Token.Type.ELSE);

        var STRING = new Pattern("STRING", new Grammar[] {new Tok(Token.Type.STRING)}, 
            (_, nodes) => new AstNode.StringLiteral((AstNode.Tok)nodes[0])
        );
        var CHAR = new Pattern("CHAR", new Grammar[] {new Tok(Token.Type.CHAR)}, 
            (_, nodes) => new AstNode.CharLiteral((AstNode.Tok)nodes[0])
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

        // TODO: allow the left side to be any expression
        // class.field etc
        var FULLNAME = new Pattern("FULLNAME", new Grammar[] {IDENTIFIER, DOT, IDENTIFIER}, 
            (loc, nodes) => new AstNode.Dot(loc, nodes[0], IdentifierText(nodes, 2))
        );

        var SUBEXPRESSION = new Pattern ("SUBEXPRESSION", new Grammar[] {LPARAN, null, RPARAN}, 
            (_, nodes) => nodes[1]
        );

        // A single term in an expresion
        var TERM = OneOf("TERM", new Grammar[] {STRING, CHAR, INTEGER, BOOLEAN, null, FULLNAME, IDENTIFIER, SUBEXPRESSION});

        var POSTFIX = new Pattern("POSTFIX", new Grammar[] {TERM, OPERATOR}, (loc, nodes) => {
            string name = IdentifierText(nodes, 1);
            return new AstNode.FuncCall(loc, name, new AstNode[] {nodes[0], new AstNode.Nothing()});
        });
        var PREFIX = new Pattern("PREFIX", new Grammar[] {OPERATOR, TERM}, (loc, nodes) => {
            string name = IdentifierText(nodes, 0);
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
            AstNode[] args = nodes[2] == null ? new AstNode[0] : MultiArray(nodes, 2);
            return new AstNode.FuncCall(loc, IdentifierText(nodes, 0), args);
        });
        TERM.possible_patterns[4][0] = FUNC_CALL;

        var FIELD_ASSIGN = new Pattern("FIELD_ASSIGN", new Grammar[] {FULLNAME, EQUAL, EXPRESSION}, 
            (loc, nodes) => new AstNode.FieldAssign(loc, (AstNode.Dot)nodes[0], nodes[2])
        );

        var VAR_ASSIGN = new Pattern("VAR_ASSIGN", new Grammar[] {IDENTIFIER, EQUAL, EXPRESSION}, 
            (loc, nodes) => new AstNode.VarAssign(loc, IdentifierText(nodes, 0), nodes[2])
        );

        var VAR_DEF = new Pattern("VAR_DEF", new Grammar[] {IDENTIFIER, VAR_ASSIGN}, (loc, nodes) => {
            string type = IdentifierText(nodes, 0);
            return new AstNode.VarDef(loc, type, (AstNode.VarAssign)nodes[1]);
        });

        var LOCAL_BLOCK = NewBlock("LOCAL_BLOCK", null);

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

        var LOCAL_STMTS = NewStatements("LOCAL", new Grammar[] {VAR_DEF, VAR_ASSIGN, FIELD_ASSIGN, EXPRESSION, IF_STMT, WHILE});
        LOCAL_BLOCK.possible_patterns[0][1] = LOCAL_STMTS;

        // function definition identifier
        var FUNC_IDEN = OneOf("FUNC_IDEN", new Grammar[] {IDENTIFIER, OPERATOR});

        var PARAM = new Pattern("PARAM", new Grammar[] {IDENTIFIER, IDENTIFIER}, 
            (loc, nodes) => new AstNode.Param(loc, IdentifierText(nodes, 0), IdentifierText(nodes, 1))
        );
        var PARAM_LIST = Multiple<AstNode.Param>("PARAM_LIST", new Grammar[] {PARAM, COMMA}, optional: true);
        var FUNC_DEF = new Pattern("FUNC_DEF", 
            new Grammar[] {IDENTIFIER, FUNC_IDEN, LPARAN, PARAM_LIST, RPARAN, LOCAL_BLOCK},
            (loc, nodes) => new AstNode.FuncDef(loc, nodes, true)
        );

        var TYPE_LIST = Multiple<AstNode.Identifier>("TYPE_LIST", new Grammar[] {IDENTIFIER, COMMA}, optional: true);
        var IL_FUNC = new Pattern("IL_FUNC", 
            new Grammar[] {OneOf("ILish", new Grammar[] {IL, ILF}), IDENTIFIER, FUNC_IDEN, LPARAN, TYPE_LIST, RPARAN, STRING},
            (loc, nodes) => {
                string type = IdentifierText(nodes, 1);
                string name = IdentifierText(nodes, 2);
                string il = ((AstNode.StringLiteral)nodes[6]).content;
                
                string[] param_types;
                if (nodes[4] == null) {
                    param_types = new string[0];
                } else {
                    var values = ((AstNode.Multiple<AstNode.Identifier>)nodes[4]).ToArray();
                    param_types = Array.ConvertAll(values, v => v.content);
                }

                if (((AstNode.Tok)nodes[0]).token.type == Token.Type.ILF) {
                    il = $"call {type} {il}({String.Join(", ", param_types)})";
                }

                return new AstNode.IlFunc(loc, name, il, param_types, type);
            }
        );

        var ALIAS = new Pattern("ALIAS", new Grammar[] {new Tok(Token.Type.ALIAS), IDENTIFIER, EQUAL, EXPRESSION}, 
            (loc, nodes) => new AstNode.Alias(loc, IdentifierText(nodes, 1), nodes[3])
        );

        // TODO: maybe make a pattern for the "type name' thing
        var FIELD = new Pattern("FIELD", new Grammar[] {IDENTIFIER, IDENTIFIER}, 
            (loc, nodes) => new AstNode.Field(loc, IdentifierText(nodes, 0), IdentifierText(nodes, 1))
        );
        
        var METHOD = new Pattern("METHOD", 
            new Grammar[] {IDENTIFIER, FUNC_IDEN.NewOptional("OPT_IDEN"), LPARAN, PARAM_LIST, RPARAN, LOCAL_BLOCK},
            (loc, nodes) => {
                if (nodes[1] == null) nodes[1] = nodes[0];
                return new AstNode.FuncDef(loc, nodes, false);
            }
        );

        var CLASS_STMTS = NewStatements("CLASS_STMTS", new Grammar[] {METHOD, FIELD});
        var CLASS_BLOCK = NewBlock("CLASS_BLOCK", CLASS_STMTS);
        var CLASS = new Pattern("CLASS", new Grammar[] {new Tok(Token.Type.CLASS), IDENTIFIER, CLASS_BLOCK}, (loc, nodes) => {
            return new AstNode.Class(loc, IdentifierText(nodes, 1), MultiArray(nodes, 2));
        });

        var GLOBAL_STATEMENTS = NewStatements("GLOBAL", new Grammar[] {FUNC_DEF, IL_FUNC, ALIAS, CLASS});

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

    public class CharLiteral : IntegerLiteral {
        public CharLiteral(Tok tok) : base(tok) {}
        public override string Type(Scope _) => "char";
    }

    public class Identifier : Value {
        public Identifier(Tok tok): base(tok) {}

        public override string Type(Scope scope) => ((Name.Varish)scope.LookUp(content, location)).Type(scope);

        public override void GenIl(Scope scope) {
            Name.Varish varish = (Name.Varish)scope.LookUp(content, location);
            varish.GenIl(scope, this);
        }
    }

    public class Param : AstNode {
        public readonly string type;
        public readonly string name;
        public Param(Location loc, string type, string name) : base(loc) {
            this.type = type;
            this.name = name;
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
            Name maybe_func = scope.LookUp(name, location);
            Name.Func func;
            if (maybe_func is Name.Func) func = (Name.Func)maybe_func;
            else if (maybe_func is Name.Class) func = ((Name.Class)maybe_func).constructor; 
            else {
                Umi.Crash("Tried to call something that is not a function", location);
                return null;
            }

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
            Name maybe_var = scope.LookUp(name, location);
            if (!(maybe_var is Name.Var)) {
                if (maybe_var is Name.Field) {
                    var this_tok = new AstNode.Tok(new Token(Token.Type.NOTHING, location, "this"));
                    var dot = new AstNode.Dot(location, new AstNode.Identifier(this_tok), name);
                    new AstNode.FieldAssign(location, dot, value).GenIl(scope);
                    return;
                } else {
                    Umi.Crash("Can only reassign local variables", location);
                }
            }
            
            Name.Var variable = (Name.Var)maybe_var;
            if (variable.defined_at > location) {
                Umi.Crash($"Can not assign to variable `{name}` before it is defined", location);
            }

            value.ExpectType(variable.type, scope);
            value.GenIl(scope);
            Output.WriteLine($"stloc {variable.index}");
        }
    }

    public class FieldAssign : AstNode {
        readonly Dot dot;
        readonly AstNode value;

        public FieldAssign(Location loc, Dot dot, AstNode value) : base(loc) {
            this.dot = dot;
            this.value = value;
        }

        public override void GenIl(Scope scope) {
            Name.Class cls = dot.GetClass(scope);
            string type = dot.Type(cls);
            value.ExpectType(type, scope);

            dot.parent.GenIl(scope);
            value.GenIl(scope);
            Output.WriteLine($"stfld {type} {cls.name}::{dot.child}");
        }
    }
    
    // TODO: Make If, etc HAVE a Block not inherit it
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
                    var variable = new Name.Var(vardef.type, false, i, vardef.location);
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
        public readonly string name;
        readonly string return_type;
        readonly Param[] parameters = new Param[0];
        readonly bool is_static;

        // for definition and calling
        readonly string il_name;
        readonly string param_types;
        string parent_class;

        readonly List<string> local_var_types = new List<string>();

        public FuncDef(Location loc, List<AstNode> nodes, bool is_static) : base(loc, ((Multiple<AstNode>)nodes[5]).ToArray()) {
            this.is_static = is_static;
            return_type = ((Identifier)nodes[0]).content;
            name = ((Identifier)nodes[1]).content;
            if (nodes[3] != null) {
                parameters = ((AstNode.Multiple<AstNode.Param>)nodes[3]).ToArray();
            }

            string[] parameter_types = Array.ConvertAll(parameters, p => p.type);
            il_name = $"'{name}'";
            if (name == return_type) { // constructor
                il_name = ".ctor";
            } if (parameter_types.Length == 2) { // unary operator
                if (parameter_types[0] == "void") {
                    il_name = $"'PREFIX{name}'";
                    parameter_types = new string[] {parameter_types[1]};
                } else if (parameter_types[1] == "void") {
                    il_name = $"'POSTFIX{name}'";
                    parameter_types = new string[] {parameter_types[0]};
                }
            }
            param_types = string.Join(", ", parameter_types);
        }

        public void SetParentClass(string pc) => parent_class = pc;

        string IlSignature(string n) {
            string rt = return_type == name ? "void" : return_type; // constructor
            return $"{rt} {n}({param_types})";
        }

        public FuncInfo CreateFuncInfo(Scope scope) {
            string[] types = Array.ConvertAll(parameters, p => p.type);
            string call_word = is_static ? "call " : "callvirt ";
            if (return_type == name) call_word = "newobj ";
            string full_name = parent_class == null ? il_name : parent_class + "::" + il_name;
            FuncInfo func_info = new FuncInfo(types, return_type, call_word + IlSignature(full_name));

            DeclareVariables(scope, local_var_types);
            
            List<Param> paras = parameters.ToList();
            if (!is_static) paras.Insert(0, new Param(location, parent_class, "this"));

            // Handle prefix operator functions
            if (paras.Count == 2 && paras[0].type == "void") {
                Param p = paras[1];
                block_scope.Add(p.name, new Name.Var(p.type, true, 0, p.location));
            } else for (int i = 0; i < paras.Count; i++) { // TODO: add `this`
                Param p = paras[i];
                var par = new Name.Var(p.type, true, i, p.location);
                block_scope.Add(p.name, par);
            }

            return func_info;
        }

        public override void CreateNameInfo(Scope scope) => scope.AddFunction(location, name, CreateFuncInfo(scope));

        public override void GenIl(Scope scope) {
            Output.WriteLine($".method {(is_static ? "static " : "")}{IlSignature(il_name)}");
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

    public class Alias : AstNode {
        readonly string name;
        readonly AstNode value;

        public Alias(Location loc, string name, AstNode value) : base(loc) {
            this.name = name;
            this.value = value;
        }

        public override void CreateNameInfo(Scope scope) => scope.Add(name, new Name.Alias(location, value));
        public override void GenIl(Scope _) {}
    }

    // e.g parent.child
    public class Dot : AstNode {
        public readonly AstNode parent;
        public readonly string child;

        public Dot(Location loc, AstNode parent, string child): base(loc) {
            this.parent = parent;
            this.child = child;
        }

        public Name.Class GetClass(Scope scope) {
            string cls_type = parent.Type(scope);
            return (Name.Class)scope.LookUp(cls_type, location);
        }

        Name.Field GetField(Name.Class cls) => ((Name.Field)cls.members.LookUp(child, location));
        
        public string Type(Name.Class cls) => GetField(cls).type;

        public override string Type(Scope scope) => Type(GetClass(scope));

        public override void GenIl(Scope scope) {
            parent.GenIl(scope);
            Name.Class cls = GetClass(scope);
            Output.WriteLine($"ldfld {Type(cls)} {cls.name}::{child}");
        }
    }

    public class Field : AstNode {
        readonly string type;
        readonly string name;

        public Field(Location loc, string type, string name) : base(loc) {
            this.type = type;
            this.name = name;
        }

        public override void CreateNameInfo(Scope scope) => scope.Add(name, new Name.Field(location, type));
        public override void GenIl(Scope scope) => Output.WriteLine($".field {type} {name}\n");
    }

    public class Class : AstNode {
        readonly string name;
        readonly AstNode[] statements;
        Scope class_scope;

        public Class(Location loc, string name, AstNode[] statements) : base(loc) {
            this.name = name;
            this.statements = statements;
        }

        public override void CreateNameInfo(Scope scope) {
            class_scope = new Scope(scope);
            var this_class = new Name.Class(location, name, class_scope);
            scope.Add(name, this_class);

            foreach (AstNode stmt in statements) {
                if (stmt is AstNode.FuncDef) { 
                    var fd = (AstNode.FuncDef)stmt;
                    fd.SetParentClass(name);
                    if (fd.name == name) this_class.constructor.functions.Add(fd.CreateFuncInfo(class_scope));
                    else fd.CreateNameInfo(class_scope);
                } else {
                    stmt.CreateNameInfo(class_scope);
                }
            }
        }

        public override void GenIl(Scope scope) {
            Output.WriteLine($".class {name}");
            Output.WriteLine("{");
            Output.Indent();
            foreach (AstNode stmt in statements) stmt.GenIl(class_scope);
            Output.Unindent();
            Output.WriteLine("}\n");
        }
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

    public class Class : Name {
        public readonly string name;
        public readonly Func constructor;
        public readonly Scope members;

        public Class(Location loc, string name, Scope members) : base(loc) {
            this.name = name;
            this.members = members;
            constructor = new Func(loc);
        }
    }

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

    // Var or Alias
    public abstract class Varish : Name {
        public Varish(Location defined_at) : base(defined_at) {}
        public abstract string Type(Scope scope);
        public abstract void GenIl(Scope scope, AstNode.Identifier context);
    }

    public class Var : Varish {
        public readonly string type;
        public readonly int index;
        readonly bool is_param; // local variable or parameter
        public Var(string type, bool is_param, int index, Location defined_at) : base(defined_at) {
            this.type = type;
            this.is_param = is_param;
            this.index = index;
        }
        public override string Type(Scope _) => type;
        public override void GenIl(Scope _, AstNode.Identifier context) {
            if (defined_at > context.location) {
                Umi.Crash($"Can not use variable `{context.content}` before it is defined", context.location);
            }
            Output.WriteLine($"{(is_param ? "ldarg" : "ldloc")} {index}");
        }
        
    }

    public class Alias : Varish {
        public AstNode node;
        public Alias(Location defined_at, AstNode node) : base(defined_at) => this.node = node;
        public override string Type(Scope scope) => node.Type(scope);
        public override void GenIl(Scope scope, AstNode.Identifier _) => node.GenIl(scope);
    }

    public class Field : Name {
        public readonly string type;
        public Field(Location defined_at, string type) : base(defined_at) => this.type = type;
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
