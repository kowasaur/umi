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
        LPARAN, RPARAN, LCURLY, RCURLY, LSQUARE, RSQUARE,
        NEWLINE, COMMA, EQUAL, COLON,
        IL, ILS, ALIAS, CLASS, MUT, STATIC,
        IF, ELSE, WHILE, INCLUDE,
        RETURN, BREAK, CONTINUE,
        AS
    }

}

class Lexer {
    
    readonly string file;
    readonly string file_path;

    int i = 0;
    int line = 1;
    int column = 1;

    public Lexer(string file_path) {
        string path = file_path;
        if (!File.Exists(file_path)) {
            path = AppDomain.CurrentDomain.BaseDirectory + path; // path of exe
            if (!File.Exists(path)) Umi.Exit(file_path + " does not exist");
        }

        file = File.ReadAllText(path);
        this.file_path = path;
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

    string ConsumeWhile(Func<char, bool> continueConsuming) {
        string content = "";
        while (continueConsuming(file[i + 1])) content += NextChar();
        return content;
    }

    string ConsumeWhile(char start, Func<char, bool> cc) => start.ToString() + ConsumeWhile(cc);
    
    string QuoteConsume(char q) {
        string content = "";
        while(file[i + 1] != q) {
            content += NextChar();
            if (file[i] == '\\' && (file[i + 1] == q || file[i + 1] == '\\')) content += NextChar();
        }
        Increment();
        return content;
    }

    static bool IsOperatorChar(char c) => "+-*/%&|=><!.".Contains(c);
    
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
                    case "ils":
                        type = Token.Type.ILS;
                        break;
                    case "alias":
                        type = Token.Type.ALIAS;
                        break;
                    case "mut":
                        type = Token.Type.MUT;
                        break;
                    case "static":
                        type = Token.Type.STATIC;
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
                    case "return":
                        type = Token.Type.RETURN;
                        break;
                    case "break":
                        type = Token.Type.BREAK;
                        break;
                    case "continue":
                        type = Token.Type.CONTINUE;
                        break;
                    case "as":
                        type = Token.Type.OPERATOR;
                        value = "as";
                        break;
                    case "include":
                        type = Token.Type.INCLUDE;
                        break;
                    default:
                        type = Token.Type.IDENTIFIER;
                        value = content;
                        break;
                }
            } else if (Char.IsDigit(c)) {
                type = Token.Type.INTEGER;
                value = ConsumeWhile(c, Char.IsDigit);
                if (file[i + 1] == 'x') {
                    value += ConsumeWhile(NextChar(), ch => Char.IsDigit(ch) || ('A' <= ch && ch <= 'F'));
                }
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
                case '[':
                    type = Token.Type.LSQUARE;
                    break;
                case ']':
                    type = Token.Type.RSQUARE;
                    break;
                case ',':
                    type = Token.Type.COMMA;
                    break;
                case ':':
                    type = Token.Type.COLON;
                    break;
                case '"':
                    type = Token.Type.STRING;
                    value = QuoteConsume('"');
                    break;
                case '\'':
                    type = Token.Type.CHAR;
                    value = QuoteConsume('\'');
                    if (value.Length == 1) value = ((int)value[0]).ToString();
                    else if (value.Length == 2 && value[0] == '\\') {
                        if (value[1] == 'n') value = "10"; // \n
                        else value = ((int)value[1]).ToString();
                    } 
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

    public List<Token> Lex() => Lex(new List<Token>());

}

abstract class Grammar {

    readonly bool optional;
    protected abstract AstNode GenAst();
    public Grammar(bool optional) => this.optional = optional; 

    static List<Token> tokens;
    static int i; // the cursor in tokens

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
    static Pattern OneOf(string name, Grammar[] options, bool optional = false) {
        var pat = new Pattern(name, new Grammar[options.Length][], (_, nodes) => nodes[0], optional);
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

    static readonly Pattern PROGRAM = CreateProgramGrammar();

    static Pattern NewStatements(string space, Grammar[] possibilities) {
        string name = space + "_STATEMENT";
        var statement = OneOf(name, possibilities);
        var statements = Multiple<AstNode>(name + "S", new Grammar[] {statement, MNL});
        return new Pattern(name + "S+NL", new Grammar[] {ONL, statements, ONL}, (_, nodes) => nodes[1], optional: true);
    }

    static Pattern NewBlock(string name, Pattern statements) => new Pattern(
        name, 
        new Grammar[] {LCURLY, statements, RCURLY}, 
        (loc, nodes) => new AstNode.Statements(loc, MultiArray<AstNode>(nodes[1]), nodes[2].location)
    );

    static T[] MultiArray<T>(AstNode node) where T : AstNode => node == null ? new T[0] : ((AstNode.Multiple<T>)node).ToArray();

    static string ValueText(AstNode node) => ((AstNode.Value)node).content;
    static string MaybeValueText(AstNode node) => node == null ? null : ValueText(node);

    static Name.Generic[] GenericsArray(AstNode node, bool is_func = false) {
        return MultiArray<AstNode.Generic>(node).Select((g, i) => new Name.Generic(g.location, g.name, i, g.constraint, is_func)).ToArray();
    }

    static string[] GenericTypesArray(AstNode node) => Array.ConvertAll(MultiArray<AstNode.Generic>(node), g => g.name);
    
    static Type TypeNodeType(AstNode node) => ((AstNode.TypeNode)node).type;

    static readonly string[][] OPERATOR_PRECEDENCE = {
        new string[] {"."},
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
        var LSQUARE = new Tok(Token.Type.LSQUARE);
        var RSQUARE = new Tok(Token.Type.RSQUARE);
        var COMMA = new Tok(Token.Type.COMMA);
        var COLON = new Tok(Token.Type.COLON);
        var EQUAL = new Tok(Token.Type.EQUAL);
        var MUT = new Tok(Token.Type.MUT, optional: true);
        var STATIC = new Tok(Token.Type.STATIC, optional: true);
        var CLASS = new Tok(Token.Type.CLASS);
        var IL = new Tok(Token.Type.IL);
        var ILS = new Tok(Token.Type.ILS);
        var IF = new Tok(Token.Type.IF);
        var ELSE = new Tok(Token.Type.ELSE);

        var ILish = OneOf("ILish", new Grammar[] {IL, ILS});

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

        var SUBEXPRESSION = new Pattern ("SUBEXPRESSION", new Grammar[] {LPARAN, null, RPARAN}, (_, nodes) => nodes[1]);

        // A single term in an expresion
        var TERM = OneOf("TERM", new Grammar[] {STRING, CHAR, INTEGER, BOOLEAN, null, null, IDENTIFIER, SUBEXPRESSION});

        var POSTFIX = new Pattern("POSTFIX", new Grammar[] {TERM, OPERATOR}, (loc, nodes) => {
            string name = ValueText(nodes[1]);
            return new AstNode.FuncCall(loc, name, new AstNode[] {nodes[0], new AstNode.Nothing()});
        });
        var PREFIX = new Pattern("PREFIX", new Grammar[] {OPERATOR, TERM}, (loc, nodes) => {
            string name = ValueText(nodes[0]);
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
            List<AstNode> parts = ((AstNode.Multiple<AstNode>)nodes[0]).ToList();

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
                AstNode.Identifier op_node = (AstNode.Identifier)parts[x];
                string op = op_node.content;
                parts.RemoveRange(x, 2);

                if (op == "as") {
                    if (!(right is AstNode.Identifier)) Umi.Crash("Must cast to class", right.location);
                    parts[x - 1] = new AstNode.TypeCast(ValueText(right), left, op_node.location);
                } else if (op == ".") {
                    if (right is AstNode.Identifier) parts[x - 1] = new AstNode.Dot(left.location, left, ValueText(right));
                    else if (right is AstNode.FuncCall) parts[x - 1] = ((AstNode.FuncCall)right).AsMethodCall(left);
                    else Umi.Crash("Not valid class member", right.location);
                } else {
                    parts[x - 1] = new AstNode.FuncCall(op_node.location, op, new AstNode[] {left, right});
                }
            }

            return parts[0];
        });
        SUBEXPRESSION.possible_patterns[0][1] = EXPRESSION;

        var INHERIT = new Pattern("INHERIT", new Grammar[] {COLON, IDENTIFIER}, (_, nodes) => nodes[1], optional: true);

        var GENERIC = new Pattern("GENERIC", new Grammar[] {IDENTIFIER, INHERIT}, 
            (_, nodes) => new AstNode.Generic((AstNode.Identifier)nodes[0], MaybeValueText(nodes[1]))
        );
        var GENERICS = new Pattern("GENERICS", 
            new Grammar[] {LSQUARE, Multiple<AstNode.Generic>("GENERIC_LIST", new Grammar[] {GENERIC, COMMA}), RSQUARE},
            (loc, nodes) => nodes[1], optional: true
        );

        var TYPE = new Pattern("TYPE", new Grammar[] {IDENTIFIER, GENERICS}, 
            (_, nodes) => new AstNode.TypeNode((AstNode.Identifier)nodes[0], GenericTypesArray(nodes[1]))
        );

        var ARGUMENTS = Multiple<AstNode>("ARGUMENTS", new Grammar[] {EXPRESSION, COMMA}, optional: true);
        var FUNC_CALL = new Pattern("FUNC_CALL", new Grammar[] {IDENTIFIER, GENERICS, LPARAN, ARGUMENTS, RPARAN},
            // TODO: make GenericTypesArray work properly
            (loc, nodes) => new AstNode.FuncCall(loc, ValueText(nodes[0]), MultiArray<AstNode>(nodes[3]), Array.ConvertAll(GenericTypesArray(nodes[1]), s => new Type(s)))
        );
        TERM.possible_patterns[4][0] = FUNC_CALL;

        var VAR_ASSIGN = new Pattern("VAR_ASSIGN", new Grammar[] {EXPRESSION, EQUAL, EXPRESSION}, (loc, nodes) => {
            if (nodes[0] is AstNode.Identifier) return new AstNode.VarAssign(loc, ValueText(nodes[0]), nodes[2]);
            if (nodes[0] is AstNode.Dot) return new AstNode.FieldAssign(loc, (AstNode.Dot)nodes[0], nodes[2]);
            return null; // If you Umi.Crash() here, the parser breaks
        });

        var VAR_DEF = new Pattern("VAR_DEF", new Grammar[] {MUT, TYPE, VAR_ASSIGN}, (loc, nodes) => {
            Type type = TypeNodeType(nodes[1]);
            return new AstNode.VarDef(loc, type, (AstNode.VarAssign)nodes[2], nodes[0] != null);
        });

        var LOCAL_BLOCK = NewBlock("LOCAL_BLOCK", null);

        var ELSE_PART = new Pattern("ELSE", new Grammar[][] {
            new Grammar[] {ONL, ELSE, LOCAL_BLOCK}, new Grammar[] {ONL, ELSE, null}
        }, (_, nodes) => nodes[2], optional: true);
        var IF_STMT = new Pattern("IF_STMT", new Grammar[] {IF, EXPRESSION, LOCAL_BLOCK, ELSE_PART}, (loc, nodes) => {
            AstNode.Statements if_stmts = (AstNode.Statements)nodes[2];
            if (nodes[3] == null) return new AstNode.If(loc, nodes[1], if_stmts);
            if (nodes[3] is AstNode.If) {
                Location end;
                if (nodes[3] is AstNode.IfElse) end = ((AstNode.IfElse)nodes[3]).else_statements.end;
                else end = ((AstNode.If)nodes[3]).statements.end;
                var single_stmt = new AstNode.Statements(nodes[3].location, new AstNode[] {nodes[3]}, end);
                return new AstNode.IfElse(loc, nodes[1], if_stmts, single_stmt);
            }
            return new AstNode.IfElse(loc, nodes[1], if_stmts, (AstNode.Statements)nodes[3]);
        });
        ELSE_PART.possible_patterns[1][2] = IF_STMT;
        TERM.possible_patterns[5][0] = IF_STMT;

        var WHILE = new Pattern("WHILE", new Grammar[] {new Tok(Token.Type.WHILE), EXPRESSION, LOCAL_BLOCK}, 
            (loc, nodes) => new AstNode.While(loc, nodes[1], (AstNode.Statements)nodes[2])
        );

        var RETURN = new Pattern("RETURN", new Grammar[] {new Tok(Token.Type.RETURN), EXPRESSION.NewOptional("OPT_EXP")},
            (loc, nodes) => new AstNode.Return(loc, nodes[1])
        );
        var BREAK = new Pattern("BREAK", new Grammar[] {new Tok(Token.Type.BREAK)}, (loc, _) => new AstNode.Break(loc));
        var CONTINUE = new Pattern("CONTINUE", new Grammar[] {new Tok(Token.Type.CONTINUE)}, (loc, _) => new AstNode.Continue(loc));

        var LOCAL_STMTS = NewStatements("LOCAL", 
            new Grammar[] {VAR_DEF, VAR_ASSIGN, EXPRESSION, IF_STMT, WHILE, RETURN, BREAK, CONTINUE}
        );
        LOCAL_BLOCK.possible_patterns[0][1] = LOCAL_STMTS;

        // function definition identifier
        var FUNC_IDEN = OneOf("FUNC_IDEN", new Grammar[] {IDENTIFIER, OPERATOR}, optional: true);

        var FUNC_DEF_HEAD = new Pattern("FUNC_DEF_HEAD", new Grammar[] {STATIC, TYPE, FUNC_IDEN, GENERICS}, (loc, nodes) => {
            Type return_type = TypeNodeType(nodes[1]);
            Name.Generic[] gens = GenericsArray(nodes[3], true);
            // constructors have only type not name
            string name = nodes[2] == null ? return_type.name : ValueText(nodes[2]);
            return new AstNode.FuncDefHead(loc, name, return_type, gens, nodes[0] != null);
        });

        var PARAM = new Pattern("PARAM", new Grammar[] {TYPE, IDENTIFIER}, 
            (loc, nodes) => new AstNode.Param(loc, TypeNodeType(nodes[0]), ValueText(nodes[1]))
        );
        var PARAM_LIST = Multiple<AstNode.Param>("PARAM_LIST", new Grammar[] {PARAM, COMMA}, optional: true);
        var FUNC_DEF = new Pattern("FUNC_DEF", new Grammar[] {FUNC_DEF_HEAD, LPARAN, PARAM_LIST, RPARAN, LOCAL_BLOCK},
            (_, nodes) => 
                new AstNode.FuncDef((AstNode.FuncDefHead)nodes[0], (AstNode.Statements)nodes[4], MultiArray<AstNode.Param>(nodes[2]))
        );

        var TYPE_LIST = Multiple<AstNode.TypeNode>("TYPE_LIST", new Grammar[] {TYPE, COMMA}, optional: true);
        var IL_FUNC = new Pattern("IL_FUNC", 
            new Grammar[] {ILish, FUNC_DEF_HEAD, LPARAN, TYPE_LIST, RPARAN, STRING},
            (loc, nodes) => {
                string il = ValueText(nodes[5]);
                bool is_ils = ((AstNode.Tok)nodes[0]).token.type == Token.Type.ILS;
                Type[] paras = Array.ConvertAll(MultiArray<AstNode.TypeNode>(nodes[3]), n => n.type);
                return new AstNode.IlFunc(loc, (AstNode.FuncDefHead)nodes[1], il, paras, is_ils);
            }
        );

        var ALIAS = new Pattern("ALIAS", new Grammar[] {new Tok(Token.Type.ALIAS), IDENTIFIER, EQUAL, EXPRESSION}, 
            (loc, nodes) => new AstNode.Alias(loc, ValueText(nodes[1]), nodes[3])
        );

        var FIELD = new Pattern("FIELD", new Grammar[] {STATIC, MUT, TYPE, IDENTIFIER}, 
            (loc, nodes) => new AstNode.Field(loc, TypeNodeType(nodes[2]), ValueText(nodes[3]), nodes[1] != null, nodes[0] != null)
        );

        var CLASS_STMTS = NewStatements("CLASS_STMTS", new Grammar[] {FUNC_DEF, FIELD, ALIAS});
        var CLASS_BLOCK = NewBlock("CLASS_BLOCK", CLASS_STMTS);
        var CLASS_DEF = new Pattern("CLASS_DEF", new Grammar[] {CLASS, IDENTIFIER, GENERICS, INHERIT, CLASS_BLOCK},
            (loc, nodes) => new AstNode.Class(loc, ValueText(nodes[1]), (AstNode.Statements)nodes[4], MaybeValueText(nodes[3]), GenericsArray(nodes[2]))
        );

        // I don't think a non-ils il field makes sense
        var IL_FIELD = new Pattern("IL_FIELD", new Grammar[] {ILS, FIELD, STRING}, (_, nodes) => {
            var field = (AstNode.Field)nodes[1];
            field.il_name = ValueText(nodes[2]);
            return field;
        });

        var IL_CLASS_STMTS = NewStatements("IL_CLASS_STMTS", new Grammar[] {IL_FIELD, IL_FUNC, ALIAS});
        var IL_CLASS_BLOCK = NewBlock("IL_CLASS_BLOCK", IL_CLASS_STMTS);
        var IL_CLASS = new Pattern("IL_CLASS", new Grammar[] {IL, CLASS, IDENTIFIER, GENERICS, STRING, IL_CLASS_BLOCK}, (loc, nodes) => {
            string il = ValueText(nodes[4]);
            return new AstNode.IlClass(loc, ValueText(nodes[2]), il, (AstNode.Statements)nodes[5], null, GenericsArray(nodes[3]));
        });

        var INCLUDE = new Pattern("INCLUDE", new Grammar[] {new Tok(Token.Type.INCLUDE), STRING}, 
            (loc, nodes) => new AstNode.Include(loc, (AstNode.StringLiteral)nodes[1])
        );

        var GLOBAL_STATEMENTS = NewStatements("GLOBAL", new Grammar[] {INCLUDE, FUNC_DEF, IL_FUNC, ALIAS, CLASS_DEF, IL_CLASS});

        return new Pattern("PROGRAM", new Grammar[] {GLOBAL_STATEMENTS}, null);
    }

    public static AstNode.Program ParseTokens(List<Token> toks) {
        i = 0;
        tokens = toks;
        furthest_got = toks[0];
        List<AstNode> ast = PROGRAM.Parse();
        if (ast == null || i != tokens.Count) {
            Umi.Crash($"Expected `{furthest_expected}` but found `{furthest_got.type}`", furthest_got.location);
        }

        List<AstNode> statements = ((AstNode.Multiple<AstNode>)ast[0]).ToList();
        foreach (var include in statements.Where(s => s is AstNode.Include).Cast<AstNode.Include>()) {
            if (Path.GetExtension(include.path) == ".cs") {
                Output.cs_paths.Add(include.path);
                continue;
            }
            statements = ParseTokens(new Lexer(include.path).Lex()).global_statements.Concat(statements).ToList();
        }
        return new AstNode.Program(statements);
    }

}

abstract class AstNode {

    public readonly Location location;
    AstNode node_parent;

    public AstNode(Location loc) => location = loc;
    void NotImplemented() => throw new NotImplementedException(location.ToString());
    public virtual Type GetType(Scope scope) => Type.VOID;
    public virtual void CreateNameInfo(Scope scope) => NotImplemented();
    public virtual void GenIl(Scope scope) => NotImplemented();
    protected virtual void SetParent(AstNode parent) => node_parent = parent;
    protected virtual void DeclareVariables(Scope scope, List<string> local_var_types) {}

    // Returns null if there is no ancestor of that type
    public T GetAncestorOfType<T>() where T : AstNode {
        if (node_parent == null) return null;
        if (node_parent is T) return (T)node_parent;
        return node_parent.GetAncestorOfType<T>();
    }

    public void ExpectType(Type expected, Scope scope) {
        Type actual_type = GetType(scope);
        if (!scope.TypesMatch(expected, actual_type, location)) {
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
        // This should only be called once since it mutates list
        public List<T> ToList() {
            list.Reverse();
            return list;
        }
    }

    // For prefix and postfix operators
    public class Nothing : AstNode {
        public Nothing() : base(new Location(-1, -1, null, -1)) {}
        public override void GenIl(Scope _) {}
    }

    public abstract class Value : AstNode {
        public readonly string content;
        public Value(Tok tok) : base(tok.location) => content = tok.token.value;
    }

    public class StringLiteral : Value {
        public StringLiteral(Tok tok): base(tok) {}
        public override Type GetType(Scope _) => Type.STRING;
        public override void GenIl(Scope _) => Output.WriteLine($"ldstr \"{content}\"");
    }

    public class IntegerLiteral : Value {
        public IntegerLiteral(Tok tok): base(tok) {}
        public override Type GetType(Scope _) => Type.INT;
        public override void GenIl(Scope _) => Output.WriteLine($"ldc.i4 {content}");
    }

    public class BooleanLiteral : IntegerLiteral {
        public BooleanLiteral(Tok tok): base(tok) {}
        public override Type GetType(Scope _) => Type.BOOL;
    }

    public class CharLiteral : IntegerLiteral {
        public CharLiteral(Tok tok) : base(tok) {}
        public override Type GetType(Scope _) => Type.CHAR;
    }

    public class Identifier : Value {
        public Identifier(Tok tok): base(tok) {}

        public override string ToString() => $"Identifier with value `{content}`";

        public override Type GetType(Scope scope) {
            Name result = scope.LookUp(content, location);
            if (result is Name.Varish) return ((Name.Varish)result).GetType(scope);
            // this is here for static methods
            if (result is Name.Class) return new Type(((Name.Class)result).name);
            Umi.Crash($"Identifier was not a variable nor class", location);
            return null; // Make compiler happy
        }

        public override void GenIl(Scope scope) {
            Name result = scope.LookUp(content, location);
            // The if is for static methods
            if (result is Name.Varish) ((Name.Varish)result).GenLoadIl(scope, this);
        }
    }

    public class Generic : AstNode {
        public readonly string name;
        public readonly string constraint;

        public Generic(AstNode.Identifier name, string constraint) : base(name.location) {
            this.name = name.content;
            this.constraint = constraint;
        }
    }

    public class TypeNode : AstNode {
        public readonly Type type;
        public TypeNode(AstNode.Identifier name, string[] generics) : base(name.location) {
            type = new Type(name.content, generics);
        }
    }

    public class Param : AstNode {
        public readonly Type type;
        public readonly string name;
        public Param(Location loc, Type type, string name) : base(loc) {
            this.type = type;
            this.name = name;
        }
    }

    public class FuncCall : AstNode {
        readonly string name;
        readonly AstNode[] arguments;
        AstNode method_caller; // left of dot of method calls
        readonly Type[] generics;

        public FuncCall(Location loc, string name, AstNode[] args, Type[] gens = null) : base(loc) {
            this.name = name;
            arguments = args;
            generics = gens ?? new Type[0];
        }

        bool IsMethodCall() => method_caller != null;

        public FuncCall AsMethodCall(AstNode left_of_dot) {
            method_caller = left_of_dot;
            return this;
        }

        public override string ToString() {
            var str_args = Array.ConvertAll(arguments, a => a.ToString());
            return $"Function call `{name}` with arguments `{string.Join("`,`", str_args)}`";
        }

        protected override void DeclareVariables(Scope scope, List<string> local_var_types) {
            method_caller?.DeclareVariables(scope, local_var_types);
            foreach (var arg in arguments) arg.DeclareVariables(scope, local_var_types);
        }

        FuncInfo GetFuncInfo(Scope scope, Scope og_scope) {
            Name maybe_func;
            if (IsMethodCall()) {
                maybe_func = scope.GetClass(method_caller, method_caller.location).GetMember(name, location, scope);
            } else {
                maybe_func = scope.LookUp(name, location);
            }
            
            Name.Func func;
            if (maybe_func is Name.Func) func = (Name.Func)maybe_func;
            else if (maybe_func is Name.Class) func = ((Name.Class)maybe_func).constructor; 
            else {
                Umi.Crash("Tried to call something that is not a function", location);
                return null;
            }

            Type parent_instance = IsMethodCall() ? getParentInstance(og_scope) : null;
            Type[] arg_types = Array.ConvertAll(arguments, arg => arg.GetType(og_scope));
            FuncInfo func_info = func.BestFit(arg_types, generics, scope, location, parent_instance);

            if (func_info == null) {
                if (scope.parent != null) return GetFuncInfo(scope.parent, og_scope);

                string types = string.Join<Type>("`, `", arg_types);
                Umi.Crash($"Overload with types `{types}` does not exist for `{name}`", location);
            }

            return func_info;
        }

        FuncInfo GetFuncInfo(Scope scope) => GetFuncInfo(scope, scope);

        // The type of the left side of the dot of method calls
        Type getParentInstance(Scope scope) => method_caller.GetType(scope);

        public override Type GetType(Scope scope) {
            FuncInfo func_info = GetFuncInfo(scope);
            Type real_type = func_info.return_type.RealType(func_info.generics, generics);
            if (!IsMethodCall()) return real_type;

            Type parent_instance = getParentInstance(scope);
            var parent_class = scope.LookUpType(parent_instance.name, location);
            if (!(parent_class is Name.Class)) return real_type;
            return real_type.RealType(parent_instance, (Name.Class)parent_class);
        }

        protected override void SetParent(AstNode parent) {
            base.SetParent(parent);
            method_caller?.SetParent(this);
            foreach (var arg in arguments) arg.SetParent(this);
        }

        public override void GenIl(Scope scope) {
            FuncInfo func_info = GetFuncInfo(scope);

            // If calling a method without `this`
            if (!IsMethodCall() && func_info is FuncInfo.Ord) {
                if (func_info.parent_class != null && !func_info.is_constructor) {
                    Output.WriteLine("ldarg 0");
                }
            } else if (func_info is FuncInfo.Base) {
                Output.WriteLine("ldarg 0");
            }

            Type parent_instance = new Type(func_info.parent_class);

            if (func_info.is_constructor) {
                parent_instance = new Type(func_info.parent_class, Array.ConvertAll(generics, g => g.name));
            } else if (IsMethodCall()) {
                parent_instance = getParentInstance(scope);
            }

            scope.GenLoadIl(method_caller, parent_instance, IsMethodCall());
            foreach (var arg in arguments) arg.GenIl(scope);
            func_info.GenIl(scope, generics, parent_instance);
        }
    }
    
    public class TypeCast : AstNode {
        readonly Type type;
        readonly AstNode value;

        public TypeCast(string type, AstNode value, Location loc): base(loc) {
            this.type = new Type(type); // TODO: make it possible to typecast to types with generics
            this.value = value;
        }

        public override Type GetType(Scope _) => type;

        public override void GenIl(Scope scope) {
            value.GenIl(scope);
            Output.WriteLine($"castclass {scope.GetIlType(type, location)}");
        }
    }

    public class VarAssign : AstNode {
        public readonly string name;
        readonly AstNode value;

        public VarAssign(Location loc, string name, AstNode value) : base(loc) {
            this.name = name;
            this.value = value;
        }

        protected override void SetParent(AstNode parent) {
            base.SetParent(parent);
            value.SetParent(this);
        }

        protected override void DeclareVariables(Scope scope, List<string> local_var_types) {
            value.DeclareVariables(scope, local_var_types);
        }

        public override void GenIl(Scope scope) {
            Name maybe_var = scope.LookUp(name, location);
            if (!(maybe_var is Name.Varish)) Umi.Crash("Can only reassign local variables and fields", location);
            
            Name.Varish variable = (Name.Varish)maybe_var;
            variable.CheckCanAssign(this);
            value.ExpectType(variable.GetType(scope), scope);
            variable.GenStoreIl(scope, location, value);
        }
    }

    public class FieldAssign : AstNode {
        readonly Dot dot;
        readonly AstNode value;

        public FieldAssign(Location loc, Dot dot, AstNode value) : base(loc) {
            this.dot = dot;
            this.value = value;
        }

        protected override void SetParent(AstNode parent) {
            base.SetParent(parent);
            dot.SetParent(this);
            value.SetParent(this);
        }

        public override void GenIl(Scope scope) {
            Name.Class cls = dot.GetClass(scope);
            Name.Varish varish = (Name.Varish)dot.GetVarish(cls, scope);
            if (varish is Name.Alias) Umi.Crash("Can not reassign alias", location);

            Name.Field field = (Name.Field)varish;
            value.ExpectType(dot.GetType(scope), scope);
            field.CheckCanAssign(this);

            Type parent_type = dot.parent.GetType(scope);
            scope.GenLoadIl(dot.parent, parent_type, !field.is_static);
            value.GenIl(scope);
            field.GenStoreIl(cls.members, location, parent_type);
        }
    }
    
    public class Statements : AstNode {
        public readonly AstNode[] statements;
        public readonly Location end;

        public Statements(Location loc, AstNode[] statements, Location end): base(loc) {
            this.statements = statements;
            this.end = end;
        }

        protected override void SetParent(AstNode parent) {
            base.SetParent(parent);
            foreach (var stmt in statements) stmt.SetParent(parent);
        }

        public override Type GetType(Scope scope) => statements.Length == 0 ? Type.VOID : statements.Last().GetType(scope);

        public override void GenIl(Scope scope) { 
            foreach (var stmt in statements) stmt.GenIl(scope);
        }

        // Check everything except last statement is Void
        // Assumes the last statement will be checked elsewhere
        public void GenIlAndTypeCheck(Scope scope) {
            foreach (var stmt in statements.SkipLast(1)) stmt.ExpectType(Type.VOID, scope);
            GenIl(scope);
        }
    }

    // TODO: Make If, etc HAVE a Block not inherit it
    public abstract class Block : AstNode {
        public readonly Statements statements;
        protected Scope block_scope;
        public Block(Location loc, Statements statements) : base(loc) => this.statements = statements;

        // This mainly exists as a separate function for IfElse
        protected static Scope SubScope(Scope scope, List<string> local_var_types, AstNode[] stmts) {
            var new_scope = new Scope(scope);
            foreach (var stmt in stmts) stmt.DeclareVariables(new_scope, local_var_types);
            return new_scope;
        }

        protected override void DeclareVariables(Scope scope, List<string> local_var_types) {
            block_scope = SubScope(scope, local_var_types, statements.statements);
        }

        protected void GenStatements() => statements.GenIlAndTypeCheck(block_scope);
        
        protected override void SetParent(AstNode parent) {
            base.SetParent(parent);
            statements.SetParent(this);
        }

        public override Type GetType(Scope _) => statements.GetType(block_scope);
    }

    public class If : Block {
        public readonly AstNode condition;

        public If(Location loc, AstNode condition, Statements statements) : base(loc, statements) {
            this.condition = condition;
        }

        // The stuff that's common in If and IfElse and return the if_end label
        protected string IfIl(Scope scope) {
            condition.ExpectType(Type.BOOL, scope);
            condition.GenIl(scope);
            string if_end = statements.end.Label();
            Output.WriteLine($"brfalse {if_end}");
            GenStatements();
            return if_end;
        }

        protected override void SetParent(AstNode parent) {
            base.SetParent(parent);
            condition.SetParent(this);
        }

        public override void GenIl(Scope scope) {
            if (GetType(scope) != Type.VOID) Umi.Crash("else block is necessary when if statement is used as value", location);
            Output.WriteLabel(IfIl(scope));
        }
    }

    public class IfElse : If {
        public readonly Statements else_statements;
        Scope else_scope;

        public IfElse(Location loc, AstNode cond, Statements if_stmts, Statements else_stmts) : base(loc, cond, if_stmts) {
            else_statements = else_stmts;
        }

        protected override void DeclareVariables(Scope scope, List<string> local_var_types) {
            base.DeclareVariables(scope, local_var_types);
            else_scope = SubScope(scope, local_var_types, else_statements.statements);
        }

        public override Type GetType(Scope _) {
            if (else_statements.GetType(else_scope) != statements.GetType(block_scope)) {
                Umi.Crash("If and Else block must have the same return type", location);
            }
            return base.GetType(_);
        }

        public override void GenIl(Scope scope) {
            string if_end = IfIl(scope);
            string else_end = else_statements.end.Label();
            Output.WriteLine($"br {else_end}");
            Output.WriteLabel(if_end);
            else_statements.GenIlAndTypeCheck(else_scope);
            Output.WriteLabel(else_end);
        }
    }

    public class While : If {
        public While(Location loc, AstNode cond, Statements stmts) : base(loc, cond, stmts) {}

        public override void GenIl(Scope scope) {
            string cond_label = condition.location.Label();
            Output.WriteLabel(cond_label);
            string end = IfIl(scope);
            Output.WriteLine($"br {cond_label}");
            Output.WriteLabel(end);
        }
    }

    public class VarDef : AstNode {
        readonly Type type;
        readonly VarAssign assignment;
        readonly bool is_mutable;

        public VarDef(Location loc, Type type, VarAssign assignment, bool is_mutable) : base(loc) {
            this.type = type;
            this.assignment = assignment;
            this.is_mutable = is_mutable;
        }

        protected override void DeclareVariables(Scope new_scope, List<string> local_var_types) {
            new_scope.Add(assignment.name, new Name.Var(type, false, local_var_types.Count, location, is_mutable));
            local_var_types.Add(new_scope.GetIlType(type, location));
            assignment.DeclareVariables(new_scope, local_var_types);
        }

        protected override void SetParent(AstNode parent) {
            base.SetParent(parent);
            assignment.SetParent(this);
        }

        public override void GenIl(Scope scope) => assignment.GenIl(scope);
    }

    public class Return : AstNode {
        readonly AstNode expression;
        public Return(Location loc, AstNode expression): base(loc) => this.expression = expression;

        protected override void SetParent(AstNode parent) {
            base.SetParent(parent);
            expression.SetParent(this);
        }
        
        public override void GenIl(Scope scope) {
            if (expression != null) {
                expression.ExpectType(GetAncestorOfType<AstNode.FuncDef>().GetType(scope), scope);
                expression.GenIl(scope);
            }
            Output.WriteLine("ret");
        }
    }

    public abstract class Goto : AstNode {
        public Goto(Location loc): base(loc) {}

        protected abstract Location LabelLoc(AstNode.While w);

        public override void GenIl(Scope scope) {
            AstNode.While w = GetAncestorOfType<AstNode.While>();
            if (w == null) Umi.Crash("No enclosed loop to break/continue out of", location);
            Output.WriteLine($"br {LabelLoc(w).Label()}");
        }
    } 

    public class Break : Goto {
        public Break(Location loc): base(loc) {}
        protected override Location LabelLoc(While w) => w.statements.end;
    }

    public class Continue : Goto {
        public Continue(Location loc): base(loc) {}
        protected override Location LabelLoc(While w) => w.condition.location;
    }

    string ParentClassName() => node_parent is AstNode.IlClass ? ((AstNode.IlClass)node_parent).name : null;

    void AddToScope(Scope scope, string parent_class, FuncInfo func_info, string name, Type return_type, bool is_static) {
        if (name == return_type.name) { // Add constructor to the class
            if (!is_static) { // Ignore static constructors
                var c = (Name.Class)scope.LookUp(parent_class, location);
                func_info.generics = c.generics;
                c.constructor.functions.Add(func_info);
            }
        } else { // Add the function to the scope
            scope.AddFunction(location, name, func_info);
        }
    }

    public class FuncDefHead : AstNode {
        public readonly string name;
        public readonly Type return_type;
        public readonly Name.Generic[] generics;
        public readonly bool is_static;

        public FuncDefHead(Location loc, string name, Type rt, Name.Generic[] gens, bool is_static = true) : base(loc) {
            this.name = name;
            return_type = rt;
            generics = gens;
            this.is_static = is_static;
        }
    }

    public class FuncDef : Block {
        public readonly string name;
        readonly Type return_type;
        readonly Param[] parameters;
        readonly Name.Generic[] generics;
        bool is_static;

        public FuncInfo.Ord func_info;
        readonly List<string> local_var_types = new List<string>();

        public FuncDef(FuncDefHead func_def_head, Statements stmts, Param[] paras): base(func_def_head.location, stmts) {
            is_static = func_def_head.is_static;
            name = func_def_head.name;
            return_type = func_def_head.return_type;
            generics = func_def_head.generics;
            parameters = paras;
        }

        public override void CreateNameInfo(Scope scope) {
            string parent_class = ParentClassName();
            if (parent_class == null) is_static = true;

            // Generics
            Scope generics_scope = new Scope(scope);
            foreach (var gen in generics) generics_scope.Add(gen.name, gen);

            func_info = new FuncInfo.Ord(parameters, return_type, is_static, name, location, parent_class, generics, generics_scope);
            AddToScope(scope, parent_class, func_info, name, return_type, is_static);
            DeclareVariables(generics_scope, local_var_types);
            
            List<Param> paras = parameters.ToList();
            if (!is_static) {
                paras.Insert(0, new Param(location, new Type(parent_class, ((IlClass)node_parent).generics), "this"));
            }

            // Handle prefix operator functions
            if (paras.Count == 2 && paras[0].type == Type.VOID) {
                Param p = paras[1];
                block_scope.Add(p.name, new Name.Var(p.type, true, 0, p.location));
            } else for (int i = 0; i < paras.Count; i++) {
                Param p = paras[i];
                var par = new Name.Var(p.type, true, i, p.location);
                block_scope.Add(p.name, par);
            }
        }

        public override void GenIl(Scope _) {
            Output.WriteLine($".method {(is_static ? "static " : "virtual ")}{func_info.IlSignature()}");
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
            
            statements.ExpectType(return_type.name == name ? Type.VOID : return_type, block_scope);
            GenStatements();
            Output.WriteLine("ret");
            Output.Unindent();
            Output.WriteLine("}\n");
        }
    }

    public class IlFunc : AstNode {
        readonly FuncDefHead head;
        readonly string il;
        readonly Type[] param_types;
        readonly bool is_ils;

        public IlFunc(Location loc, FuncDefHead func_def_head, string il, Type[] param_types, bool is_ils) : base(loc) {
            head = func_def_head;
            this.il = il;
            this.param_types = param_types;
            this.is_ils = is_ils;
        }

        public override void CreateNameInfo(Scope scope) {
            string parent_class = ParentClassName();
            bool is_static = head.is_static;
            if (parent_class == null) is_static = true;

            bool is_constructor = head.return_type.name == head.name;
            FuncInfo func_info;
            if (is_ils) func_info = new FuncInfo.Ord(param_types, head.return_type, is_static, il, location, parent_class, head.generics, scope, is_constructor);
            else func_info = new FuncInfo.Il(param_types, head.return_type, il, head.generics, parent_class, location, is_constructor);
            AddToScope(scope, parent_class, func_info, head.name, head.return_type, is_static);
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

        public Name.Class GetClass(Scope scope) => (Name.Class)scope.GetClass(parent, location);

        public Name.Varish GetVarish(Name.Class cls, Scope scope) => (Name.Varish)cls.GetMember(child, location, scope);
        
        public override Type GetType(Scope scope) {
            Type parent_instance = parent.GetType(scope);
            var parent_class = (Name.Class)scope.LookUpType(parent_instance.name, location);
            var field = (Name.Varish)parent_class.GetMember(child, location, scope);
            return field.GetType(scope).RealType(parent_instance, parent_class);
        }

        protected override void SetParent(AstNode node_parent) {
            base.SetParent(node_parent);
            parent.SetParent(this);
        }

        public override void GenIl(Scope scope) {
            Name.Class cls = GetClass(scope);
            Name.Varish v = GetVarish(cls, scope);
            if (v is Name.Alias) {
                v.GenLoadIl(scope, null);
                return;
            }

            var field = (Name.Field)v;
            Type parent_type = parent.GetType(scope);
            scope.GenLoadIl(parent, parent_type, !field.is_static);
            field.GenLoadIl(cls.members, location, parent_type);
        }
    }

    public class Field : AstNode {
        public readonly Type type;
        public readonly string name;
        public string il_name; // for ils fields
        readonly bool is_mutable;
        readonly bool is_static;

        public Field(Location loc, Type type, string name, bool is_mutable = false, bool is_static = false) : base(loc) {
            this.type = type;
            this.name = name;
            this.is_mutable = is_mutable;
            this.is_static = is_static;
        }

        public override void CreateNameInfo(Scope scope) {
            if (il_name == null) il_name = name;

            var cls = (AstNode.IlClass)node_parent;
            scope.Add(name, new Name.Field(location, type, name, new Type(cls.name, cls.generics), is_mutable, is_static, il_name));
        }

        public override void GenIl(Scope scope) {
            Output.WriteLine($".field{(is_static ? " static" : "")} {scope.GetIlType(type, location)} {name}");
        }
    }

    public class IlClass : AstNode {
        public readonly string name;
        public readonly Name.Generic[] generics;
        readonly string il_name;
        protected string base_class;
        protected readonly Statements statements;
        protected Scope class_scope;

        public IlClass(Location loc, string name, string il_name, Statements statements, string base_class, Name.Generic[] generics) : base(loc) {
            this.name = name;
            this.il_name = il_name;
            this.statements = statements;
            this.base_class = base_class;
            this.generics = generics;
        }

        protected override void SetParent(AstNode parent) {
            base.SetParent(parent);
            statements.SetParent(this);
        }

        public override void CreateNameInfo(Scope scope) {
            class_scope = new Scope(scope);
            scope.Add(name, new Name.Class(location, name, class_scope, il_name, base_class, generics));

            foreach (var gen in generics) class_scope.Add(gen.name, gen);
            foreach (AstNode stmt in statements.statements) stmt.CreateNameInfo(class_scope);

            // Copy the scope of the base class into this class
            // * Note that this means the base class must come before this class
            if (base_class != null) {
                Name base_name = scope.LookUp(base_class, location);
                if (!(base_name is Name.Class)) Umi.Crash("Can only inherit from classes", location);
                var base_class_name = (Name.Class)base_name;

                // `base`
                foreach (var func_info in base_class_name.constructor.functions) {
                    class_scope.AddFunction(location, "base", new FuncInfo.Base(func_info.param_types, base_class, location));
                }

                foreach (var entry in base_class_name.members.names) class_scope.names.TryAdd(entry.Key, entry.Value);
            }
        }

        public override void GenIl(Scope _) {}
    }

    public class Class : IlClass {
        public Class(Location loc, string name, Statements statements, string bas, Name.Generic[] generics) 
            : base(loc, name, ilName(name, generics), statements, bas, generics) {}

        public override void GenIl(Scope scope) {
            string extends = base_class == null ? "" : $" extends {scope.GetIlType(new Type(base_class), location)}";
            Output.WriteLine($".class {name}{Name.Generic.GenericsString(generics)}{extends}");
            Output.WriteLine("{");
            Output.Indent();
            statements.GenIl(class_scope);
            Output.Unindent();
            Output.WriteLine("}\n");
        }

        static string ilName(string name, Name.Generic[] generics) {
            if (generics.Length == 0) return name;
            var gens = string.Join(", ", Enumerable.Range(0, generics.Length).Select(n => $"{{{n}}}"));
            return $"class {name}<{gens}>";
        }
    }

    public class Include : AstNode {
        public string path;
        public Include(Location loc, AstNode.StringLiteral path_node) : base(loc) => path = path_node.content;
        public override void CreateNameInfo(Scope _) {}
        public override void GenIl(Scope _) {}
    }

    public class Program : AstNode {
        public readonly List<AstNode> global_statements;

        public Program(List<AstNode> s) : base(s[0].location) => global_statements = s;

        public override void CreateNameInfo(Scope scope) {
            foreach(AstNode statement in global_statements) {
                statement.SetParent(this);
                statement.CreateNameInfo(scope);
            }
        }

        public override void GenIl(Scope scope) {
            File.Delete("output.il");
            Output.WriteLine(@".assembly extern System.Core
{
  .ver 4:0:0:0
  .publickeytoken = (B7 7A 5C 56 19 34 E0 89 ) // .z\V.4..
}
.assembly extern mscorlib
{
  .ver 4:0:0:0
  .publickeytoken = (B7 7A 5C 56 19 34 E0 89 ) // .z\V.4..
}");
            Output.WriteLine(".assembly UmiProgram {}\n");
            Output.WriteCsIl();
            foreach(AstNode statement in global_statements) statement.GenIl(scope);
        }
    }

}

class Type {
    public readonly string name;
    public readonly string[] generics; // TODO: change to Type[]

    public Type(string name, string[] generics) {
        this.name = name;
        this.generics = generics;
    }

    public Type(string name) : this(name, new string[0]) {}

    public Type(string name, Name.Generic[] generics) : this(name, Array.ConvertAll(generics, g => g.name)) {}

    // `this` is the type used in the generic class/function
    // `generic_params` is in the definition of the generic class/function
    // `generic_args` is what is supplied to the generic class/function
    public Type RealType(Name.Generic[] generic_params, Type[] generic_args) {
        int i = Array.FindIndex(generic_params, g => g.name == name);
        Type result = i == -1 ? this : generic_args[i];
        // ?  I think this might suck
        var gens = Array.ConvertAll(result.generics, g => new Type(g).RealType(generic_params, generic_args).name);
        return new Type(result.name, gens);
    }

    public Type RealType(Type parent_instance, Name.Class parent_class) {
        return RealType(parent_class.generics, Array.ConvertAll(parent_instance.generics, g => new Type(g)));
    }

    public static bool operator ==(Type a, Type b) => a.name == b.name && Enumerable.SequenceEqual(a.generics, b.generics);
    public static bool operator !=(Type a, Type b) => !(a == b);

    public override string ToString() {
        string str = name;
        if (generics.Length > 0) str += $"[{string.Join(", ", generics)}]";
        return str;
    }

    public override bool Equals(object obj) {
        if (!(obj is Type)) throw new NotImplementedException("Equality doesn't exist for non `Type`s");
        return this == (Type)obj;
    }

    public override int GetHashCode() => throw new NotImplementedException("Don't call GetHashCode on Type");
    
    public static Type VOID = new Type("Void");
    public static Type STRING = new Type("String");
    public static Type INT = new Type("Int");
    public static Type CHAR = new Type("Char");
    public static Type BOOL = new Type("Bool");
    public static Type STRUCT = new Type("Struct");
}

class Output {
    static int indentation = 0;
    // With chains of if else, they all go to the end so they create duplicate labels
    static HashSet<string> labels = new HashSet<string>();
    // For compiling C# files later
    public static readonly List<string> cs_paths = new List<string>();

    public static void WriteLine(string il_code) {
        // TODO allow the output file to be changed
        File.AppendAllText("output.il", new string(' ', indentation) + il_code + "\n");
    }

    public static void WriteLabel(string label) {
        // Prevent duplicates
        if (!labels.Contains(label)) {
            Output.WriteLine($"{label}:");
            labels.Add(label);
        }
    }

    // Compile the C# files and then put the il into the output
    public static void WriteCsIl() {
        foreach (string path in cs_paths) {
            Umi.RunCommand("mcs", "-unsafe -out:temp_cs.dll -target:library " + path);
            Umi.RunCommand("monodis", "--output=temp_cs.il temp_cs.dll");
            // ? not sure whether this will always work tbh with the version
            string necessary_il = File.ReadAllText("temp_cs.il").Split(".ver  0:0:0:0\n}")[1];
            WriteLine(necessary_il);
        }
        File.Delete("temp_cs.dll");
        File.Delete("temp_cs.il");
    }

    public static void Indent() => indentation += 4;
    public static void Unindent() => indentation -= 4;
}

abstract class Name {

    public readonly Location defined_at;

    Name(Location defined_at) => this.defined_at = defined_at;

    public class Generic : Name {
        public readonly string name;
        public readonly string il_name;
        public readonly string base_class;

        public Generic(Location loc, string name, string il_name, string base_class) : base(loc) {
            this.name = name;
            this.il_name = il_name;
            this.base_class = base_class;
        }

        public Generic(Location loc, string name, int index, string constraint, bool func) 
        : this(loc, name, genericIlName(index, func), constraint) {}

        public virtual Name GetMember(string child, Location location, Scope scope) {
            if (base_class == null) Umi.Crash($"Generic parameter of type `{name}` with no constraints has no members", location);
            return scope.LookUpType(base_class, location).GetMember(child, location, scope);
        }

        // TODO: this won't work when inheriting from generic classes or il classes
        public override string ToString() => (base_class != null ? $"(class {base_class}) " : "") + name;
        
        public static string GenericsString(Generic[] generics) {
            return generics.Length == 0 ? "" : $"<{string.Join<Name.Generic>(',', generics)}>";
        }

        static string genericIlName(int index, bool is_func) => is_func ? $"!!{index}" : $"!{index}";
    }

    public class Class : Generic {
        public readonly Func constructor;
        public readonly Scope members;
        public readonly Name.Generic[] generics;

        public Class(Location loc, string name, Scope members, string il_name, string base_class, Name.Generic[] generics) 
        : base(loc, name, il_name, base_class) {
            this.members = members;
            constructor = new Func(loc);
            this.generics = generics;
        }

        public override Name GetMember(string child, Location location, Scope _) {
            Name member;
            if (!members.names.TryGetValue(child, out member)) {
                Umi.Crash($"`{child}` does not exist in the `{name}` class", location);
            }
            return member;
        }
    }

    public class Func : Name {
        // The various definitions for the different overloads
        public readonly List<FuncInfo> functions = new List<FuncInfo>();
        public Func(Location defined_at) : base(defined_at) {}

        // This function is currently a misnomer
        // It returns the *first* FuncInfo that matches
        // Returns null if nothing matches
        public FuncInfo BestFit(Type[] types, Type[] gen_types, Scope scope, Location loc, Type parent_instance) {
            foreach (var func_info in functions) {
                if (func_info.param_types.Length != types.Length) continue;
                if (gen_types.Length != func_info.generics.Length) continue;

                Type[] correct_types = new Type[func_info.param_types.Length];
                func_info.param_types.CopyTo(correct_types, 0);
                
                for (int g = 0; g < func_info.generics.Length; g++) {
                    Generic gen = func_info.generics[g];
                    // supplied generics don't fit
                    if (gen.base_class != null && !scope.TypesMatch(new Type(gen.base_class), gen_types[g], loc)) break;

                    for (int t = 0; t < types.Length; t++) {
                        if (gen.name == func_info.param_types[t].name) correct_types[t] = gen_types[g];
                    }
                }

                // Class Generics
                if (!object.ReferenceEquals(parent_instance, null)) {
                    var parent_class = scope.LookUpType(parent_instance.name, loc) as Name.Class;
                    if (parent_class != null) {
                        for (int g = 0; g < parent_class.generics.Length; g++) {
                            Generic gen = parent_class.generics[g];
                            for (int t = 0; t < types.Length; t++) {
                                if (gen.name == func_info.param_types[t].name) correct_types[t] = new Type(parent_instance.generics[g]);
                            }
                        }
                    }
                }

                if (types.Zip(correct_types, (t, ct) => scope.TypesMatch(ct, t, loc)).All(b => b)) return func_info;
            }
            return null;
        }
    }

    // Var or Alias
    public abstract class Varish : Name {
        public Varish(Location defined_at) : base(defined_at) {}
        public abstract Type GetType(Scope scope);
        public abstract void GenLoadIl(Scope scope, AstNode.Identifier context);
        public abstract void GenAddressIl(Scope scope, AstNode.Identifier context);
        public abstract void CheckCanAssign(AstNode assignment);
        public virtual void GenStoreIl(Scope scope, Location loc, AstNode value) => value.GenIl(scope);
    }

    public class Var : Varish {
        public readonly Type type;
        public readonly int index;
        readonly bool is_param; // local variable or parameter
        readonly bool is_mutable;

        public Var(Type type, bool is_param, int index, Location defined_at, bool mutable = false) : base(defined_at) {
            this.type = type;
            this.is_param = is_param;
            this.index = index;
            is_mutable = mutable;
        }

        public override void CheckCanAssign(AstNode assignment) {
            Location assign_loc = assignment.location;
            if (defined_at > assign_loc) {
                string name = ((AstNode.VarAssign)assignment).name;
                Umi.Crash($"Can not assign to variable `{name}` before it is defined", assign_loc);
            }
            if (!is_mutable && assign_loc.line > defined_at.line) {
                Umi.Crash($"Can not reassign immutable variables", assign_loc);
            }
        }

        public override Type GetType(Scope _) => type;

        void GenLoadIl(AstNode.Identifier context, string arg, string local) {
            if (defined_at > context.location) {
                Umi.Crash($"Can not use variable `{context.content}` before it is defined", context.location);
            }
            Output.WriteLine($"{(is_param ? arg : local)} {index}");
        }

        public override void GenLoadIl(Scope _, AstNode.Identifier context) => GenLoadIl(context, "ldarg", "ldloc");
        
        public override void GenAddressIl(Scope scope, AstNode.Identifier context) => GenLoadIl(context, "ldarga", "ldloca");

        public override void GenStoreIl(Scope scope, Location loc, AstNode value) {
            base.GenStoreIl(scope, loc, value);
            Output.WriteLine($"stloc {index}");
        }
    }

    public class Alias : Varish {
        public AstNode node;
        public Alias(Location defined_at, AstNode node) : base(defined_at) => this.node = node;
        public override Type GetType(Scope scope) => node.GetType(scope);
        public override void GenLoadIl(Scope scope, AstNode.Identifier _) => node.GenIl(scope);
        public override void GenAddressIl(Scope _, AstNode.Identifier ctx) => Umi.Crash("Aliases do not have an address", ctx.location);
        public override void CheckCanAssign(AstNode a) => Umi.Crash("Can not reassign alias", a.location);
    }

    public class Field : Varish {
        readonly Type type;
        public readonly bool is_mutable;
        readonly string name;
        readonly string il_name;
        readonly Type parent_class; // only use the generics when used without `this`
        public readonly bool is_static;

        public Field(Location defined_at, Type type, string name, Type pc, bool mutable, bool stati, string il_name) : base(defined_at) {
            this.type = type;
            this.name = name;
            this.parent_class = pc;
            is_mutable = mutable;
            is_static = stati;
            this.il_name = il_name;
        }

        public override void CheckCanAssign(AstNode assignment) {
            if (is_mutable) return;
            FuncInfo.Ord func_info = assignment.GetAncestorOfType<AstNode.FuncDef>().func_info;
            if (func_info.is_constructor && func_info.parent_class == parent_class.name) return;
            Umi.Crash($"Cannot reassign immutable member `{parent_class}.{name}` outside of constructor", assignment.location);
        }

        public override Type GetType(Scope _) => type;

        void GenInstructionIl(Scope scope, Location loc, string instruction, Type parent) {
            Output.WriteLine($"{instruction} {scope.GetIlType(type, loc)} {scope.GetIlType(parent, loc)}::{il_name}");
        }

        public void GenLoadIl(Scope scope, Location loc, Type parent) => GenInstructionIl(scope, loc, is_static ? "ldsfld" : "ldfld", parent);
        
        public void GenStoreIl(Scope scope, Location loc, Type parent) => GenInstructionIl(scope, loc, is_static ? "stsfld" : "stfld", parent);

        public override void GenStoreIl(Scope scope, Location loc, AstNode value) {
            // `this`. This function should only be called when treated like normal var assign
            if (!is_static) Output.WriteLine("ldarg 0");
            base.GenStoreIl(scope, loc, value);
            GenStoreIl(scope, loc, parent_class);
        }

        // This should only be called when the field is used without `this` in its class
        public override void GenLoadIl(Scope scope, AstNode.Identifier context) {
            if (!is_static) Output.WriteLine("ldarg 0");
            GenLoadIl(scope, context.location, parent_class);
        }

        // TODO: check if this works (most likely doesn't)
        public override void GenAddressIl(Scope scope, AstNode.Identifier context) {
            GenInstructionIl(scope, context.location, "ldflda", parent_class);
        }
    }

}

abstract class FuncInfo {
    public readonly Type[] param_types;
    public readonly Type return_type;
    public readonly string parent_class;
    public Name.Generic[] generics; // can be modified because constructors
    public readonly bool is_constructor;
    readonly Location defined_at;
    
    public FuncInfo(Type[] param_types, Type return_type, Name.Generic[] generics, string parent_class, Location loc, bool constructor) {
        this.param_types = param_types;
        this.return_type = return_type;
        this.generics = generics;
        this.parent_class = parent_class;
        this.defined_at = loc;
        this.is_constructor = constructor;
    }

    public abstract void GenIl(Scope scope, Type[] gens, Type parent_instance);

    // Ordinary function
    public class Ord : FuncInfo {
        readonly bool is_static;
        readonly string il_name;
        readonly Scope scope;

        public Ord(Type[] paras, Type rtype, bool is_static, string name, Location loc, string pclass, Name.Generic[] gens, Scope scope, bool constructor)
        : base(paras, rtype, gens, pclass, loc, constructor) {
            this.is_static = is_static;
            il_name = name;
            this.scope = scope;
        }

        public Ord(AstNode.Param[] paras, Type rtype, bool is_static, string name, Location loc, string parent_class, Name.Generic[] gens, Scope scope)
            : this(Array.ConvertAll(paras, p => p.type), rtype, is_static, IlName(name, rtype, paras, is_static), loc, parent_class, gens, scope, name == rtype.name) {}

        static string IlName(string name, Type return_type, AstNode.Param[] parameters, bool is_static) {
            if (name == return_type.name) return is_static ? ".cctor" : ".ctor"; // constructor   
            if (parameters.Length == 2) { // unary operator
                if (parameters[0].type == Type.VOID) return $"'PREFIX{name}'";
                if (parameters[1].type == Type.VOID) return $"'POSTFIX{name}'";
            }
            return $"'{name}'";
        }

        // `gens` is the passed in generics whereas `generics` is the type parameters 
        public string IlSignature(Type[] gens = null, string prefix = "") {
            string rt = scope.GetIlType(return_type, defined_at);
            if (is_constructor) rt = "void";

            Type[] pts = (Type[])param_types.Clone(); 
            if (pts.Length == 2) { // unary operator
                if (pts[0] == Type.VOID) pts = new Type[] {pts[1]};
                else if (pts[1] == Type.VOID) pts = new Type[] {pts[0]};
            }
            string para_types = scope.ParameterTypesString(pts, defined_at);

            string generic = Name.Generic.GenericsString(generics);
            if (gens != null && gens.Length > 0) generic = $"<{scope.ParameterTypesString(gens, defined_at)}>";

            return $"{rt} {prefix}{il_name}{generic}({para_types})";
        }

        public override void GenIl(Scope called_scope, Type[] gens, Type parent_instance) {
            string call_word = is_static ? "call" : "callvirt";
            if (is_constructor) call_word = "newobj";
            
            string prefix = parent_class == null ? "" : called_scope.GetIlType(parent_instance, defined_at) + "::";
            Output.WriteLine($"{call_word} {IlSignature(gens, prefix)}");
        }
    }

    // TODO: static constructors' base
    // Calling the base constructor within a constructor
    public class Base : FuncInfo {
        public Base(Type[] ptypes, string parent_class, Location loc)
        : base(ptypes, Type.VOID, new Name.Generic[0], parent_class, loc, false) {}

        public override void GenIl(Scope scope, Type[] _, Type __) {
            string paras = scope.ParameterTypesString(param_types, defined_at);
            Output.WriteLine($"call instance void {parent_class}::.ctor({paras})");
        }
    }

    public class Il : FuncInfo {
        readonly string il;

        public Il(Type[] ptypes, Type rtype, string il, Name.Generic[] gens, string parent_class, Location loc, bool is_constructor)
        : base(ptypes, rtype, gens, parent_class, loc, is_constructor) => this.il = il;
        
        public override void GenIl(Scope scope, Type[] __, Type parent_instance) {
            Output.WriteLine(scope.InsertGenerics(il, parent_instance.generics, defined_at));
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

    public Name.Generic LookUpType(string type, Location location) {
        Name name = LookUp(type, location);
        if (!(name is Name.Generic)) Umi.Crash($"`{type}` is not a type", location);
        return (Name.Generic)name;
    }

    public Name.Generic GetClass(AstNode parent, Location location) {
        return LookUpType(parent.GetType(this).name, location);
    }

    string GetIlType(string type, Location location) => LookUpType(type, location).il_name;

    public string InsertGenerics(string insertee, string[] generics, Location location) {
        return string.Format(insertee, Array.ConvertAll(generics, g => GetIlType(g, location)));
    }

    public string GetIlType(Type type, Location location) {
        return InsertGenerics(GetIlType(type.name, location), type.generics, location);
    }
    
    public string ParameterTypesString(Type[] param_types, Location location) {
        return string.Join(", ", Array.ConvertAll(param_types, p => GetIlType(p, location)));
    }

    // TODO: probably allow generic parts of type to not be exactly same but research first
    public bool TypesMatch(Type expected, Type actual, Location loc) {
        if (expected == actual) return true;
        var actual_class = (Name.Generic)LookUpType(actual.name, loc);
        if (actual_class.base_class == null) return false;
        return TypesMatch(expected, new Type(actual_class.base_class), loc);
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

    // ldloc, ldloca, ldarg, ldarga, etc. for fields and methods
    public void GenLoadIl(AstNode parent, Type parent_type, bool do_load) {
        if (!do_load) return;

        if(!TypesMatch(Type.STRUCT, parent_type, parent.location)) {
            parent.GenIl(this);
            return;
        }

        if (!(parent is AstNode.Identifier)) Umi.Crash("Using struct fields can only be done on variables", parent.location);
        
        var identifier = (AstNode.Identifier)parent;
        Name p = LookUp(identifier.content, identifier.location);
        if (!(p is Name.Varish)) return;
        ((Name.Varish)p).GenAddressIl(this, identifier);
    }
}

class Umi {

    static bool DEBUG = false;

    public static void Exit(string message) {
        Console.WriteLine(message);
        Environment.Exit(1);
    }

    public static void Crash(string message, Location loc) {
        if (DEBUG) throw new Exception(loc + ": " + message);
        Exit(loc + ": " + message);
    }

    public static void RunCommand(string command, string args) {
        Process process = Process.Start(command, args);
        process.WaitForExit();
        if (process.ExitCode != 0) Environment.Exit(1);
    }

    static void Main(string[] args) {
        string path = "";
        bool RUN = false;

        foreach (string arg in args) {
            if (arg[0] != '-') {
                if (path != "") Exit("Only supply one umi file");
                path = arg;
                continue;
            }
            switch (arg) {
                case "-d":
                    DEBUG = true;
                    break;
                case "-r":
                    RUN = true;
                    break;
                default:
                    Exit($"Unrecognised command line option `{arg}`");
                    break;
            }
        }

        if (path == "") Exit("You must specify the path to the file");

        List<Token> tokens = new Lexer("std.umi").Lex();
        tokens = new Lexer(path).Lex(tokens);
        AstNode.Program ast = Grammar.ParseTokens(tokens);

        Scope global_namespace = new Scope(null);
        ast.CreateNameInfo(global_namespace);
        ast.GenIl(global_namespace);

        RunCommand("ilasm", "-quiet output.il");
        if (!DEBUG) File.Delete("output.il");
        if (RUN) RunCommand("mono", "output.exe");
    }

}
