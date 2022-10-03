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
        NEWLINE, COMMA, EQUAL, COLON,
        IL, ILF, ALIAS, CLASS, MUT, STATIC,
        IF, ELSE, WHILE,
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
                    case "ilf":
                        type = Token.Type.ILF;
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
        return new Pattern(name + "S+NL", new Grammar[] {ONL, statements, ONL}, (_, nodes) => nodes[1], optional: true);
    }

    static Pattern NewBlock(string name, Pattern statements) {
        return new Pattern(name, new Grammar[] {LCURLY, statements, RCURLY}, (loc, nodes) => {
            if (nodes[1] == null) return new AstNode.Statements(loc, new AstNode[0], nodes[2].location);
            return new AstNode.Statements(loc, MultiArray(nodes, 1), nodes[2].location);
        });
    }

    // Shorthands
    static AstNode[] MultiArray(List<AstNode> nodes, int i) => ((AstNode.Multiple<AstNode>)nodes[i]).ToArray();
    static string ValueText(AstNode node) => ((AstNode.Value)node).content;
    
    static string[] IlParamTypes(AstNode type_node) {
        if (type_node == null) return new string[0]; 
        AstNode.Identifier[] values = ((AstNode.Multiple<AstNode.Identifier>)type_node).ToArray();
        return Array.ConvertAll(values, v => v.content);
    }

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
        var COMMA = new Tok(Token.Type.COMMA);
        var COLON = new Tok(Token.Type.COLON);
        var EQUAL = new Tok(Token.Type.EQUAL);
        var MUT = new Tok(Token.Type.MUT, optional: true);
        var STATIC = new Tok(Token.Type.STATIC, optional: true);
        var CLASS = new Tok(Token.Type.CLASS);
        var IL = new Tok(Token.Type.IL);
        var ILF = new Tok(Token.Type.ILF);
        var IF = new Tok(Token.Type.IF);
        var ELSE = new Tok(Token.Type.ELSE);

        var ILish = OneOf("ILish", new Grammar[] {IL, ILF});

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
        var TERM = OneOf("TERM", new Grammar[] {STRING, CHAR, INTEGER, BOOLEAN, null, IDENTIFIER, SUBEXPRESSION});

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

        var ARGUMENTS = Multiple<AstNode>("ARGUMENTS", new Grammar[] {EXPRESSION, COMMA}, optional: true);
        var FUNC_CALL = new Pattern("FUNC_CALL", new Grammar[] {IDENTIFIER, LPARAN, ARGUMENTS, RPARAN}, (loc, nodes) => {
            AstNode[] args = nodes[2] == null ? new AstNode[0] : MultiArray(nodes, 2);
            return new AstNode.FuncCall(loc, ValueText(nodes[0]), args);
        });
        TERM.possible_patterns[4][0] = FUNC_CALL;

        var VAR_ASSIGN = new Pattern("VAR_ASSIGN", new Grammar[] {EXPRESSION, EQUAL, EXPRESSION}, (loc, nodes) => {
            if (nodes[0] is AstNode.Identifier) return new AstNode.VarAssign(loc, ValueText(nodes[0]), nodes[2]);
            if (nodes[0] is AstNode.Dot) return new AstNode.FieldAssign(loc, (AstNode.Dot)nodes[0], nodes[2]);
            return null; // If you Umi.Crash() here, the parser breaks
        });

        var VAR_DEF = new Pattern("VAR_DEF", new Grammar[] {MUT, IDENTIFIER, VAR_ASSIGN}, (loc, nodes) => {
            string type = ValueText(nodes[1]);
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
        var FUNC_IDEN = OneOf("FUNC_IDEN", new Grammar[] {IDENTIFIER, OPERATOR});

        var PARAM = new Pattern("PARAM", new Grammar[] {IDENTIFIER, IDENTIFIER}, 
            (loc, nodes) => new AstNode.Param(loc, ValueText(nodes[0]), ValueText(nodes[1]))
        );
        var PARAM_LIST = Multiple<AstNode.Param>("PARAM_LIST", new Grammar[] {PARAM, COMMA}, optional: true);
        var FUNC_DEF = new Pattern("FUNC_DEF", 
            new Grammar[] {IDENTIFIER, FUNC_IDEN, LPARAN, PARAM_LIST, RPARAN, LOCAL_BLOCK},
            (loc, nodes) => {
                string return_type = ValueText(nodes[0]);
                string name = ValueText(nodes[1]);
                AstNode.Param[] parameters = null;
                if (nodes[3] != null) parameters = ((AstNode.Multiple<AstNode.Param>)nodes[3]).ToArray();
                return new AstNode.FuncDef(loc, true, return_type, name, (AstNode.Statements)nodes[5], parameters);
            }
        );

        var FUNC_TYPE_NAME = new Pattern("FUNC_TYPE_NAME", new Grammar[] {IDENTIFIER, FUNC_IDEN.NewOptional("OPT_IDEN")}, 
            (loc, nodes) => {
                if (nodes[1] == null) nodes[1] = nodes[0];
                // Maybe a Field shouldn't be used
                return new AstNode.Field(loc, ValueText(nodes[0]), ValueText(nodes[1]));
            }
        );

        var TYPE_LIST = Multiple<AstNode.Identifier>("TYPE_LIST", new Grammar[] {IDENTIFIER, COMMA}, optional: true);
        var IL_FUNC = new Pattern("IL_FUNC", 
            new Grammar[] {ILish, IDENTIFIER, FUNC_IDEN, LPARAN, TYPE_LIST, RPARAN, STRING},
            (loc, nodes) => {
                string type = ValueText(nodes[1]);
                string name = ValueText(nodes[2]);
                string il = ValueText(nodes[6]);
                bool is_ilf = ((AstNode.Tok)nodes[0]).token.type == Token.Type.ILF;
                return new AstNode.IlFunc(loc, name, il, IlParamTypes(nodes[4]), type, is_ilf, true);
            }
        );

        var ALIAS = new Pattern("ALIAS", new Grammar[] {new Tok(Token.Type.ALIAS), IDENTIFIER, EQUAL, EXPRESSION}, 
            (loc, nodes) => new AstNode.Alias(loc, ValueText(nodes[1]), nodes[3])
        );

        var FIELD = new Pattern("FIELD", new Grammar[] {STATIC, MUT, IDENTIFIER, IDENTIFIER}, 
            (loc, nodes) => new AstNode.Field(loc, ValueText(nodes[2]), ValueText(nodes[3]), nodes[1] != null, nodes[0] != null)
        );
        
        var METHOD = new Pattern("METHOD", 
            new Grammar[] {STATIC, FUNC_TYPE_NAME, LPARAN, PARAM_LIST, RPARAN, LOCAL_BLOCK},
            (loc, nodes) => {
                string return_type = ((AstNode.Field)nodes[1]).type;
                string name = ((AstNode.Field)nodes[1]).name;
                AstNode.Param[] parameters = null;
                if (nodes[3] != null) parameters = ((AstNode.Multiple<AstNode.Param>)nodes[3]).ToArray();
                return new AstNode.FuncDef(loc, nodes[0] != null, return_type, name, (AstNode.Statements)nodes[5], parameters);
            }
        );

        var INHERIT = new Pattern("INHERIT", new Grammar[] {COLON, IDENTIFIER}, (loc, nodes) => nodes[1], optional: true);

        var CLASS_STMTS = NewStatements("CLASS_STMTS", new Grammar[] {METHOD, FIELD});
        var CLASS_BLOCK = NewBlock("CLASS_BLOCK", CLASS_STMTS);
        var CLASS_DEF = new Pattern("CLASS_DEF", new Grammar[] {CLASS, IDENTIFIER, INHERIT, CLASS_BLOCK}, (loc, nodes) => {
            string base_class = nodes[2] != null ? ValueText(nodes[2]) : null;
            return new AstNode.Class(loc, ValueText(nodes[1]), (AstNode.Statements)nodes[3], base_class);
        });

        var IL_METHOD = new Pattern("IL_FUNC", new Grammar[] {STATIC, ILish, FUNC_TYPE_NAME, LPARAN, TYPE_LIST, RPARAN, STRING},
            (loc, nodes) => {
                AstNode.Field type_name = (AstNode.Field)nodes[2];
                string il = ValueText(nodes[6]);
                string[] param_types = IlParamTypes(nodes[4]);
                bool is_ilf = ((AstNode.Tok)nodes[1]).token.type == Token.Type.ILF;
                return new AstNode.IlFunc(loc, type_name.name, il, param_types, type_name.type, is_ilf, nodes[0] != null);
            }
        );

        var IL_CLASS_STMTS = NewStatements("IL_CLASS_STMTS", new Grammar[] {IL_METHOD});
        var IL_CLASS_BLOCK = NewBlock("IL_CLASS_BLOCK", IL_CLASS_STMTS);
        var IL_CLASS = new Pattern("IL_CLASS", new Grammar[] {IL, CLASS, IDENTIFIER, STRING, IL_CLASS_BLOCK}, (loc, nodes) => {
            string il = ((AstNode.StringLiteral)nodes[3]).content;
            return new AstNode.IlClass(loc, ValueText(nodes[2]), il, (AstNode.Statements)nodes[4], null);
        });

        var GLOBAL_STATEMENTS = NewStatements("GLOBAL", new Grammar[] {FUNC_DEF, IL_FUNC, ALIAS, CLASS_DEF, IL_CLASS});

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

abstract class AstNode {

    public readonly Location location;
    AstNode node_parent;

    public AstNode(Location loc) => location = loc;
    public virtual string Type(Scope scope) => throw new NotImplementedException();
    public virtual void CreateNameInfo(Scope scope) => throw new NotImplementedException();
    public virtual void GenIl(Scope scope) => throw new NotImplementedException();
    protected virtual void SetParent(AstNode parent) => node_parent = parent;

    // Returns null if there is no ancestor of that type
    public T GetAncestorOfType<T>() where T : AstNode {
        if (node_parent == null) return null;
        if (node_parent is T) return (T)node_parent;
        return node_parent.GetAncestorOfType<T>();
    }

    public void ExpectType(string expected, Scope scope) {
        string actual_type = Type(scope);
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
    }

    // For prefix and postfix operators
    public class Nothing : AstNode {
        public Nothing() : base(new Location(-1, -1, null, -1)) {}
        public override string Type(Scope _) => "Void";
        public override void GenIl(Scope _) {}
    }

    public abstract class Value : AstNode {
        public readonly string content;
        public Value(Tok tok) : base(tok.location) => content = tok.token.value;
    }

    public class StringLiteral : Value {
        public StringLiteral(Tok tok): base(tok) {}
        public override string Type(Scope _) => "String";
        public override void GenIl(Scope _) => Output.WriteLine($"ldstr \"{content}\"");
    }

    public class IntegerLiteral : Value {
        public IntegerLiteral(Tok tok): base(tok) {}
        public override string Type(Scope _) => "Int";
        public override void GenIl(Scope _) => Output.WriteLine($"ldc.i4 {content}");
    }

    public class BooleanLiteral : IntegerLiteral {
        public BooleanLiteral(Tok tok): base(tok) {}
        public override string Type(Scope _) => "Bool";
    }

    public class CharLiteral : IntegerLiteral {
        public CharLiteral(Tok tok) : base(tok) {}
        public override string Type(Scope _) => "Char";
    }

    public class Identifier : Value {
        public Identifier(Tok tok): base(tok) {}

        public override string ToString() => $"Identifier with value `{content}`";

        public override string Type(Scope scope) {
            Name result = scope.LookUp(content, location);
            if (result is Name.Varish) return ((Name.Varish)result).Type(scope);
            if (result is Name.Class) return ((Name.Class)result).name;
            Umi.Crash($"Identifier was not a variable nor class", location);
            return null; // Make compiler happy
        }

        public override void GenIl(Scope scope) {
            Name result = scope.LookUp(content, location);
            // The if is for static methods
            if (result is Name.Varish) ((Name.Varish)result).GenLoadIl(scope, this);
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
        readonly bool is_method_call;

        public FuncCall(Location loc, string name, AstNode[] args, bool is_method_call = false) : base(loc) {
            this.name = name;
            arguments = args;
            this.is_method_call = is_method_call;
        }

        public FuncCall AsMethodCall(AstNode left_of_dot) {
            return new AstNode.FuncCall(location, name, arguments.Prepend(left_of_dot).ToArray(), true);
        }

        public override string ToString() {
            var str_args = Array.ConvertAll(arguments, a => a.ToString());
            return $"Function call `{name}` with arguments `{string.Join("`,`", str_args)}`";
        }

        FuncInfo GetFuncInfo(Scope scope) {
            Name maybe_func;
            if (is_method_call) {
                AstNode parent_arg = arguments[0];
                maybe_func = scope.GetClass(parent_arg, parent_arg.location).GetMember(name, location);
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

            string[] arg_types = Array.ConvertAll(arguments, arg => arg.Type(scope));
            if (is_method_call) arg_types = arg_types.Skip(1).ToArray(); // the class itself
            FuncInfo func_info = func.BestFit(arg_types);

            if (func_info == null) {
                string types = String.Join("`, `", arg_types);
                Umi.Crash($"Overload with types `{types}` does not exist for `{name}`", location);
            }

            return func_info;
        }

        public override string Type(Scope scope) => GetFuncInfo(scope).return_type;

        protected override void SetParent(AstNode parent) {
            base.SetParent(parent);
            foreach (var arg in arguments) arg.SetParent(this);
        }

        public override void GenIl(Scope scope) {
            FuncInfo func_info = GetFuncInfo(scope);

            // If calling a method without `this`
            if (!is_method_call && func_info is FuncInfo.Ord) {
                var fi = (FuncInfo.Ord)func_info;
                if (fi.parent_class != null && !fi.IsConstructor()) Output.WriteLine("ldarg 0");
            } else if (func_info is FuncInfo.Base) {
                Output.WriteLine("ldarg 0");
            }

            foreach (var arg in arguments) arg.GenIl(scope);
            func_info.GenIl(scope);
        }
    }
    
    public class TypeCast : AstNode {
        readonly string type;
        readonly AstNode value;

        public TypeCast(string type, AstNode value, Location loc): base(loc) {
            this.type = type;
            this.value = value;
        }

        public override string Type(Scope _) => type;

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

        public override void GenIl(Scope scope) {
            Name maybe_var = scope.LookUp(name, location);
            if (!(maybe_var is Name.Varish)) Umi.Crash("Can only reassign local variables and fields", location);
            
            Name.Varish variable = (Name.Varish)maybe_var;
            variable.CheckCanAssign(this);
            value.ExpectType(variable.Type(scope), scope);
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
            Name.Field field = dot.GetField(scope);

            value.ExpectType(field.type, scope);
            field.CheckCanAssign(this);

            dot.parent.GenIl(scope);
            value.GenIl(scope);
            field.GenStoreIl(scope, location);
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

        public override void GenIl(Scope scope) { 
            foreach (var stmt in statements) stmt.GenIl(scope);
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
            foreach (var stmt in stmts) {
                if (stmt is AstNode.VarDef) {
                    ((AstNode.VarDef)stmt).DeclareVariable(scope, new_scope, local_var_types);
                } else if (stmt is AstNode.Block) {
                    ((AstNode.Block)stmt).DeclareVariables(new_scope, local_var_types);
                }
            }
            return new_scope;
        }

        protected virtual void DeclareVariables(Scope scope, List<string> local_var_types) {
            block_scope = SubScope(scope, local_var_types, statements.statements);
        }

        protected void GenStatements() => statements.GenIl(block_scope);

        protected override void SetParent(AstNode parent) {
            base.SetParent(parent);
            statements.SetParent(this);
        }
    }

    public class If : Block {
        public readonly AstNode condition;

        public If(Location loc, AstNode condition, Statements statements) : base(loc, statements) {
            this.condition = condition;
        }

        // The stuff that's common in If and IfElse and return the if_end label
        protected string IfIl(Scope scope) {
            condition.ExpectType("Bool", scope);
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

        public override void GenIl(Scope scope) => Output.WriteLabel(IfIl(scope));
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

        public override void GenIl(Scope scope) {
            string if_end = IfIl(scope);
            string else_end = else_statements.end.Label();
            Output.WriteLine($"br {else_end}");
            Output.WriteLabel(if_end);
            else_statements.GenIl(else_scope);
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
        readonly string type;
        readonly VarAssign assignment;
        readonly bool is_mutable;

        public VarDef(Location loc, string type, VarAssign assignment, bool is_mutable) : base(loc) {
            this.type = type;
            this.assignment = assignment;
            this.is_mutable = is_mutable;
        }

        public void DeclareVariable(Scope scope, Scope new_scope, List<string> local_var_types) {
            new_scope.Add(assignment.name, new Name.Var(type, false, local_var_types.Count, location, is_mutable));
            local_var_types.Add(scope.GetIlType(type, location));
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
            expression?.GenIl(scope);
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

    void AddToScope(Scope scope, string parent_class, FuncInfo func_info, string name, string return_type, bool is_static) {
        if (name == return_type) { // Add constructor to the class
            if (!is_static) { // Ignore static constructors
                var c = (Name.Class)scope.LookUp(parent_class, location);
                c.constructor.functions.Add(func_info);
            }
        } else { // Add the function to the scope
            scope.AddFunction(location, name, func_info);
        }
    }

    public class FuncDef : Block {
        public readonly string name;
        readonly string return_type;
        readonly Param[] parameters;
        readonly bool is_static;

        public FuncInfo.Ord func_info;
        readonly List<string> local_var_types = new List<string>();

        public FuncDef(Location loc, bool is_static, string rt, string name, Statements stmts, Param[] paras): base(loc, stmts) {
            this.is_static = is_static;
            this.name = name;
            return_type = rt;
            parameters = paras ?? new Param[0];
        }

        public override void CreateNameInfo(Scope scope) {
            string parent_class = ParentClassName();
            func_info = new FuncInfo.Ord(parameters, return_type, is_static, name, location, parent_class);
            
            AddToScope(scope, parent_class, func_info, name, return_type, is_static);
            DeclareVariables(scope, local_var_types);
            
            List<Param> paras = parameters.ToList();
            if (!is_static) paras.Insert(0, new Param(location, parent_class, "this"));

            // Handle prefix operator functions
            if (paras.Count == 2 && paras[0].type == "Void") {
                Param p = paras[1];
                block_scope.Add(p.name, new Name.Var(p.type, true, 0, p.location));
            } else for (int i = 0; i < paras.Count; i++) {
                Param p = paras[i];
                var par = new Name.Var(p.type, true, i, p.location);
                block_scope.Add(p.name, par);
            }
        }

        public override void GenIl(Scope scope) {
            Output.WriteLine($".method {(is_static ? "static " : "virtual ")}{func_info.IlSignature(scope)}");
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
        readonly bool is_ilf;
        readonly bool is_static;

        public IlFunc(Location loc, string name, string il, string[] param_types, string rt, bool is_ilf, bool is_stc) : base(loc) {
            this.name = name;
            this.il = il;
            this.param_types = param_types;
            return_type = rt;
            this.is_ilf = is_ilf;
            is_static = is_stc;
        }

        public override void CreateNameInfo(Scope scope) {
            string parent_class = ParentClassName();
            FuncInfo func_info;
            if (is_ilf) func_info = new FuncInfo.Ord(param_types, return_type, is_static, il, location, parent_class);
            else func_info = new FuncInfo.Il(param_types, return_type, il);
            AddToScope(scope, parent_class, func_info, name, return_type, is_static);
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

        public Name.Field GetField(Scope scope) => (Name.Field)scope.GetClass(parent, location).GetMember(child, location);
        
        public override string Type(Scope scope) => GetField(scope).type;

        protected override void SetParent(AstNode node_parent) {
            base.SetParent(node_parent);
            parent.SetParent(this);
        }

        public override void GenIl(Scope scope) {
            parent.GenIl(scope);
            GetField(scope).GenLoadIl(scope, location);
        }
    }

    public class Field : AstNode {
        public readonly string type;
        public readonly string name;
        readonly bool is_mutable;
        readonly bool is_static;

        public Field(Location loc, string type, string name, bool is_mutable = false, bool is_static = false) : base(loc) {
            this.type = type;
            this.name = name;
            this.is_mutable = is_mutable;
            this.is_static = is_static;
        }

        public override void CreateNameInfo(Scope scope) {
            scope.Add(name, new Name.Field(location, type, name, ParentClassName(), is_mutable, is_static));
        }

        public override void GenIl(Scope scope) {
            Output.WriteLine($".field {(is_static ? "static" : "")} {scope.GetIlType(type, location)} {name}\n");
        }
    }

    public class IlClass : AstNode {
        public readonly string name;
        readonly string il_name;
        protected string base_class;
        protected readonly Statements statements;
        protected Scope class_scope;

        public IlClass(Location loc, string name, string il_name, Statements statements, string base_class) : base(loc) {
            this.name = name;
            this.il_name = il_name;
            this.statements = statements;
            this.base_class = base_class;
        }

        protected override void SetParent(AstNode parent) {
            base.SetParent(parent);
            statements.SetParent(this);
        }

        public override void CreateNameInfo(Scope scope) {
            class_scope = new Scope(scope);
            scope.Add(name, new Name.Class(location, name, class_scope, il_name, base_class));

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
        public Class(Location loc, string name, Statements statements, string bas) : base(loc, name, name, statements, bas) {}

        public override void GenIl(Scope scope) {
            string extends = base_class == null ? "" : $" extends {base_class}";
            Output.WriteLine($".class {name}{extends}");
            Output.WriteLine("{");
            Output.Indent();
            statements.GenIl(class_scope);
            Output.Unindent();
            Output.WriteLine("}\n");
        }
    }

    public class Program : AstNode {
        AstNode[] global_statements;

        public Program(AstNode[] s) : base(s[0].location) => global_statements = s;

        public override void CreateNameInfo(Scope scope) {
            foreach(AstNode statement in global_statements) {
                statement.SetParent(this);
                statement.CreateNameInfo(scope);
            }
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
    // With chains of if else, they all go to the end so they create duplicate labels
    static HashSet<string> labels = new HashSet<string>();

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

    public static void Indent() => indentation += 4;
    public static void Unindent() => indentation -= 4;
}

abstract class Name {

    public readonly Location defined_at;

    Name(Location defined_at) => this.defined_at = defined_at;

    public class Class : Name {
        public readonly string name;
        public readonly string il_name;
        public readonly string base_class;
        public readonly Func constructor;
        public readonly Scope members;

        public Class(Location loc, string name, Scope members, string il_name, string base_class) : base(loc) {
            this.name = name;
            this.members = members;
            constructor = new Func(loc);
            this.il_name = il_name;
            this.base_class = base_class;
        }

        public Name GetMember(string child, Location location) {
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
        public abstract void GenLoadIl(Scope scope, AstNode.Identifier context);
        public abstract void CheckCanAssign(AstNode assignment);
        public virtual void GenStoreIl(Scope scope, Location loc, AstNode value) => value.GenIl(scope);
        
    }

    public class Var : Varish {
        public readonly string type;
        public readonly int index;
        readonly bool is_param; // local variable or parameter
        readonly bool is_mutable;

        public Var(string type, bool is_param, int index, Location defined_at, bool mutable = false) : base(defined_at) {
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

        public override string Type(Scope _) => type;

        public override void GenLoadIl(Scope _, AstNode.Identifier context) {
            if (defined_at > context.location) {
                Umi.Crash($"Can not use variable `{context.content}` before it is defined", context.location);
            }
            Output.WriteLine($"{(is_param ? "ldarg" : "ldloc")} {index}");
        }

        public override void GenStoreIl(Scope scope, Location loc, AstNode value) {
            base.GenStoreIl(scope, loc, value);
            Output.WriteLine($"stloc {index}");
        }
    }

    public class Alias : Varish {
        public AstNode node;
        public Alias(Location defined_at, AstNode node) : base(defined_at) => this.node = node;
        public override string Type(Scope scope) => node.Type(scope);
        public override void GenLoadIl(Scope scope, AstNode.Identifier _) => node.GenIl(scope);
        public override void CheckCanAssign(AstNode a) => Umi.Crash("Can not reassign alias", a.location);
    }

    public class Field : Varish {
        public readonly string type;
        public readonly bool is_mutable;
        readonly string name;
        readonly string parent_class;
        readonly bool is_static;

        public Field(Location defined_at, string type, string name, string pc, bool mutable, bool stati) : base(defined_at) {
            this.type = type;
            this.name = name;
            this.parent_class = pc;
            is_mutable = mutable;
            is_static = stati;
        }

        public override void CheckCanAssign(AstNode assignment) {
            if (is_mutable) return;
            FuncInfo.Ord func_info = assignment.GetAncestorOfType<AstNode.FuncDef>().func_info;
            if (func_info.IsConstructor() && func_info.parent_class == parent_class) return;
            Umi.Crash($"Cannot reassign immutable member `{parent_class}.{name}` outside of constructor", assignment.location);
        }

        public override string Type(Scope _) => type;

        void GenInstructionIl(Scope scope, Location loc, string instruction) {
            Output.WriteLine($"{instruction} {scope.GetIlType(type, loc)} {parent_class}::{name}");
        }

        public void GenLoadIl(Scope scope, Location loc) => GenInstructionIl(scope, loc, is_static ? "ldsfld" : "ldfld");
        
        public void GenStoreIl(Scope scope, Location loc) => GenInstructionIl(scope, loc, is_static ? "stsfld" : "stfld");

        public override void GenStoreIl(Scope scope, Location loc, AstNode value) {
            // `this`. This function should only be called when treated like normal var assign
            if (!is_static) Output.WriteLine("ldarg 0");
            base.GenStoreIl(scope, loc, value);
            GenStoreIl(scope, loc);
        }

        // This should only be called when the field is used without `this` in its class
        public override void GenLoadIl(Scope scope, AstNode.Identifier context) {
            if (!is_static) Output.WriteLine("ldarg 0");
            GenLoadIl(scope, context.location);
        }
    }

}

abstract class FuncInfo {
    public readonly string[] param_types;
    public readonly string return_type;
    
    public FuncInfo(string[] param_types, string return_type) {
        this.param_types = param_types;
        this.return_type = return_type;
    }

    public abstract void GenIl(Scope scope);

    // Ordinary function
    public class Ord : FuncInfo {
        readonly bool is_static;
        readonly string il_name;
        public readonly string parent_class;
        readonly Location defined_at;

        public Ord(string[] paras, string rtype, bool is_static, string name, Location loc, string pclass) : base(paras, rtype) {
            this.is_static = is_static;
            defined_at = loc;
            il_name = name;
            parent_class = pclass;
        }

        public Ord(AstNode.Param[] paras, string rtype, bool is_static, string name, Location loc, string parent_class)
            : this(Array.ConvertAll(paras, p => p.type), rtype, is_static, IlName(name, rtype, paras, is_static), loc, parent_class) {}

        static string IlName(string name, string return_type, AstNode.Param[] parameters, bool is_static) {
            if (name == return_type) return is_static ? ".cctor" : ".ctor"; // constructor   
            if (parameters.Length == 2) { // unary operator
                if (parameters[0].type == "Void") return $"'PREFIX{name}'";
                if (parameters[1].type == "Void") return $"'POSTFIX{name}'";
            }
            return $"'{name}'";
        }

        public bool IsConstructor() => il_name == ".ctor" || il_name == ".cctor";

        public string IlSignature(Scope scope, string prefix = "") {
            string rt = scope.GetIlType(return_type, defined_at);
            if (IsConstructor()) rt = "void";

            string[] pts = (string[])param_types.Clone(); 
            if (pts.Length == 2) { // unary operator
                if (pts[0] == "Void") pts = new string[] {pts[1]};
                else if (pts[1] == "Void") pts = new string[] {pts[0]};
            }
            string para_types = scope.ParameterTypesString(pts, defined_at);

            return $"{rt} {prefix}{il_name}({para_types})";
        }

        public override void GenIl(Scope scope) {
            string call_word = is_static ? "call" : "callvirt";
            if (IsConstructor()) call_word = "newobj";
            
            string prefix = parent_class == null ? "" : scope.GetIlType(parent_class, defined_at) + "::";
            Output.WriteLine($"{call_word} {IlSignature(scope, prefix)}");
        }
    }

    // TODO: static constructors' base
    // Calling the base constructor within a constructor
    public class Base : FuncInfo {
        readonly Location defined_at;
        readonly string parent_class;

        public Base(string[] ptypes, string parent_class, Location loc): base(ptypes, "Void") {
            defined_at = loc;
            this.parent_class = parent_class;
        }

        public override void GenIl(Scope scope) {
            string paras = scope.ParameterTypesString(param_types, defined_at);
            Output.WriteLine($"call instance void {parent_class}::.ctor({paras})");
        }
    }

    public class Il : FuncInfo {
        readonly string il;
        public Il(string[] ptypes, string rtype, string il) : base(ptypes, rtype) => this.il = il;
        
        public override void GenIl(Scope _) => Output.WriteLine(il);
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

    public Name.Class LookUpClass(string type, Location location) {
        Name name = LookUp(type, location);
        if (!(name is Name.Class)) Umi.Crash($"`{type}` is not a class", location);
        return (Name.Class)name;
    }

    public Name.Class GetClass(AstNode parent, Location location) {
        string cls_type = parent.Type(this);
        return LookUpClass(cls_type, location);
    }

    public string GetIlType(string type, Location location) => LookUpClass(type, location).il_name;
    
    public string ParameterTypesString(string[] param_types, Location location) {
        return string.Join(", ", Array.ConvertAll(param_types, p => GetIlType(p, location)));
    }

    public bool TypesMatch(string expected, string actual, Location loc) {
        if (expected == actual) return true;
        var actual_class = LookUpClass(actual, loc);
        if (actual_class.base_class == null) return false;
        return TypesMatch(expected, actual_class.base_class, loc);
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
