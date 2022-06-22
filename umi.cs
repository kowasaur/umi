using System;
using System.IO;
using System.Collections.Generic;

class Location {

    public uint line;
    public uint column;

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
        LPARAN,
        RPARAN,
        LCURLY,
        RCURLY,
        STATEMENT_END,
        COMMA
    }

}

// TODO: make IL generation work properly

class AstNode {

    public readonly Location location;

    public AstNode(Location loc) {
        location = loc;
    }

    public virtual void GenIl() => throw new NotImplementedException();

    public class Value : AstNode {
        public readonly Token value;
        public Value(Token value) : base(value.location) => this.value = value;
    }

    public class Arguments : AstNode {
        public readonly string[] args;
        public Arguments(Location loc, string[] args) : base(loc) => this.args = args;
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
        public readonly string name;
        public readonly string[] arguments;

        public FuncCall(Location loc, string name, string[] args) : base(loc) {
            this.name = name;
            arguments = args;
        }

        public override void GenIl() {
            foreach (var arg in arguments) {
                Output.WriteLine($"ldstr \"{arg}\"");
            }
            Output.WriteLine("call void " + name + "(string)");
        }
    }

    public class FuncDef : AstNode {
        public readonly string name;
        public readonly string return_type;
        public readonly FuncCall[] function_calls;
        public readonly Parameters parameters;

        public FuncDef(Location loc, string n, string t, FuncCall[] f, Parameters p) : base(loc) {
            name = n;
            return_type = t;
            function_calls = f;
            parameters = p;
        }

        public override void GenIl() {
            Output.WriteLine($".method static void {name}()");
            Output.WriteLine("{");
            Output.Indent();
            if (name == "main") Output.WriteLine(".entrypoint");

            foreach (var func_call in function_calls) func_call.GenIl();

            Output.WriteLine("ret");
            Output.Unindent();
            Output.WriteLine("}\n");
        }
    }

}

class Pattern {

    public delegate AstNode GenAstType(List<Token> tokens, ref int i, Pattern self);
    public delegate Pattern Constructor();

    public Pattern[] tails;
    public GenAstType GenAst = (List<Token> tokens, ref int i, Pattern self) => {
        if (tokens[i].type == self.token_type) return new AstNode.Value(tokens[i++]);
        return null;
    };

    readonly Token.Type token_type;

    readonly Pattern subpattern_head;
    readonly string name;

    public Pattern(Token.Type token_type, Pattern[] tails = null) {
        this.token_type = token_type;
        this.tails = tails;
    }

    public Pattern(Constructor constructor, GenAstType GenAst, string name, Pattern[] tails = null) {
        subpattern_head = constructor();
        this.GenAst = GenAst;
        this.name = name;
        this.tails = tails;
    }

    public static implicit operator string(Pattern p) => p.name == null ? p.token_type.ToString() : p.name;

    public List<AstNode> ParseTokens(List<Token> tokens, ref int i) {
        List<AstNode> nodes = new List<AstNode>();
        if (subpattern_head == null) Umi.Crash("ParseTokens being called on primitive", tokens[i].location);
        var pattern_node = subpattern_head;
        var head_ast_node = pattern_node.GenAst(tokens, ref i, pattern_node);
        if (head_ast_node != null) {
            nodes.Add(head_ast_node);
            // Console.WriteLine($"Added {pattern_node.token_type}");
        } else {
            Umi.Crash("Pretty sure this should never be reached", tokens[i].location);
        }

        while (pattern_node.tails != null && i < tokens.Count) {
            var token = tokens[i];
            bool matched = false;
            foreach (var child_node in pattern_node.tails) {
                var ast_node = child_node.GenAst(tokens, ref i, child_node);
                if (ast_node != null) {
                    nodes.Add(ast_node);
                    pattern_node = child_node;
                    // Console.WriteLine($"In {(string)this} added {(string)child_node}");
                    matched = true;
                    break;
                }
            }
            if (!matched) {
                string expected = "";
                foreach (var child_node in pattern_node.tails) {
                    expected += child_node + ", ";
                }
                Umi.Crash($"Unexpected token; In {name} expected {expected}but found {token.type}", token.location);
            }
        }
        
        return nodes;
    }

    // This is a tree structure
    static readonly Pattern FUNC_DEF = new Pattern(() => {
        var end_block = new Pattern(Token.Type.RCURLY);
        var statement = new Pattern((Constructor) (() => {
            var semicolon = new Pattern(Token.Type.STATEMENT_END);

            var arguments = new Pattern(() => {
                var r_paran = new Pattern(Token.Type.RPARAN);
                var additional_argument = new Pattern(Token.Type.STRING, new Pattern[] {r_paran, null});
                var comma = new Pattern(Token.Type.COMMA, new Pattern[] {additional_argument});
                additional_argument.tails[1] = comma;
                var first_argument = new Pattern(Token.Type.STRING, new Pattern[] {r_paran, comma});
                var l_paran = new Pattern(Token.Type.LPARAN, new Pattern[] {r_paran, first_argument});
                return l_paran;
            }, (List<Token> tokens, ref int i, Pattern self) => {
                int start = i;
                List<AstNode> nodes = self.ParseTokens(tokens, ref i);
                string[] args = new string[(nodes.Count - 1)/2];
                // Ignore the brackets and commas
                for (int j = 1; j < nodes.Count - 1; j += 2) {
                    args[(j-1)/2] = ((AstNode.Value)nodes[j]).value.value;
                }
                return new AstNode.Arguments(tokens[start].location, args);
            }, "arguments", new Pattern[] {semicolon});

            var name = new Pattern(Token.Type.IDENTIFIER, new Pattern[] {arguments});
            return name;
        }), (List<Token> tokens, ref int i, Pattern self) => {
            int start = i;
            List<AstNode> nodes = self.ParseTokens(tokens, ref i);
            string name = ((AstNode.Value)nodes[0]).value.value;
            string[] args = ((AstNode.Arguments)nodes[1]).args;
            return new AstNode.FuncCall(tokens[start].location, name, args);
        }, "statements");
        statement.tails = new Pattern[] {end_block, statement};
        var start_block = new Pattern(Token.Type.LCURLY, new Pattern[] {statement});
        var parameters = new Pattern(() => {
            var r_paran = new Pattern(Token.Type.RPARAN);

            var addit_param_var = new Pattern(Token.Type.IDENTIFIER);
            var addit_param_type = new Pattern(Token.Type.IDENTIFIER, new Pattern[] {addit_param_var});
            var comma = new Pattern(Token.Type.COMMA, new Pattern[] {addit_param_type});
            addit_param_var.tails = new Pattern[] {r_paran, comma};

            var first_param_var = new Pattern(Token.Type.IDENTIFIER, new Pattern[] {r_paran, comma});
            var first_param_type = new Pattern(Token.Type.IDENTIFIER, new Pattern[] {first_param_var});
            var l_paran = new Pattern(Token.Type.LPARAN, new Pattern[] {r_paran, first_param_type});
            return l_paran;
        }, (List<Token> tokens, ref int i, Pattern self) => {
            int start = i;
            List<AstNode> nodes = self.ParseTokens(tokens, ref i);
            // TODO: check this maths and other arrays
            string[] types = new string[(nodes.Count - 1)/3];
            string[] names = new string[(nodes.Count - 1)/3];
            // Ignore the brackets and commas
            for (int j = 1; j < nodes.Count - 1; j += 3) {
                types[(j-1)/3] = ((AstNode.Value)nodes[j]).value.value;
                names[(j-1)/3] = ((AstNode.Value)nodes[j]).value.value;
            }
            // TODO use the actual things
            return new AstNode.Parameters(tokens[start].location, types, names);
        }, "parameters", new Pattern[] {start_block});
        var func_def_name = new Pattern(Token.Type.IDENTIFIER, new Pattern[] {parameters});
        var type = new Pattern(Token.Type.IDENTIFIER, new Pattern[] {func_def_name});
        return type;
    }, (List<Token> tokens, ref int i, Pattern self) => {
        int start = i;
        List<AstNode> nodes = self.ParseTokens(tokens, ref i);

        string type = ((AstNode.Value)nodes[0]).value.value;
        string name = ((AstNode.Value)nodes[1]).value.value;
        AstNode.Parameters parameters = (AstNode.Parameters)nodes[2];

        int func_call_amount = nodes.Count - 5;
        AstNode.FuncCall[] funcs = new AstNode.FuncCall[func_call_amount];
        for (int j = 4; j < nodes.Count - 1; j++) funcs[j-4] = (AstNode.FuncCall)nodes[j];
 
        return new AstNode.FuncDef(tokens[start].location, name, type, funcs, parameters);
    }, "function definition");

    // Workaround to allow multiple functions
    public static readonly Pattern PROGRAM = new Pattern(() => {
        FUNC_DEF.tails = new Pattern[] {FUNC_DEF};
        return FUNC_DEF;
    }, null, "program");
    
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

            switch (c) {
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
            }
            if ('A' <= c && c <= 'z') {
                type = Token.Type.IDENTIFIER;
                string content = $"{c}";
                while ('1' <= file[i + 1] && file[i + 1] <= 'z') {
                    i++;
                    position.Increase(file[i]);
                    content += file[i];
                }
                value = content;
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
        Output.WriteLine(".method static void print(string)");
        Output.WriteLine("{");
        Output.Indent();
        Output.WriteLine("ldarg 0");
        Output.WriteLine("call void [mscorlib]System.Console::WriteLine(string)");
        Output.WriteLine("ret");
        Output.Unindent();
        Output.WriteLine("}\n");

        foreach (var node in ast) node.GenIl();
    }

    static void Main(string[] args) {
        if (args.Length < 1) {
            Console.WriteLine("You must specify the path to the file");
            return;
        }

        List<Token> tokens = Lex(File.ReadAllText(args[0]));
        int i = 0;
        List<AstNode> ast = Pattern.PROGRAM.ParseTokens(tokens, ref i);
        GenIlForProgram(ast);
    }

}
