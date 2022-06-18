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
        STATEMENT_END
    }

}

class AstNode {

    public readonly Location location;

    public AstNode(Location loc) {
        location = loc;
    }

    public virtual void GenIl() => throw new NotImplementedException();

    public class FuncCall : AstNode {
        public string name;
        public string argument;

        public FuncCall(Location loc, string name, string arg) : base(loc) {
            this.name = name;
            argument = arg;
        }

        public override void GenIl() {
            Output.WriteLine($"ldstr \"{argument}\"");
            Output.WriteLine("call void " + name + "(string)");
        }
    }

    public class FuncDef : AstNode {
        public string name;
        public string return_type;
        public FuncCall[] function_calls;

        public FuncDef(Location loc, string name, string type, FuncCall[] func_calls) : base(loc) {
            this.name = name;
            return_type = type;
            function_calls = func_calls;
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

    // i is the initial index in the list and n is the number of tokens
    public delegate AstNode CreateNode(List<Token> tokens, int i, int n);

    public readonly Token.Type token; // for when it's just a token
    public readonly Pattern[] pattern;
    public readonly CreateNode createNode;

    public static readonly Pattern IDENTIFIER = new Pattern(Token.Type.IDENTIFIER); 
    public static readonly Pattern STRING = new Pattern(Token.Type.STRING);
    public static readonly Pattern LPARAN = new Pattern(Token.Type.LPARAN); 
    public static readonly Pattern RPARAN = new Pattern(Token.Type.RPARAN); 
    public static readonly Pattern LCURLY = new Pattern(Token.Type.LCURLY); 
    public static readonly Pattern RCURLY = new Pattern(Token.Type.RCURLY); 
    public static readonly Pattern STATEMENT_END = new Pattern(Token.Type.STATEMENT_END);

    public static readonly Pattern FUNC_CALL = new Pattern(
        new Pattern[] {IDENTIFIER, LPARAN, STRING, RPARAN, STATEMENT_END},
        (tokens, i, _) => new AstNode.FuncCall(tokens[i+0].location, tokens[i+0].value, tokens[i+2].value)
    );
    public static readonly Pattern FUNC_DEF = new Pattern(
        // For now if there's a pattern in a pattern it will be an infinite amount
        new Pattern[] {IDENTIFIER, IDENTIFIER, LPARAN, RPARAN, LCURLY, FUNC_CALL, RCURLY},
        (tokens, i, n) => {
            int func_amount = (n - 6) / 5; // 6 tokens in funcdef, 5 tokens each in funccall
            AstNode.FuncCall[] funcs = new AstNode.FuncCall[func_amount];
            for (int fi = 0; fi < func_amount; fi++) {
                funcs[fi] = (AstNode.FuncCall) FUNC_CALL.createNode(tokens, 5 * fi + 5 + i, 5);
            }
            return new AstNode.FuncDef(tokens[i+0].location, tokens[i+1].value, tokens[i+0].value, funcs);
        }
    );
    
    public Pattern(Token.Type token) => this.token = token;
    
    public Pattern(Pattern[] pattern, CreateNode createNode) {
        this.pattern = pattern;
        this.createNode = createNode;
    }

    public bool DoesMatch(List<Token> tokens, int start_index, out int new_offset) {
        new_offset = 0; // Default for when it doesn't match
        int offset = start_index;

        if (tokens.Count - offset < pattern.Length) return false;

        for (int i = 0; i < pattern.Length; i++) {
            Pattern sub_pattern = pattern[i];
            if (sub_pattern.pattern == null) { // Toke
                if (pattern[i].token != tokens[i + offset].type) return false;
            } else { // Pattern
                while (sub_pattern.DoesMatch(tokens, i + offset, out int o)) {
                    offset += o;
                }
                offset--; // tbh idk why I have to do this
            }
        }

        new_offset = offset + pattern.Length - start_index;
        return true;
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

    static readonly Pattern[] PATTERNS = {Pattern.FUNC_CALL, Pattern.FUNC_DEF};

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

    static List<AstNode> ParseTokens(List<Token> tokens) {
        int i = 0;
        List<AstNode> nodes = new List<AstNode>();
        while (i < tokens.Count) {
            bool found_match = false;
            foreach (Pattern pattern in PATTERNS) {
                if (pattern.DoesMatch(tokens, i, out int offset)) {
                    found_match = true;
                    nodes.Add(pattern.createNode(tokens, i, offset));
                    i += offset;
                    break;
                } 
            }
            if (!found_match) {
                Console.WriteLine("Unknown pattern");
                Environment.Exit(1);
            }
        }
        return nodes;
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
        List<AstNode> ast = ParseTokens(tokens);
        GenIlForProgram(ast);
    }

}
