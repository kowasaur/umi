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

    public Location Copy() {
        return new Location(line, column);
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
        IDENTIFIER,
        STRING,
        LPARAN,
        RPARAN,
        LCURLY,
        RCURLY,
        STATEMENT_END
    }

}

class Umi {

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

    static void Main(string[] args) {
        if (args.Length < 1) {
            Console.WriteLine("You must specify the path to the file");
            return;
        }

        var tokens = Lex(File.ReadAllText(args[0]));
        foreach (var token in tokens) {
            Console.WriteLine($"{token.type} {token.location.line}:{token.location.column} {token.value}");
        }
    }

}
