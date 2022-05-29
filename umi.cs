using System;
using System.IO;

class Token {

    public readonly Type type;
    public readonly string value;
    public readonly uint line;
    public readonly uint column;

    public Token(Type type, uint line, uint col, string value = null) {
        this.type = type;
        this.line = line;
        column = col;
        this.value = value;
    }

    public enum Type {
        IDENTIFIER,
        STRING,
        LPARAN,
        RPARAN,
        BLOCK_START,
        BLOCK_END,
        STATEMENT_END
    }

}

class Umi {
    static void Main(string[] args) {
        if (args.Length < 1) {
            Console.WriteLine("You must specify the path to the file");
            return;
        }

        string text = File.ReadAllText(args[0]);
        Console.WriteLine(text);
    }
}
