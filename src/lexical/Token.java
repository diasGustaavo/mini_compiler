package lexical;

import utils.TokenType;

public class Token {
    private TokenType type;
    private String content;
    private int line;
    private int column;

    public Token(TokenType type, String content, int line, int column) {
        this.type = type;
        this.content = content;
        this.line = line;
        this.column = column;
    }

    public TokenType getType() {
        return type;
    }

    public void setType(TokenType type) {
        this.type = type;
    }

    public String getContent() {
        return content;
    }

    public void setContent(String content) {
        this.content = content;
    }

    public int getLine() {
        return line;
    }

    public void setLine(int line) {
        this.line = line;
    }

    public int getColumn() {
        return column;
    }

    public void setColumn(int column) {
        this.column = column;
    }

    @Override
    public String toString() {
        return "Token [type=" + type + ", content=" + content + ", line=" + line + ", column=" + column + "]";
    }
}
