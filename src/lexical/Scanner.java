package lexical;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.HashMap;
import java.util.Map;

import exceptions.ScannerException;
import utils.TokenType;

public class Scanner {
    char[] source_code;
    int state;
    int pos;
    int line;
    int column;

    private static final Map<String, TokenType> reservedWords;

    static {
        reservedWords = new HashMap<>();
        reservedWords.put("INT", TokenType.RESERVED_KEYWORD);
        reservedWords.put("FLOAT", TokenType.RESERVED_KEYWORD);
        reservedWords.put("PRINT", TokenType.RESERVED_KEYWORD);
        reservedWords.put("IF", TokenType.RESERVED_KEYWORD);
        reservedWords.put("ELSE", TokenType.RESERVED_KEYWORD);
        reservedWords.put("STATEMENTS", TokenType.RESERVED_KEYWORD);
        reservedWords.put("ALGORITHM", TokenType.RESERVED_KEYWORD);
        reservedWords.put("THEN", TokenType.RESERVED_KEYWORD);
        reservedWords.put("BEGIN", TokenType.RESERVED_KEYWORD);
        reservedWords.put("END", TokenType.RESERVED_KEYWORD);
        reservedWords.put("INPUT", TokenType.RESERVED_KEYWORD);
        reservedWords.put("ASSIGN", TokenType.RESERVED_KEYWORD);
        reservedWords.put("AND", TokenType.RESERVED_KEYWORD);
        reservedWords.put("OR", TokenType.RESERVED_KEYWORD);
    }

    public Scanner(String filename) {
        try {
            String contentBuffer = new String(Files.readAllBytes(Paths.get(filename)), StandardCharsets.UTF_8);
            this.source_code = contentBuffer.toCharArray();
            this.pos = 0;
            this.line = 1;
            this.column = 1;
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    public Token nextToken() throws Exception {
        char currentChar;
        String content = "";
        this.state = 0;

        while (true) {
            if (isEOF()) {
                return null;
            }
            currentChar = nextChar();

            if (currentChar == '#') {
                while (!isEOF() && currentChar != '\n' && currentChar != '\r') {
                    currentChar = nextChar();
                }
                continue;
            }

            switch (state) {
                case 0:
                    if (isLetter(currentChar) || isUnderline(currentChar)) {
                        content += currentChar;
                        this.state = 1;
                    } else if (isDigit(currentChar) || currentChar == '.') {
                        content += currentChar;
                        this.state = currentChar == '.' ? 4 : 2;
                    } else if (isOperator(currentChar)) {
                        TokenType operator = getOperator(currentChar);
                        return new Token(operator, String.valueOf(currentChar), line, column);
                    } else if (currentChar == '=') {
                        if (peekChar() == '=') {
                            char nextChar = nextChar(); // Consuming the next '='
                            if (peekChar() == '=') {
                                nextChar();
                                throw new ScannerException("Unrecognized symbol \'" + currentChar + "\' at line " + line + ", column " + column);
                            }

                            return new Token(TokenType.EQUALS,  String.valueOf(currentChar) + nextChar, line, column);

                        } else {
                            return new Token(TokenType.ASSIGN, String.valueOf(currentChar), line, column);
                        }

                    } else if (currentChar == '>') {
                        if (peekChar() == '=') {
                            char nextChar = nextChar(); // Consuming the next '='
                            return new Token(TokenType.GREATER_EQUALS, String.valueOf(currentChar) + nextChar, line, column);
                        } else {
                            return new Token(TokenType.GREATER, String.valueOf(currentChar), line, column);
                        }
                    } else if(currentChar == '<') {
                        if (peekChar() == '=') {
                            char nextChar = nextChar(); // Consuming the next '='
                            return new Token(TokenType.LESS_EQUALS, String.valueOf(currentChar) + nextChar, line, column);
                        } else {
                            return new Token(TokenType.LESS, String.valueOf(currentChar), line, column);
                        }
                    } else if (currentChar == '!' && peekChar() == '=') {
                        char nextChar = nextChar(); // Consuming the next '='
                        return new Token(TokenType.DIF_OP, String.valueOf(currentChar) + nextChar, line, column);
                    } else if (currentChar == '(') {
                        return new Token(TokenType.LEFT_PARENTHESIS, String.valueOf(currentChar), line, column);
                    } else if(currentChar == ')') {
                        return new Token(TokenType.RIGHT_PARENTHESIS, String.valueOf(currentChar), line, column);
                    } else if(isTwoPoints(currentChar)) {
                        return new Token(TokenType.TWO_POINTS, String.valueOf(currentChar), line, column);
                    } else if(isDelimeter(currentChar)) {
                        return new Token(TokenType.DELIM, String.valueOf(currentChar), line, column);
                    } else if (isSpace(currentChar)) {
                        this.state = 0;
                    }  else if(isDoubleQuotes(currentChar)) {
                        content += currentChar;
                        this.state = 6;
                    } else {
                        throw new ScannerException("Unrecognized symbol \'" + currentChar + "\' at line " + line + ", column " + column);
                    }
                    break;
                case 1:
                    if (isLetter(currentChar) || isDigit(currentChar)) {
                        content += currentChar;
                        this.state = 1;
                    } else {
                        back();
                        TokenType type = reservedWords.getOrDefault(content, TokenType.IDENTIFIER);
                        return new Token(type, content, line, column);
                    }
                    break;
                case 2:
                    if (isDigit(currentChar)) {
                        content += currentChar;
                        this.state = 2;
                    } else if (currentChar == '.') {
                        content += currentChar;
                        this.state = 3;
                    } else {
                        back();
                        return new Token(TokenType.NUMBER, content, line, column);
                    }
                    break;

                case 3:
                    if (isDigit(currentChar)) {
                        content += currentChar;
                        this.state = 5;
                    } else {
                        throw new ScannerException("Number Malformed: expected number after '.' received \'" + currentChar + "\' at line " + line + ", column " + column);
                    }
                    break;

                case 4:
                    if (isDigit(currentChar)) {
                        content += currentChar;
                        this.state = 5;
                    } else {
                        throw new ScannerException("Number Malformed: expected number after '.' received \'" + currentChar + "\' at line " + line + ", column " + column);
                    }
                    break;

                case 5:
                    if (isDigit(currentChar)) {
                        content += currentChar;
                        this.state = 5;
                    } else {
                        back();
                        return new Token(TokenType.NUMBER, content, line, column);
                    }
                    break;

                
                case 6:
                    if (isDoubleQuotes(currentChar)) {
                        content += currentChar;
                        return new Token(TokenType.STRING, content, line, column);
                    } else {
                        content += currentChar;
                    }
                default:
                    break;
            }
        }
    }

    private boolean isSpace(char currentChar) {
        return currentChar == ' ' || currentChar == '\n' || currentChar == '\t' || currentChar == '\r';
    }

    private void back() {
        this.pos--;
        // Ensure column and line are adjusted
        if (pos > 0 && source_code[pos] == '\n') {
            line--;
            // reset column to the length of the previous line
            column = 1;  // reset column and then find the last newline character
            for (int i = pos - 2; i >= 0; i--) {
                if (source_code[i] == '\n') {
                    break;
                }
                column++;
            }
        } else {
            column--;
        }
    }

    private char peekChar() {
        if (isEOF()) {
            return '\0';
        }
        return this.source_code[pos];
    }

    private void incrementCounters(char currentChar) {
        if (currentChar == '\n') {
            line++;
            column = 1;
        } else {
            column++;
        }
    }

    private char nextChar() {
        char currentChar = this.source_code[pos];
        incrementCounters(currentChar);
        pos++;  // Moving to the next character only after consuming the current one.
        return currentChar;
    }

    private boolean isEOF() {
        return this.pos >= this.source_code.length;
    }

    private boolean isDigit(char currentChar) {
        return currentChar >= '0' && currentChar <= '9';
    }

    private boolean isOperator(char currentChar) {
        return currentChar == '+' || currentChar == '-' || currentChar == '*' || currentChar == '/';
    }

    private boolean isTwoPoints(char currentChar) {
        return currentChar == ':';
    }

    private boolean isDelimeter(char currentChar) {
        return currentChar == ';';
    }

    private TokenType getOperator(char currentChar) {
        if (currentChar == '+') {
            return TokenType.SUM_OP;
        } else if(currentChar == '-') {
            return TokenType.SUB_OP;
        } else if(currentChar == '*') {
            return TokenType.MULT_OP;
        } else  {
            return TokenType.DIV_OP;
        }
    }

    private boolean isLetter(char currentChar) {
        return (currentChar >= 'a' && currentChar <= 'z') || (currentChar >= 'A' && currentChar <= 'Z') || currentChar == '_';
    }

    private boolean isDoubleQuotes(char currentChar) {
        return currentChar == '"';
    }

    private boolean isUnderline(char currentChar) {
        return currentChar == '_';
    }
}

