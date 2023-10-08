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
        reservedWords.put("int", TokenType.RESERVED_KEYWORD);
        reservedWords.put("float", TokenType.RESERVED_KEYWORD);
        reservedWords.put("print", TokenType.RESERVED_KEYWORD);
        reservedWords.put("if", TokenType.RESERVED_KEYWORD);
        reservedWords.put("else", TokenType.RESERVED_KEYWORD);
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
                    if (isLetter(currentChar)) {
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
                    }  else {
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
    }

    private char peekChar() {
        if (isEOF()) {
            return '\0';
        }
        return this.source_code[pos];
    }

    private char nextChar() {
        char currentChar = this.source_code[pos++];
        if (currentChar == '\n') {
            line++;
            column = 1;
        } else {
            column++;
        }
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
}
