package lexical;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.HashMap;
import java.util.Map;

import utils.TokenType;

public class Scanner {

	int pos;
	char[] source_code;
	int state;
	public int line;
	public int column;

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
		this.state = 0;
		String content = "";
		char currentChar;

		while (true) {
			if (isEOF()) {
				return null;
			}
			
			currentChar = this.nextChar();

			if (currentChar == '#') {
				while (!this.isEndOfLine(currentChar)) {
					currentChar = this.nextChar();
				}

				this.line++;
				this.column = 1;
				state = 0;
			}

			switch (state) {
				case 0:
					if (this.isLetter(currentChar) || this.isUnderscore(currentChar)) {
						content += currentChar;
						state = 1;
					} else if (isSpace(currentChar)) {
                        // System.out.println(this.source_code[pos]);
                        if (isEndOfLine(this.source_code[pos])) {
                            this.line++;
                            this.column = 1;
                        }
						state = 0;
					} else if (isDigit(currentChar)) {
						content += currentChar;
						state = 2;
					} else if (isMathOperator(currentChar)) {
						content += currentChar;
						state = 3;
					} else if (isEquals(currentChar)) {
						content += currentChar;
						state = 4;
					} else if (isLess(currentChar)) {
						content += currentChar;
						state = 5;
					} else if (isGreater(currentChar)) {
						content += currentChar;
						state = 6;
					} else if (isExclamation(currentChar)) {
						content += currentChar;
						state = 7;
					} else if(isLeftParenthesis(currentChar)) {
						content += currentChar;

                        return this.returnedTokenInEndOfLine(currentChar, TokenType.LEFT_PARENTHESIS, content);
					} else if(isRightParenthesis(currentChar)) {
						content += currentChar;

                        return this.returnedTokenInEndOfLine(currentChar, TokenType.RIGHT_PARENTHESIS, content);
					} else if (isDot(currentChar)) {
						content += currentChar;
						state = 8;
					} else {
						throw new Exception("Unrecognized symbol \'" + currentChar + "\' at line " + line + ", column " + column);
					}
					break;
				case 1:
					if (this.isLetter(currentChar) || this.isDigit(currentChar) || this.isUnderscore(currentChar)) {
						content += currentChar;
						state = 1;
					} else {
						if (!isEndOfLine(currentChar)) this.back();
						if (isSpace(currentChar) || isMathOperator(currentChar)) {
							TokenType type = reservedWords.getOrDefault(content, TokenType.IDENTYFIER);
							return this.returnedTokenInEndOfLine(currentChar, type, content);
						}

						throw new Exception("Unrecognized symbol \'" + currentChar + "\' at line " + line + ", column " + column);
					}
					break;
				case 2:
					if(isDigit(currentChar)) {
						content += currentChar;
						state = 2;
					} else if(currentChar == '.') {
						content += currentChar;
						state = 8;
					} else	if (isSpace(currentChar) || isEndOfLine(currentChar) || isRightParenthesis(currentChar)) {
                        return this.returnedTokenInEndOfLine(currentChar, TokenType.NUMBER, content);
					} else if (isMathOperator(currentChar)) {
						this.back();
                        return this.returnedTokenInEndOfLine(currentChar, TokenType.NUMBER, content);
					} else {
						throw new Exception("Unrecognized symbol \'" + currentChar + "\' at line " + line + ", column " + column);
					}
					break;
					
				case 3: 
					return getOperationToken(content, currentChar);
				case 4: 
					if (isEquals(currentChar) && isEquals(this.source_code[this.pos])) {
						this.nextChar();
						throw new Exception("Unrecognized symbol \'" + currentChar + "\' at line " + line + ", column " + column);

					} else if(isEquals(currentChar) && !isEquals(this.source_code[this.pos])) {
						content += currentChar;
                        return this.returnedTokenInEndOfLine(currentChar, TokenType.EQUALS_OP, content);
					} else {
						if (!isEndOfLine(currentChar)) this.back();
                        return this.returnedTokenInEndOfLine(currentChar, TokenType.ASSIGN_OP, content);
					}

				case 5:
					if (isEquals(currentChar)) {
						content += currentChar;
                        return this.returnedTokenInEndOfLine(currentChar, TokenType.LESS_EQUALS_OP, content);
					} else {
						if (!isEndOfLine(currentChar)) this.back();
                        return this.returnedTokenInEndOfLine(currentChar, TokenType.LESS_OP, content);
					}
				
				case 6:
					if (isEquals(currentChar)) {
						content += currentChar;
                        return this.returnedTokenInEndOfLine(currentChar, TokenType.GREATER_EQUALS_OP, content);
					} else {
						if (!isEndOfLine(currentChar)) this.back();
                        return this.returnedTokenInEndOfLine(currentChar, TokenType.GREATER_OP, content);
					}

				case 7:
					if (isEquals(currentChar)) {
						content += currentChar;
                        return this.returnedTokenInEndOfLine(currentChar, TokenType.DIF_OP, content);
					} else {
						if (!isEndOfLine(currentChar)) this.back();
						throw new Exception("Operator ! don't is supported [line:" + line  + " ] [column:"+ column + "]");
					}
				case 8:
					if (isDigit(currentChar)) {
						content += currentChar;
						state = 9;
					} else {
						this.back();
						throw new Exception("Error: Invalid Character for Number [line:" + line  + " ] [column:"+ column + "]");
					}
					break;

				case 9: 
					if (isDigit(currentChar)) {
						content += currentChar;
						state = 9;
					} else if (isSpace(currentChar) || isEndOfLine(currentChar)) {
                        return this.returnedTokenInEndOfLine(currentChar, TokenType.NUMBER, content);
					} else {
						throw new Exception("Unrecognized symbol for number \'" + currentChar + "\' at line " + line + ", column " + column);
					}
					break;
			}
		
            
        }
	}

    private Token returnedTokenInEndOfLine(char currentChar, TokenType type, String content) {
        Token newToken = new Token(type, content, this.line, this.column);

        if (this.isEndOfLine(currentChar) || this.isEndOfLine(this.source_code[pos])) {
            this.line++;
            this.column = 1;
        }

        return newToken;
    }

	private char nextChar() {
		char currentChar = this.source_code[pos++];
    	column++;

        return currentChar;
		
	}

	private void back() {
		this.column--;
		this.pos--;
	}

	private boolean isLetter(char currentChar) {
		return (currentChar >= 'a' && currentChar <= 'z') || (currentChar >= 'A' && currentChar <= 'Z');
	}

	private boolean isDigit(char currentChar) {
		return currentChar >= '0' && currentChar <= '9';
	}

	private boolean isUnderscore(char currentChar) {
		return currentChar == '_';
	}

	private boolean isLess(char currentChar) {
		return currentChar == '<';
	}

	private boolean isGreater(char currentChar) {
		return currentChar == '>';
	}

	private boolean isDot(char currentChar) {
		return currentChar == '.';
	}

	private boolean isExclamation(char currentChar) {
		return currentChar == '!';
	}

	private boolean isSpace(char currentChar) {
		return currentChar == ' ' || currentChar == '\n' || currentChar == '\t' || currentChar == '\r';
	}

	private boolean isEndOfLine(char currentChar) {
		return currentChar == '\n' || currentChar == '\r';
	}

	private boolean isEquals(char currentChar) {
		return currentChar == '=';
	}

	private boolean isMathOperator(char currentChar) {
		return currentChar == '+' || currentChar == '-' || currentChar == '*' || currentChar == '/';
	}

	private boolean isEOF() {
		if (this.pos >= this.source_code.length) {
			return true;
		}
		return false;
	}

	private Token getOperationToken(String currentString, char currentChar) {
		this.back();

		switch (currentString) {
			case "+":
                return returnedTokenInEndOfLine(currentChar, TokenType.SUM_OP, currentString);
			case "-":
                return returnedTokenInEndOfLine(currentChar, TokenType.SUB_OP, currentString);
			case "*":
                return returnedTokenInEndOfLine(currentChar, TokenType.MULT_OP, currentString);
			default: 
                return returnedTokenInEndOfLine(currentChar, TokenType.DIV_OP, currentString);
		}

	}

	private boolean isLeftParenthesis(char c) {
		return c == '(';
	}
	
	private boolean isRightParenthesis(char c) {
		return c == ')';
	}

}
