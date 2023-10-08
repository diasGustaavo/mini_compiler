package syntax;

import exceptions.ParserException;
import lexical.Scanner;
import lexical.Token;
import utils.TokenType;

public class Parser {
	private Scanner scanner;
	private Token token;
	
	public Parser(Scanner scanner) {
		this.scanner = scanner;
	}
	
    public void match(Token token, TokenType type) {
        if (this.token != null) {
            if (this.token.getType() != type) {
                throw new ParserException("Type " + type + " expected, found " + token.getType() + " with value: " +  token.getContent() + " : " + "[line:" + this.token.getLine()  + " ] [column:"+ this.token.getColumn() + "]");
            }

            return;
        }

        throw new ParserException("Type " + type + " expected, found null"  + ": " + "[line:" + this.token.getLine()  + " ] [column:"+ this.token.getColumn() + "]");
    }

    public void Programa() throws Exception {
        this.token = this.scanner.nextToken();
    }
}