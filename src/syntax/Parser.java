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
	
	public void E() throws Exception {
		T();
		El();		
	}
	
	public void El() throws Exception {
		this.token = this.scanner.nextToken();
		if(this.token != null) {			
			OP();
			T();
			El();
		}
	}
	
	public void T() throws Exception {
		this.token = this.scanner.nextToken();
		if(this.token.getType() != TokenType.IDENTIFIER && this.token.getType() != TokenType.NUMBER)
			throw new ParserException("Expected IDENTIFIER or NUMBER, found " + this.token.getType() + ": " + this.token.getContent() + " na linha x coluna y");
	}
	
	public void OP() throws Exception {
		if(this.token.getType() != TokenType.MATH_OP)
			throw new ParserException("Expected MATH_OPERATOR, found " + this.token.getType() + ": " + this.token.getContent());
	}

}
