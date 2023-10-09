package syntax;

import exceptions.ParserException;
import lexical.Scanner;
import lexical.Token;
import utils.Keywords;
import utils.TokenType;

public class Parser {
	private Scanner scanner;
	private Token token;
	
	public Parser(Scanner scanner) {
		this.scanner = scanner;
	}
	
    public void match(Token token, TokenType type) {
        System.out.println("TOKEN: " + token.getContent() + "   " + "TYPE: " + type.toString().intern());
        if (this.token != null) {
            if (this.token.getType() != type) {
                throw new ParserException("Type " + type + " expected, found " + token.getType() + " with value: " +  token.getContent().intern() + " : " + "[line:" + this.token.getLine()  + " ] [column:"+ this.token.getColumn() + "]");
            }

			return;
        }

        throw new ParserException("Type " + type + " expected, found null"  + ": " + "[line:" + this.token.getLine()  + " ] [column:"+ this.token.getColumn() + "]");
    }

	public void match(Token token, Keywords keyword) {
        System.out.println("TOKEN: " + token.getContent() + "   " + "KEYWORD: " + keyword.toString().intern());
		if (this.token != null) {
			if (this.token.getContent().intern() != keyword.toString().intern()) {
				throw new ParserException("Keyword " + keyword + " expected, found " + token.getType() + " with value: " +  token.getContent().intern() + " : " + "[line:" + this.token.getLine()  + " ] [column:"+ this.token.getColumn() + "]");
			}

			return;
		}

		throw new ParserException("Keyword " + keyword + " expected, found null"  + ": " + "[line:" + this.token.getLine()  + " ] [column:"+ this.token.getColumn() + "]");
	}

    public void programa() throws Exception {
		this.token = this.scanner.nextToken();
		match(this.token, TokenType.TWO_POINTS);
		this.token = this.scanner.nextToken();
		match(this.token, Keywords.STATEMENTS);
		this.token = this.scanner.nextToken();
		listaDeclaracoes();	

		match(this.token, TokenType.TWO_POINTS);
		this.token = this.scanner.nextToken();
		match(this.token, Keywords.ALGORITHM);

		this.token = this.scanner.nextToken();
		listaComandos();
	}

	public void listaDeclaracoes() throws Exception {
        declaracao();
	}

	public void declaracao() throws Exception {
        match(this.token, TokenType.IDENTIFIER);
        this.token = this.scanner.nextToken();
        match(this.token, TokenType.TWO_POINTS);
        tipoVar();

		this.token = this.scanner.nextToken();
		if (this.token.getType() != TokenType.TWO_POINTS) {
			listaDeclaracoes();
        }
    }

	public void tipoVar() throws Exception { 
        this.token = this.scanner.nextToken();

        if(
            this.token.getContent().intern() != Keywords.INT.toString().intern()
            && this.token.getContent().intern() != Keywords.FLOAT.toString().intern()
        ) {
            throw new ParserException("Expected INT or FLOAT and found " + token.getContent().intern());  
        }
    }

	public void listaComandos() throws Exception {
		comando();
	}

	public void comando() throws Exception {
		if (token.getType().toString().intern() == "IDENTIFIER") {
			comandoAtribuicao();
		} else if (token.getContent().intern() == Keywords.INPUT.toString().intern()) {
			comandoEntrada();
		} else if (token.getContent().intern() == Keywords.PRINT.toString().intern()) {
            comandoSaida();
        } else if (token.getContent().intern() == Keywords.IF.toString().intern()) {
            comandoCondicao();
        } else if (token.getContent().intern() == Keywords.WHILE.toString().intern()) {
            comandoRepeticao();
        } else {
            throw new ParserException("Expected ASSIGN, INPUT, PRINT, IF or WHILE and found " + token.getContent().intern());
        }

        this.token = this.scanner.nextToken();
		if (this.token != null) {
			listaComandos();
		} 
	}

	public void comandoRepeticao() throws Exception {
		expressaoRelacional();
		comando();
        
		this.token = this.scanner.nextToken();
		if (this.token != null) {
			comando();
		}
	}

	public void comandoCondicao() throws Exception {
        match(this.token, Keywords.IF);
        this.token = this.scanner.nextToken();
		expressaoRelacional();
		match(this.token, Keywords.THEN);
		comando();

		this.token = this.scanner.nextToken();
		if (this.token.getContent().intern() == Keywords.ELSE.toString().intern()) {
			comando();
		}
	}

	public void expressaoRelacional() throws Exception {
        termoRelacional();
        expressaoRelacional2();
	}

    public void expressaoRelacional2() throws Exception {
        if (this.token.getType() == TokenType.IDENTIFIER) {
            this.token = this.scanner.nextToken();
        }

        if (
			this.token != null
			&& (this.token.getContent().intern() == Keywords.AND.toString().intern()
			|| this.token.getContent().intern() == Keywords.OR.toString().intern())) {
            operadorBooleano();
            this.token = this.scanner.nextToken();
            expressaoRelacional();
        }
    }

    public void termoRelacional() throws Exception {
        if (this.token.getType() == TokenType.LEFT_PARENTHESIS) {
            match(this.token, TokenType.LEFT_PARENTHESIS);
            this.token = this.scanner.nextToken();
            expressaoRelacional();
            this.token = this.scanner.nextToken();
            match(this.token, TokenType.RIGHT_PARENTHESIS);
            return;
        }

        expressaoAritmetica();
        operadorRelacional();

        this.token = this.scanner.nextToken();
        expressaoAritmetica();
    }

    public void operadorRelacional() throws Exception {
        rel_op();
    }

    private void rel_op() throws Exception {
        if (this.token.getType() == TokenType.IDENTIFIER) {
            this.token = this.scanner.nextToken();
        }
        TokenType type = this.token.getType();

        if (type != TokenType.LESS && type != TokenType.LESS_EQUALS && type != TokenType.EQUALS && type != TokenType.GREATER && type != TokenType.GREATER_EQUALS && type != TokenType.DIF_OP) {
            throw new ParserException("Type REL_OP expected, found " + token.getType() + " with value: " + token.getContent().intern());
        }
    }

    public void operadorBooleano() {
        if(
            this.token.getContent().intern() != Keywords.AND.toString().intern() 
            && this.token.getContent().intern() != Keywords.OR.toString().intern()
        ) {
            throw new ParserException("Expected AND or OR and found " + token.getContent().intern());  
        }
    }

	public void comandoSaida() throws Exception {
		this.token = this.scanner.nextToken();
		if (token.getType() != TokenType.IDENTIFIER) {
			throw new ParserException("Identyfier or String expected, found " + token.getType());
		}

        this.token = this.scanner.nextToken();
	}

	public void comandoEntrada() throws Exception{
		this.token = this.scanner.nextToken();
		match(this.token, TokenType.IDENTIFIER);
	}

	public void comandoAtribuicao() throws Exception {
		match(this.token, TokenType.IDENTIFIER);
		this.token = this.scanner.nextToken();
		match(this.token, TokenType.ASSIGN);
		this.token = this.scanner.nextToken();
		expressaoAritmetica();
	}

	public void expressaoAritmetica() throws Exception {
        termoAritmetico();
        expressaoAritmetica2();
	}

    public void expressaoAritmetica2() throws Exception {
        if (this.token != null && (this.token.getType() == TokenType.SUM_OP || this.token.getType() == TokenType.SUB_OP)) {
            expressaoAritmetica3();
            expressaoAritmetica2();
        } 
	}

	public void expressaoAritmetica3() throws Exception {
        if (this.token.getType() == TokenType.SUM_OP) {
            match(this.token, TokenType.SUM_OP);
            this.token = this.scanner.nextToken();
            termoAritmetico();

            return;
        }

        match(this.token, TokenType.SUB_OP);
        this.token = this.scanner.nextToken();
        termoAritmetico();
	}

	public void fatorAritmetico() throws Exception {
        if (
            this.token.getType() != TokenType.NUMBER &&
            this.token.getType() != TokenType.IDENTIFIER
        ) {
            if (this.token.getType() != TokenType.LEFT_PARENTHESIS) {
                throw new ParserException("Expected INT, FLOAT, IDENTYFIER or (EXPRESSION) and found " + token.getContent().intern());  
            }

            match(this.token, TokenType.LEFT_PARENTHESIS);
            this.token = this.scanner.nextToken();
            expressaoAritmetica();
            match(this.token, TokenType.RIGHT_PARENTHESIS);
        }
    } 
	
	public void termoAritmetico () throws Exception {
        fatorAritmetico();
        termoAritmetico2();
    }

    public void termoAritmetico2 () throws Exception {
        if (this.token.getType() != TokenType.IDENTIFIER) {
            this.token = this.scanner.nextToken();
        }

        if (
            this.token != null && (
                this.token.getType() == TokenType.MULT_OP
                || this.token.getType() == TokenType.DIV_OP
                )
        ) {
            termoAritmetico3();
            termoAritmetico2();
        }

    }

    public void termoAritmetico3 () throws Exception {
        if (this.token.getType() == TokenType.MULT_OP) {
            match(this.token, TokenType.MULT_OP);
            this.token = this.scanner.nextToken();
            fatorAritmetico();
            return;
        }

        match(this.token, TokenType.DIV_OP);
        this.token = this.scanner.nextToken();
        fatorAritmetico();
    }
}
