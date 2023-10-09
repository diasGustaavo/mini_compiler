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

    public void toNextToken() throws Exception {
        this.token = this.scanner.nextToken();
        System.out.println(this.token.getContent() + " Tipo: " + this.token.getType() + " line: " + this.token.getLine() + " column: " + this.token.getColumn());
    }

	public void match(Token token, TokenType type) {
		if (this.token != null) {
			if (token.getType() != type) {
				throw new ParserException("Type " + type + " expected, found " + token.getType() + " with value: " +  token.getContent() + " : " + "[line:" + this.token.getLine()  + " ] [column:"+ this.token.getColumn() + "]");
			}
		} else {
			throw new ParserException("Type " + type + " expected, found null"  + ": " + "[line:" + this.token.getLine()  + " ] [column:"+ this.token.getColumn() + "]");
		}

	}

	public void match(Token token, Keywords keyword) {
		if(this.token != null) {
			if (token.getContent().intern() != keyword.toString().intern()) {
				throw new ParserException("Keyword " + keyword + " expected, found " + token.getContent() + ": " + "[line:" + this.token.getLine()  + " ] [column:"+ this.token.getColumn() + "]");
			}
		} else {
			throw new ParserException("Keyword " + keyword + " expected, found null" + ": " + "[line:" + this.token.getLine()  + " ] [column:"+ this.token.getColumn() + "]");
		}

	}

	public void programa() throws Exception {
		this.toNextToken();
		match(this.token, TokenType.TWO_POINTS);
		this.toNextToken();
		match(this.token, Keywords.STATEMENTS);
        this.toNextToken();
		listaDeclaracoes();	

		match(this.token, TokenType.TWO_POINTS);
		this.toNextToken();
        System.out.println("erro depois daqui");
		match(this.token, Keywords.ALGORITHM);
        System.out.println("erro depois daqui 2");

        this.toNextToken();
		listaComandos();
        System.out.println("erro depois daqui 3");

		this.toNextToken();
        System.out.println("erro depois daqui 4");
		match(this.token, TokenType.DELIM);
        System.out.println("erro depois daqui 5");
	}

	public void listaDeclaracoes() throws Exception {
        declaracao();
        this.toNextToken();
        match(this.token, TokenType.DELIM);
        this.toNextToken();

        if (this.token.getType() != TokenType.TWO_POINTS) {
            listaDeclaracoes();
        }
	}

    public void declaracao() throws Exception {
        match(this.token, TokenType.IDENTIFIER);
        this.toNextToken();
        match(this.token, TokenType.TWO_POINTS);
        this.toNextToken();
        tipoVar();
    }

    public void tipoVar() {
        if(
            this.token.getContent().intern() != Keywords.INT.toString().intern() 
            && this.token.getContent().intern() != Keywords.FLOAT.toString()
        ) {
            throw new ParserException("Expected INT or FLOAT and found " + token.getContent() + ": " + "[line:" + this.token.getLine()  + " ] [column:"+ this.token.getColumn() + "]");  
        }
    }

    public void listaComandos() throws Exception {
		comando();
		this.toNextToken();

        if (this.token != null) {
            listaComandos();
        }
	}

    public void comando() throws Exception {
        if (token.getType().toString().intern().equals("IDENTIFIER")) {
			comandoAtribuicao();
		} else if (token.getContent().intern().equals(Keywords.INPUT.toString().intern())) {
            comandoEntrada();
        } else if (token.getContent().intern().equals(Keywords.PRINT.toString().intern())) {
            comandoSaida();
        } else if (token.getContent().intern().equals(Keywords.IF.toString().intern())) {
            comandoCondicao();
        } else if (token.getContent().intern().equals(Keywords.WHILE.toString().intern())) {
            comandoRepeticao();
        } else if (token.getContent().intern().equals(Keywords.BEGIN.toString().intern())) {
            subAlgoritmo();
        } else {
            throw new ParserException("Expected ASSING, INPUT, PRINT, IF or WHILE and found " + token.getContent() + ": " + "[line:" + this.token.getLine()  + " ] [column:"+ this.token.getColumn() + "]");
        }
	}

    public void comandoAtribuicao() throws Exception {
        match(this.token, TokenType.IDENTIFIER);
		this.toNextToken();
		match(this.token, TokenType.ASSIGN);
		this.toNextToken();
		// expressaoAritmetica();
	}

    public void comandoEntrada() throws Exception {
        match(this.token, Keywords.INPUT);
		this.toNextToken();
		match(this.token, TokenType.IDENTIFIER);
        this.toNextToken();
	}

    public void comandoSaida() throws Exception {
        match(this.token, Keywords.PRINT);
		this.toNextToken();
        match(this.token, TokenType.LEFT_PARENTHESIS);
        this.toNextToken();

		if (token.getType() != TokenType.IDENTIFIER && token.getType() != TokenType.STRING) {
			throw new ParserException("Identyfier or String expected, found " + token.getType() + ": " + "[line:" + this.token.getLine()  + " ] [column:"+ this.token.getColumn() + "]");
		}

        this.toNextToken();
        match(this.token, TokenType.RIGHT_PARENTHESIS);
        this.toNextToken();
        match(this.token, TokenType.DELIM);
	}

    public void comandoCondicao() throws Exception {
        match(this.token, Keywords.IF);
        this.toNextToken();
        expressaoRelacional();
		this.toNextToken();
		match(this.token, Keywords.THEN);
        this.toNextToken();
		comando();
		
        this.toNextToken();
        if (this.token.getContent().toString().intern() == Keywords.ELSE.toString().intern()) {
            comando();
        }
    }

    private void expressaoRelacional() {
        // implementar
    }

    public void comandoRepeticao() throws Exception {
        match(this.token, Keywords.WHILE);
        this.toNextToken();
		// expressaoRelacional(null);
        this.toNextToken();
		comando();
	}

    public void subAlgoritmo() throws Exception {
        match(this.token, Keywords.BEGIN);
        this.toNextToken();
        this.listaComandos();
        this.toNextToken();
        match(this.token, Keywords.END);
    }


	public void expressaoAritmetica() throws Exception {
        termoAritmetico();
        expressaoAritmetica_2();
        expressaoAritmetica_3();
        match(this.token, TokenType.DELIM);
	}

    public void expressaoAritmetica_2() throws Exception {
        match(this.token, TokenType.SUM_OP);
        this.toNextToken();
        termoAritmetico();
        this.toNextToken();

        if (this.token != null && this.token.getType().toString().intern() != TokenType.DELIM.toString().intern()) {
            expressaoAritmetica_2();
        }
    }

    public void expressaoAritmetica_3() throws Exception {
        match(this.token, TokenType.SUB_OP);
        this.toNextToken();
        termoAritmetico();
        this.toNextToken();
        expressaoAritmetica_2();
        this.toNextToken();

        if (this.token != null && this.token.getType().toString().intern() != TokenType.DELIM.toString().intern()) {
            expressaoAritmetica_3();
        }
    }

    public void termoAritmetico() {

    }
}
