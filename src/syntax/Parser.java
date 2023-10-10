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
		match(this.token, Keywords.ALGORITHM);

        this.toNextToken();
		listaComandos();
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
        if (token.getType().toString().intern().equals(TokenType.IDENTIFIER.toString().intern())) {
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
        System.out.println("ENTROU no COMANDO comandoAtribuicao");
        match(this.token, TokenType.IDENTIFIER);
		this.toNextToken();
		match(this.token, TokenType.ASSIGN);
		this.toNextToken();
		expressaoAritmetica();
        System.out.println("SAIU no COMANDO comandoAtribuicao");
	}

    public void comandoEntrada() throws Exception {
        System.out.println("ENTROU no COMANDO comandoEntrada");
        match(this.token, Keywords.INPUT);
		this.toNextToken();
		match(this.token, TokenType.IDENTIFIER);
        this.toNextToken();
        System.out.println("SAIU no COMANDO comandoEntrada");
	}

    public void comandoSaida() throws Exception {
        System.out.println("ENTROU no COMANDO comandoSaida");
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
        System.out.println("SAIU no COMANDO comandoSaida");
	}

    public void comandoCondicao() throws Exception {
        System.out.println("ENTROU no COMANDO CONDICAO");
        match(this.token, Keywords.IF);
        this.toNextToken();
        expressaoRelacional();
		this.toNextToken();
		comando();
		
        this.toNextToken();
        if (this.token.getContent().toString().intern() == Keywords.ELSE.toString().intern()) {
            comando();
        }
        
        System.out.println("SAIU no comandoCondicao");
    }

    private void expressaoRelacional() throws Exception {
        System.out.println("ENTROU no expressaoRelacional");
        termoRelacional();
        expressaoRelacional_2();
        System.out.println("SAIU no expressaoRelacional");
    }

    private void expressaoRelacional_2() throws Exception {
         System.out.println("ENTROU no expressaoRelacional_2");
        if (this.token.getContent().intern() != Keywords.THEN.toString().intern()) {
            operadorBooleano();
            expressaoRelacional();
        }
        System.out.println("SAIU no expressaoRelacional_2");

    }

    private void operadorBooleano() {
        System.out.println("ENTROU no operadorBooleano");        

        if (
            this.token.getContent().toString().intern() != Keywords.AND.toString().intern()
            && this.token.getContent().toString().intern() != Keywords.OR.toString().intern()
        ) {
            throw new ParserException("Expected AND or OR operator and found " + token.getContent() + ": " + "[line:" + this.token.getLine()  + " ] [column:"+ this.token.getColumn() + "]");
        }
         System.out.println("SAIU no operadorBooleano");
    }

    private void termoRelacional() throws Exception {
        System.out.println("ENTROU no termoRelacional");
        if (this.token.getType().toString().intern() == TokenType.LEFT_PARENTHESIS.toString().intern()) {
            match(this.token, TokenType.LEFT_PARENTHESIS);
            expressaoRelacional();
            match(this.token, TokenType.RIGHT_PARENTHESIS);

            return;
        }

        expressaoAritmetica();
        op_rel();
        expressaoAritmetica();

        System.out.println("SAIU no termoRelacional");
    }

    private void op_rel() throws Exception {
         System.out.println("ENTROU no op_rel");
        TokenType type = this.token.getType();
        if (type != TokenType.LESS && type != TokenType.LESS_EQUALS && type != TokenType.EQUALS && type != TokenType.GREATER && type != TokenType.GREATER_EQUALS && type != TokenType.DIF_OP) {
            throw new ParserException("Type REL_OP expected, found " + token.getType() + " with value: " + token.getContent());
        }

        this.toNextToken();
       System.out.println("SAIU no op_rel");

    }

    public void comandoRepeticao() throws Exception {
        System.out.println("ENTROU no comandoRepeticao");
        match(this.token, Keywords.WHILE);
        this.toNextToken();
		expressaoRelacional();
        this.toNextToken();
		comando();
        System.out.println("SAIU no comandoRepeticao");
	}

    public void subAlgoritmo() throws Exception {
        System.out.println("ENTROU no subAlgoritmo");
        match(this.token, Keywords.BEGIN);
        this.toNextToken();
        this.listaComandos();
        this.toNextToken();
        match(this.token, Keywords.END);
        System.out.println("SAIU no subAlgoritmo");
    }

	public void expressaoAritmetica() throws Exception {
         System.out.println("ENTROU no expressaoAritmetica");
        termoAritmetico();
        expressaoAritmetica_2();
        System.out.println("SAIU no expressaoAritmetica");
	}

    public void expressaoAritmetica_2() throws Exception {
         System.out.println("ENTROU no expressaoAritmetica_2");
        if (this.token != null && (this.token.getType() == TokenType.SUM_OP || this.token.getType() == TokenType.SUB_OP)) {
            expressaoAritmetica_3();
            expressaoAritmetica_2();
        }

        this.toNextToken();

        System.out.println("SAIU no expressaoAritmetica");
    }

    public void expressaoAritmetica_3() throws Exception {
         System.out.println("ENTROU no expressaoAritmetica_3");
        if (
            this.token.getType().toString().intern() != TokenType.SUB_OP.toString().intern()
            && this.token.getType().toString().intern() != TokenType.SUM_OP.toString().intern()
        ) {
            throw new ParserException("Expected SUM or SUB operation and found " + token.getContent() + ": " + "[line:" + this.token.getLine()  + " ] [column:"+ this.token.getColumn() + "]");
        }

        termoAritmetico();

        System.out.println("SAIU no expressaoAritmetica_3");
    }

    public void termoAritmetico() throws Exception {
        System.out.println("ENTROU no termoAritmetico");
        fatorAritmetico();
        termoAritmetico_2();
         System.out.println("SAIU no termoAritmetico");
    }

    public void termoAritmetico_2() throws Exception {
        System.out.println("ENTROU no termoAritmetico_2");
        if (this.token != null && (this.token.getType() == TokenType.MULT_OP || this.token.getType() == TokenType.DIV_OP)) {
            termoAritmetico_3();
            termoAritmetico_2();
        }
        System.out.println("SAIU no termoAritmetico_2");

    }

    public void termoAritmetico_3() throws Exception {
        System.out.println("ENTROU no termoAritmetico_3");
        if (
            this.token.getType().toString().intern() != TokenType.MULT_OP.toString().intern()
            && this.token.getType().toString().intern() != TokenType.DIV_OP.toString().intern()
        ) {
            throw new ParserException("Expected MULT or DIV operation and found " + token.getContent() + ": " + "[line:" + this.token.getLine()  + " ] [column:"+ this.token.getColumn() + "]");
        }

        fatorAritmetico();
        System.out.println("ENTROU no termoAritmetico_3");

    }

    public void fatorAritmetico() throws Exception {
        System.out.println("ENTROU no fatorAritmetico");
        if (this.token.getType().toString().intern() == TokenType.LEFT_PARENTHESIS.toString().intern()) {
            match(this.token, TokenType.LEFT_PARENTHESIS);
            expressaoAritmetica();
            match(this.token, TokenType.RIGHT_PARENTHESIS);

            return;
        }

        if (
            this.token.getType().toString().intern() != TokenType.IDENTIFIER.toString().intern()
            && this.token.getType().toString().intern() != TokenType.NUMBER.toString().intern()
        ) {
            throw new ParserException("Expected Identifier or Number and found " + token.getContent() + ": " + "[line:" + this.token.getLine()  + " ] [column:"+ this.token.getColumn() + "]");
        }
        System.out.println("SAIU no fatorAritmetico");
    }
}
