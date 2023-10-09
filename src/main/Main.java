package main;

import exceptions.ParserException;
import exceptions.ScannerException;
import lexical.Scanner;
import lexical.Token;
import syntax.Parser;

public class Main {
	public static void main(String[] args) {
		Scanner sc = new Scanner("programa_checkpoint2.mc");
		// Token tk;
		// try {
		// 	do {
		// 		tk = sc.nextToken();
		// 		System.out.println(
		// 			"TYPE: " + tk.getType() +
		// 			" | CONTENT: " + tk.getContent() +
		// 			" | LINE: " + tk.getLine() +
		// 			" | COLUMN: " + tk.getColumn()
		// 		);

		// 	} while (tk != null);
		// } catch (Exception e) {
		// 	e.printStackTrace();
		// }
		Parser parser = new Parser(sc);
		try {
			parser.programa();
			System.out.println("Compilation Successful!");
		} catch (ScannerException e) {
			System.out.println("Lexical Error: " + e.getMessage());
		} catch (ParserException e) {
			System.out.println("Syntax Error: " + e.getMessage());
		} catch (Exception e) {
			System.out.println("Generic Error: " + e.getMessage());
		}
	}
}
