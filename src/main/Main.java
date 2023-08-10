package main;

import lexical.Scanner;
import lexical.Token;

public class Main {
	public static void main(String[] args) {
		Scanner sc = new Scanner("source_code.mc");
		Token tk;
		try {
			do {
				tk = sc.nextToken();
				System.out.println(
					"TYPE: " + tk.getType() +
					" | CONTENT: " + tk.getContent() +
					" | LINE: " + tk.getLine() +
					" | COLUMN: " + tk.getColumn()
				);

			} while (tk != null);
		} catch (Exception e) {
			e.printStackTrace();
		}
	}
}
