package com.nixartech.cvt425;

import java.io.BufferedReader;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.io.PrintWriter;
import java.io.UnsupportedEncodingException;
import java.util.LinkedList;

public class App {
	static public void main(String[] args) throws IOException {
		LinkedList <String> lines = readAllLines("buildr4.rpgle");
		int indent=2;
		RPGConverter rpgConverter = new RPGConverter(lines, indent);
		rpgConverter.parse();
		
		putAllLines(lines, "buildr4.rpglefree");
	}
	
	public static LinkedList<String> readAllLines(String path) throws IOException {
		LinkedList<String> lines = new LinkedList<String>();
		String filename = path;
		BufferedReader reader = new BufferedReader(new FileReader(filename));
		String line= reader.readLine();
		while(line != null) {			
			lines.add(line);
			line= reader.readLine();
		}
		
		reader.close();
		return lines;
	}
	
	public static void putAllLines(LinkedList<String> lines, String path) throws FileNotFoundException, UnsupportedEncodingException {
		String filename = path;
		PrintWriter writer = new PrintWriter(filename,"UTF-8");
		for(String line: lines) {
			writer.write(line+"\n");
		}
		writer.close();				
	}	
}
