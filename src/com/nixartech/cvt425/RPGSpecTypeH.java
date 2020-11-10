package com.nixartech.cvt425;

public class RPGSpecTypeH extends RPGSpec {

	public RPGConvertionResult parse(String input, int indent) {
		RPGConvertionResult output = new RPGConvertionResult();
		
		output.remove = false;
		output.change = false;
		output.value="";
		output.beforeSpaces=0;
		output.nextSpaces=0;
		
		var keywords = input.substring(7);
	    output.value = "Ctl-Opt " + keywords.trim();

	      if (!output.value.equals("")) {
	          output.change = true;
	          output.value = output.value.stripTrailing() + ';';
	      }
	      return output;
	  }
}