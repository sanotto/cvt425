package com.nixartech.cvt425;

public class RPGSpecTypeP extends RPGSpec {

	@Override
	public RPGConvertionResult parse(String input, int indent) {
		
		RPGConvertionResult output = new RPGConvertionResult();
		
		output.remove = false;
		output.change = false;
		output.value="";
		output.beforeSpaces=0;
		output.nextSpaces=0;
		
		var prevName="";
		var name = input.substring(7, 23).trim();
		var keywords = input.substring(44).trim();

		input = input.stripTrailing();

		if (!prevName.contentEquals("")) {
		      name = prevName;
		      prevName = "";
	    }
		
	    if (input.endsWith("...")) {
		      prevName = input.substring(7, input.length() - 10).trim();
		      output.remove = true;
	    } 
	    else {
	    	  String boe =input.substring(24,25).toUpperCase(); 
		      switch (boe) {
		        case "B":
		          output.value = ("Dcl-Proc " + name + " " + keywords).stripTrailing();
		          output.nextSpaces = indent ;
		          break;
		        case "E":
		          output.beforeSpaces = -indent;
		          output.value = "End-Proc";
		          break;
		      }
	    }

	    if (!output.value.contentEquals("")) {
		      output.change = true;
		      output.value = output.value.stripTrailing() + ';';
	    }
		
	    return output;
	}
}
