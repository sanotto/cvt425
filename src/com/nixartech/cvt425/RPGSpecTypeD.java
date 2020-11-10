package com.nixartech.cvt425;

public class RPGSpecTypeD extends RPGSpec {

	static boolean isSubf = false;
	static String prevName = "";
	static boolean  DSisQualified = false;
	static boolean DSisLIKEDS = false;

	
	
	@Override
	public RPGConvertionResult parse(String input, int indent) {
	
		RPGConvertionResult output = new RPGConvertionResult();
		output.rvar.standalone=false;
		output.rvar.name="";
		output.rvar.type="";
		output.rvar.len=0;
		
		
		var potentialName = input.substring(7).trim();
	    var name = input.substring(7, 24).trim();
	    var pos = input.substring(30, 33).trim();
	    var len = input.substring(33, 40).trim();
	    var type = input.substring(40, 41).trim();
	    var decimals = input.substring(41, 44).trim();
	    var field = input.substring(24, 26).trim().toUpperCase();
	    var keywords = input.substring(44).trim();

		output.rvar.standalone = false;
	    if (field.contentEquals("S")) {
	    	output.rvar.standalone = true;	   
	    	output.rvar.name = name;
	    	output.rvar.type = type;
	    	try {
	    		output.rvar.len = Integer.parseInt(len);
	    	}catch (Exception e) {
	    		output.rvar.len=null;
			}
	    }
	    
	    if (keywords.endsWith("+")) {
	    	keywords = keywords.substring(0, keywords.length()-1);
	    }

	    if (type.contentEquals("")) {
	      if (decimals.contentEquals("")) {
	    	  output.rvar.type = "A"; //Character
	      }
	      else {
	        output.rvar.type = "S"; //Zoned
	      }
	    }

	    if (!pos.contentEquals("")) {
	    	var l = Integer.parseInt(len) - Integer.parseInt(pos) + 1;
	    	len = String.format("%d", l);
	    	keywords = "Pos(" + pos + ") " + keywords;
	    }


	    if (!prevName.contentEquals("")) {
	    	name = prevName;
	    	prevName = "";
	    }
	    
	    if (potentialName.endsWith("...")) {
	    	prevName = potentialName.substring(0, potentialName.length() - 3);
	    	output.remove = true;
	    }

		
	    if (output.remove == false) {
	      switch (type.toUpperCase()) {
	        case "A":
	          if (keywords.toUpperCase().indexOf("VARYING") >= 0) {
	        	  keywords = keywords.replace("(?i)varying", "");
	        	  type = "Varchar";
	          } 
	          else {
	            type = "Char";
	          }
	          type += "(" + len + ")";
	          break;
	        case "B":
	          type = "Bindec" + "(" + len + ")";
	          break;
	        case "C":
	          type = "Ucs2" + "(" + len + ")";
	          break;  
	        case "D":
	          type = "Date";
	          break;
	        case "F":
	          type = "Float" + "(" + len + ")";
	          break;
	        case "G":
	          if (keywords.toUpperCase().indexOf("VARYING") >= 0) {
	            keywords = keywords.replace("(?i)varying", "");
	            type = "Vargraph";
	          } else {
	            type = "Graph";
	          }
	          type += "(" + len + ")";
	          break;
	        case "I":
	          type = "Int" + "(" + len + ")";
	          break;
	        case "N":
	          type = "Ind";
	          break;
	        case "P":
	          type = "Packed" + "(" + len + ":" + decimals + ")";
	          break;
	        case "S":
	          type = "Zoned" + "(" + len + ":" + decimals + ")";
	          break;
	        case "T":
	          type = "Time";
	          break;
	        case "U":
	          type = "Uns" + "(" + len + ")";
	          break;
	        case "Z":
	          type = "Timestamp";
	          break;
	        case "*":
	          type = "Pointer";
	          break;
	        case "":
	          if (len != "") {
	            if (decimals.isEmpty()) {
	              if (keywords.toUpperCase().indexOf("VARYING") >= 0) {
	                keywords = keywords.replace("(?i)varying", "");
	                type = "Varchar";
	              } else {
	                type = "Char";
	              }
	              type += "(" + len + ")";
	            } else {
	              if (isSubf) {
	                type = "Zoned" + "(" + len + ":" + decimals + ")";
	              } else {
	                type = "Packed" + "(" + len + ":" + decimals + ")";
	              }
	            }
	          }
	          break;
	      }

	      switch (field) {
	        case "C":
	          //output.value = "Dcl-C " + name.padEnd(10) + " " + keywords;
	        	output.value = "Dcl-C " + name + " " + keywords;
	          break;
	        case "S":
	          //output.value = "Dcl-S " + name.padEnd(12) + " " + type.padEnd(10) + " " + keywords;
	        	output.value = "Dcl-S " + name + " " + type + " " + keywords;
	          break;
	        
	        case "PR":	        	
	        case "PI":	        	
	        case "DS":
	        	type="";
	        	if (field.contentEquals("DS") && input.substring(23, 24).trim().toUpperCase().contentEquals( "S")) {	        	  
	        		keywords = "PSDS " + keywords;
	        	}
	        	if (keywords.toUpperCase().indexOf("QUALIFIED") == -1) {
	        		DSisQualified = false;
	        	}

	          if (keywords.toUpperCase().indexOf("LIKEDS") == -1) {
	        	  DSisLIKEDS = false;
	          }

	          if (name.isBlank()) {
	        	  name = "*N";
	          }
	          
	          isSubf = false;
	          if (field.contentEquals("DS")) {
                  isSubf=true;       	  	
 	          }
	          
	          output.value = "Dcl-" + field + " " + name + " " + type + " " + keywords;

		      //if (!DSisLIKEDS) {
		      //	  output.isSub ="true";
	          //}
	          output.blockType = field;

	          output.nextSpaces = indent;
	          break;
	        case "":
	          if (name.isBlank()) {
	        	  name = "*N";
	          }
	          if (name.contentEquals("*N")  && type.contentEquals("")) {
	        	  output.aboveKeywords = keywords;
	        	  output.remove = true;
	          } 
	          else {
	        	  //(isSubf ? "Dcl-Subf" : "Dcl-Parm")
	        	  //output.value = name.padEnd(14) + " " + type.padEnd(10) + " " + keywords;
	        	  output.value = name +" " + type + " " + keywords;

	        	  if (!DSisQualified) {
	        		  output.rvar.standalone = true;
	        	  }
	          }
	          break;
	      }
	    }

	    if (!output.value.isBlank()) {
	    	output.change = true;
	    	output.value = output.value.stripTrailing() + ';';
	    }
	    return output;
	}
}
