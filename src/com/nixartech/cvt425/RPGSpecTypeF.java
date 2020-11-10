package com.nixartech.cvt425;

public class RPGSpecTypeF extends RPGSpec {

	@Override
	public RPGConvertionResult parse(String input, int indent) {
		
		RPGConvertionResult output = new RPGConvertionResult();		
		output.remove = false;
		output.change = false;
		output.value="";
		output.beforeSpaces=0;
		output.nextSpaces=0;
		
		var name     = input.substring(7, 17).trim(); //File name
        var type     = input.substring(17, 18).toUpperCase(); // I, U, O, C
        var field    = input.substring(34, 35).toUpperCase(); //KEYED
        var device   = input.substring(36, 43).toUpperCase().trim(); //device: DISK, WORKSTN
        var keywords = input.substring(44).trim();
		
        output.value = "Dcl-F " + name;
        
        switch (type) {
	        case "I":
	            type = "*Input";
	            break;
	        case "U":
	            type = "*Update:*Delete:*Output";
	            break;
	        case "O":
	            if (!device.contentEquals( "PRINTER")) {	            	
	                type = "*Output";
	            }
	            else {
	                type = "";
	            }
	            break;
	        case "C":
	            if (!device.contentEquals("WORKSTN")) {
	                type = "*INPUT:*OUTPUT";
	            }
	            else {
	                type = "";
	            }
	            break;	
	        default:
	            type = "";
	            break;
	    	}

        	if (!device.contentEquals("DISK")) {
        		output.value += ' ' + device;
        	}

        if (!type.contentEquals("")) {
        	output.value += " Usage(" + type + ")";
        }

        if (field.contentEquals("K")) {
        	output.value += " Keyed";
        }
        if (!keywords.contentEquals("")) {
        	if (name.contentEquals("")) {
        		output.aboveKeywords = keywords;
        	}
        	else {
        		output.value += " " + keywords;
        	}
        }

        if (!output.value.contentEquals("")) {
        	output.change = true;
        	output.value = output.value.stripTrailing() + ';';
        }
        return output;
	}	
}
