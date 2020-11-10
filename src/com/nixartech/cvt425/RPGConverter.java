package com.nixartech.cvt425;

import java.util.LinkedList;
import java.util.HashMap;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class RPGConverter {

	private LinkedList<String> lines;
	private int indent;
	
	List<String> knownSpecs = Stream.of("C", "D", "F","H","P").collect(Collectors.toList());
	RPGConvertionResult result = new RPGConvertionResult();
	private LinkedList<Message>  messages = new LinkedList<Message>();	
	private HashMap<String, RPGVar>  vars = new HashMap<String, RPGVar>();
	int currentLine=0;
	int spaces=0;
	String lastBlock = "";
	boolean wasSub = false;
	int index=0;
	int length =0;
	public RPGConverter(LinkedList<String> lines, int indent) {
		
		this.lines = lines;
		this.indent=indent;
		this.length = lines.size();
	}

	public void parse() {
				
		for(index=0;  index < length; index++) {
			
			String comment="";
			String ignoredColumns="";
			String spec="";
			String escapeChar="";
			boolean isMove=false;
			boolean hasKeywords=false;
			
			currentLine = index;
			comment="";
			String line = lines.get(index);
		    System.out.printf("%6d %6d  %-80s", length, index, line);
		    System.out.printf(" -> ");
			line = " " + padEnd(line,80);
			if (line.length() > 81) {
		        line = line.substring(0, 81);
		        comment = lines.get(index).substring(80);
			}
		    ignoredColumns = line.substring(1, 4);
		    spec = line.substring(6, 7).toUpperCase();
		    escapeChar = line.substring(7, 8);
		    switch(escapeChar) {
		    	case "/":
		    		spec="";
		    				    		
		    		String directive = line.substring(8).trim().toUpperCase();
		    		switch (directive) {
		            	case "FREE":
		            	case "END-FREE":
		            		//this.lines.splice(index, 1); Delete 'index' entry 
		            		lines.remove(index);
		            		index--;
		            		length--;
		            		break;
		            	default:
		            		lines.set(index, padEnd(" ",8) +padEnd(" ", spaces) + line.substring(7).trim());
		            		break;
		    		}
		    		break;
		    	case "*":
		            spec = "";

		            comment = line.substring(8).trim();
		            if (!comment.equals(""))
		            	lines.set(index,  padEnd(" ",8) + padEnd(" ",spaces) + "//" + comment);
		            else
		             	lines.set(index,"");
		            break;
		    }
		    
		    //Process known Specs
		    if (knownSpecs.contains(spec)) {
		    	
		    	RPGSpec rpgSpec =  RPGSpec.getInstanceFor(spec);
		    	result=rpgSpec.parse(line, indent);
		    	

		    	if (!result.blockType.isBlank() ) {
		    		
		    		 if (!wasSub) {
		    			 wasSub = true;
		    			 lastBlock = result.blockType;
		    		 }
		    		 else {
		    			  endBlock(this.lines,this.indent);
		    			  wasSub = true;
		    			  lastBlock = result.blockType;
		    		 }
		    	}
		    	
		        if(result.var != null) {
		        	addVar(result.var);
		        }
		    	
		        if (result.move !=null) {
		        	isMove = true;
		        }
		        if (result.aboveKeywords!=null) {
		        	hasKeywords =true;
		    	}

		        if (result.message != null) {
		            messages.add(new Message( currentLine, result.message));
		        }
		        
		        if (isMove) {
		        	result = suggestMove(result.move);
		        	
		        	if(result.change) {
		        		 lines.set(index ,  ignoredColumns + "    " + padEnd(" ",spaces) + result.value);
		        	}
		        }
		        else if (hasKeywords) {		 
		        	var l =  lines.get(index - 1);
		        	var endStmti =l.indexOf(';');
		        	
		        		var endStmt = lines.get(index - 1).substring(endStmti); //Keep those end line comments :D
		        		lines.set(index-1,  lines.get(index - 1).substring(0, endStmti) + ' ' + result.aboveKeywords + endStmt);
		        		lines.remove(index);		             
		        		index--;
		        		length--;
		        	
		        }
		        else if (result.remove) {
		        	if (!comment.trim().equals("")) {
		                lines.set(index, ignoredColumns + "    " + padEnd(" ",spaces) + "//" + comment);
	                } 
		        	else
		        	{
		                lines.remove(index);
		                index--;
		                length++;
		            }
		        }
		        else if (result.change) {
		        	spaces += result.beforeSpaces;
		        	if (result.arrayOutput.size() > 0) {
		        		lines.remove(index);
		        		for (String lne : result.arrayOutput) {
		                    lne = ignoredColumns + "    " + padEnd(" ",spaces) + lne;

		                    //this.lines.splice(index, 0, result.arrayoutput[y]);
		                    lines.add(index, lne);
		                    index++;
		                    length++;
		                }
		                index--;
		                length--;
		        	}
		        	else {
		        	     String changedLine=ignoredColumns + "    " + padEnd(" ",spaces) + result.value;
		        		 lines.set(index,changedLine) ;
		                 if (!comment.trim().contentEquals("")) {
		                	 lines.set(index, lines.get(index)+ " //" + comment);
		                 }
		        	}
		        	spaces+=result.nextSpaces;
		        }
		        
		   
		    }
		    else {
		    	if (wasSub) {
		    		endBlock(lines, indent);
		    	}
		    } 			    	
		    System.out.printf("%6d %-81s", index, lines.get(index));
		    System.out.println("");
		}
	}
	
	private RPGConvertionResult suggestMove(RPGMove move) {
		
		RPGConvertionResult result = new RPGConvertionResult();
		
		
		result.change=false;
		result.value="";
		
		var sourceVar = vars.get(move.source.toUpperCase());
		var targetVar = vars.get(move.target.toUpperCase());
		
		if (sourceVar == null) {
			sourceVar = new RPGVar();
			if(move.source.startsWith("'")) {
				sourceVar.name = move.source;
				sourceVar.type="A";
				sourceVar.len=move.source.length()-2;
			
		
				if (targetVar == null) {
					String message = "Assuming " + move.target + " is a character field for MOVE/MOVEL operation.";
					messages.add(new Message(currentLine, message));
					sourceVar.name = move.target;
					sourceVar.type="A";			
				}
			}
			else if(move.source.startsWith("*")) {
				sourceVar.name = move.source;
				sourceVar.type="S";
			} 
			else {
				sourceVar.name=move.source;
				sourceVar.type="S";
				sourceVar.len=move.source.length();
			}
			sourceVar.constant = true;
		}
		else {
			switch (sourceVar.type) {
	        case "D":
	          sourceVar.len = 10;
	          sourceVar.constant = true;
	          break;
	        case "T":
	          sourceVar.len = 8;
	          sourceVar.constant = true;
	          break;
	      }
		}
		
		if (targetVar == null && sourceVar != null) {
			
			targetVar = new RPGVar();
			
			String message = "Assuming " + move.target + " is a type "+sourceVar.type+" field for MOVE/MOVEL operation.";
			messages.add(new Message(currentLine, message));
			
			targetVar.name = move.target;
			targetVar.type = sourceVar.type;
			
		}
		
		if (targetVar != null) {
			var assignee = targetVar.name;
			
			switch (targetVar.type) {
	        case "S": //numeric (not specific to packed or zoned)
	          result.value = assignee + " = " + sourceVar.name;
	          break;
	      
	        case "D": //date
	          if (sourceVar.name.toUpperCase().equals("*DATE")) {
	            result.value = targetVar.name + " = " + sourceVar.name;
	          } 
	          else 
	          {
	            if (move.attr.equals(""))
	              result.value = targetVar.name + " = %Date(" + sourceVar.name + ")";
	            else
	              result.value = targetVar.name + " = %Date(" + sourceVar.name + ":" + move.attr + ")";
	          }
	          break;

	        case "A": //character
	          if (move.padded) {
	            if (move.dir.equals("MOVEL"))
	              assignee = targetVar.name;
	            else
	              assignee = "EvalR " + targetVar.name;
	          } 
	          else 
	          {
	            if( move.equals( "MOVEL")) {
	              if (sourceVar.constant) {
	                assignee = "%Subst(" + targetVar.name + ":1:" + sourceVar.len + ")";
	              }
	              else {
	                assignee = "%Subst(" + targetVar.name + ":1:%Len(" + sourceVar.name + "))";
	              }
	            }
	            else  if (sourceVar.constant) {
	              assignee = "%Subst(" + targetVar.name + ":%Len(" + targetVar.name + ")-" + sourceVar.len + ")";
	            }
	            else {
	              assignee = "%Subst(" + targetVar.name + ":%Len(" + targetVar.name + ")-%Len(" + sourceVar.name + "))";
	            }
	          }

	          switch (sourceVar.type) {

	            case "A":
	              result.value = assignee + " = " + sourceVar.name;
	              break;

	            case "S":
	            case "P":
	            case "I":
	            case "F":
	            case "U":
	              result.value = assignee + " = %Char(" + sourceVar.name + ")";
	              break;

	            case "D":
	            case "T":
	              if (!move.attr.equals(""))
	                result.value = assignee + " = %Char(" + sourceVar.name + ":" + move.attr + ")";
	              else
	                result.value = assignee + " = %Char(" + sourceVar.name + ")";
	          }
	          break;
	      }
	    }		
		
		if (!result.value.contentEquals("")) {
			result.change=true;
			result.value = result.value.stripTrailing()+";";
		}
		else {
			this.messages.add(new Message(currentLine, "Unable to convert MOVE/MOVEL operation."));
		}
		
		return result;
	}

	private void addVar(RPGVar var) {
		  if (var.standalone )
		      vars.put(var.name.toUpperCase(), var );
		
	}

	private void endBlock(LinkedList<String> lines, int indent) {
	      spaces -= indent;
	      if (lastBlock != null) {
	        String lne = padEnd(" ",7) + padEnd(" ",spaces) + "End-" + lastBlock + ";";
            lines.add(index, lne);
            System.out.printf("%6d %-81s", index, lines.get(index));
		    System.out.println("");
	        index++;
	        length++;
	      }
	      wasSub = false;
		
	}

	public static String padEnd(String s, int n) {
		if (n==0) return s;
		
	     var padded =  String.format("%-" + n + "s", s);
	     return padded;
	}

	public static String padLeft(String s, int n) {
	    return String.format("%" + n + "s", s);  
	}


}
