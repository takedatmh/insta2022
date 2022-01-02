package dataflow.converter;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import dataflow.util.*;

public class Converter {
	
	//Node list 
	List<String> nodeList = new ArrayList<String>();
	
	//tab split edges list like "startNode	endNode"
	List<String> edgeList = new ArrayList<String>();
	
	public boolean convertDot2Igraph(String filePath) throws IOException{
		//return value
		boolean ret = false;
		
		//Read .dot file.
		FileReaderUtil fileReaderUtil = new FileReaderUtil();
		
		//Loop: Read line
		List<String> list = fileReaderUtil.readFile(filePath);
		for(String line : list){
			
			//Skip the first line and the latest one.
			if(line.contains("{") || line.contains("}")){
				continue;
			}

			//IF this line contains '->', set this line into edge list.
			else if(line.contains("->") == true){
				
				edgeList.add(line.replace("	", "").replace(";", "").replace("\"", "").replace("->", "	"));
			}
			
			//IF this line dose not contains '->' set this line into node list.
			else {
				nodeList.add(line.replace(";", "").replace("\"", ""));
			}
		}
		//Write Node list.
		PathWritter.writeNode("./igraph", "Sample_Node_list_for_igraph", nodeList);
		
		//Write Edge list.
		PathWritter.writeEdge("./igraph", "Sample_Edge_list_for_igraph", edgeList);
		
		//Update return values
		ret = true;
		
		//return
		return ret;
	}

}
