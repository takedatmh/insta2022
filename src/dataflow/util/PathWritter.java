package dataflow.util;

import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.List;

import soot.MethodOrMethodContext;
import soot.jimple.toolkits.callgraph.Edge;
import soot.jimple.toolkits.callgraph.ReachableMethods;

public class PathWritter {
	
	/**
	 * 引数で指定したファイルパスとフィアル名で、CFGパスリストの中身をそのまま、
	 * 行単位（パス単位）に.txtファイルとして書き出す。
	 * 
	 * @param path ファイルパス
	 * @param targetClass　ファイル名の一部としてのクラス名
	 * @param methodName　ファイル名の一部としての対象CFGのメソッド名
	 * @param pathList　CFGのパスリスト
	 * @return ファイ込書込　boolean
	 */
	public static boolean writePath(String path, String targetClass, String methodName, List<String> pathList ) {
		boolean ret = false;
		
		//Write the list of path.
		FileWriter in = null;
		PrintWriter out = null;
		String filePath = path + targetClass + methodName +".txt";
		try {
			//postscript version
			in = new FileWriter(filePath, true);
			out = new PrintWriter(in);
			for(String p : pathList)
				out.println(p.toString());
			out.flush();
		} catch (Exception e) {
			e.printStackTrace();
		} finally {
			try {
				//close
				in.close();
				out.close();
				//Change flag from false to true.
				ret = true;
			} catch (IOException e) {
				e.printStackTrace();
			}
		}
		
		//return value.
		return ret;
		
	}
	
	/**
	 * 概要プロセスの#3にあたる処理。
	 * 解析対象MethodからののCGを抽出した後に、このメソッドで到達可能なUnitを抽出して
	 * ファイルに書き出す。
	 * @param path ファイルパス
	 * @param targetClass　フィアル名の一部として利用するクラスファイル名
	 * @param methodName　ファイル名の一部として利用するCFG解析対象のメソッド名
	 * @param reachMethods 指定されたCGのノードに関連のあるノードを抽出するオブジェクト
	 * @return ファイル書き込み成否　boolean
	 */
	public static boolean writePath(String path, String targetClass, String methodName,  ReachableMethods reachMethods) {
		boolean ret = false;
		
		//Write the list of path.
		FileWriter in = null;
		PrintWriter out = null;
		String filePath = path + targetClass + methodName +".txt";
		try {
			//postscript version
			in = new FileWriter(filePath, true);
			out = new PrintWriter(in);
			while(reachMethods.listener().hasNext()){
				MethodOrMethodContext mc = reachMethods.listener().next();
				out.println(mc.toString());
			}
			out.flush();
		} catch (Exception e) {
			e.printStackTrace();
		} finally {
			try {
				//close
				in.close();
				out.close();
				//Change flag from false to true.
				ret = true;
			} catch (IOException e) {
				e.printStackTrace();
			}
		}
		
		//return value.
		return ret;
		
	}
	
	/**
	 * Edgeの情報のリストをファイルとして書き込むメソッド。
	 * igraphのdataframeのインプット情報として利用できるメリットがある。
	 * 書き込まれるデータはテーブル形式で、
	 * 開始ノード, 終点ノード, Edgeの種類(CRUD), シーケンス番号
	 * 
	 * @param filePath ファイルパス
	 * @param targetClass ファイル名の一部としての解析対象クラス名
	 * @param methodName フィアル名の一部としての解析対象クラス名
	 * @param pathList 解析したCFGパスリスト
	 * @return ファイル書き込み成否 boolean型
	 */
	public static boolean writeEdgePath(String filePath, String targetClass, String methodName, List<List<Edge>> pathList ) {
		boolean ret = false;
		
		//Write the list of path.
		FileWriter in = null;
		PrintWriter out = null;
		String fPath = filePath + targetClass + methodName +".csv";
		try {
			//postscript version
			in = new FileWriter(fPath, true);
			out = new PrintWriter(in);
			//Header column name
			out.println("Start"+","+"End"+","+"KindOfEdge"+","+"PathNo");
			int pathID = 1;
			for(List<Edge> path : pathList) {
				for(Edge e : path)
					out.println(e.getSrc().method().toString() +","+e.getTgt().method().toString()+","+e.kind().toString()+","+pathID);
				pathID++;
			}
			out.flush();
		} catch (Exception e) {
			e.printStackTrace();
		} finally {
			try {
				//close
				in.close();
				out.close();
				//Change flag from false to true.
				ret = true;
			} catch (IOException e) {
				e.printStackTrace();
			}
		}
		
		//return value.
		return ret;
		
	}
	
	/**
	 * Edgeの情報のリストをファイルとして書き込むメソッド。
	 * startNode	endNode
	 * の形式で記述
	 * 
	 * @param filePath 出力ファイルパス
	 * @param fileName 出力ファイル名
	 * @param edgeList List<String>のedge list tab区切りでstart	endの形式
	 * @return ファイル書き込み成否 boolean型
	 */
	public static boolean writeEdge(String filePath, String fileName, List<String> edgeList ) {
		boolean ret = false;
		
		//Write the list of path.
		FileWriter in = null;
		PrintWriter out = null;
		String fPath = filePath + Context.SEPARATOR + fileName +".csv";
		try {
			//postscript version
			in = new FileWriter(fPath, true);
			out = new PrintWriter(in);
			//Header column name
			out.println("Start"+ "	" + "end");
			for(String edge : edgeList) {
				String[] array = edge.split("	");
				out.println(array[0].replace("\"", "").trim() + "	" + array[1].replace("\"", "").trim());
			}
			out.flush();
		} catch (Exception e) {
			e.printStackTrace();
		} finally {
			try {
				//close
				in.close();
				out.close();
				//Change flag from false to true.
				ret = true;
			} catch (IOException e) {
				ret = false;
				e.printStackTrace();
			}
		}
		
		//update return value.
		ret = true;
		
		//return value.
		return ret;
		
	}
	
	/**
	 * Nodeの情報のリストをファイルとして書き込むメソッド。
	 * NodeName
	 * NodeName
	 * の形式で記述
	 * 
	 * @param filePath 出力ファイルパス
	 * @param fileName 出力ファイル名
	 * @param nodeList List<String>のnode list
	 * @return ファイル書き込み成否 boolean型
	 */
	public static boolean writeNode(String filePath, String fileName, List<String> nodeList ) {
		boolean ret = false;
		
		//Write the list of path.
		FileWriter in = null;
		PrintWriter out = null;
		String fPath = filePath + Context.SEPARATOR + fileName +".csv";
		try {
			//postscript version
			in = new FileWriter(fPath, true);
			out = new PrintWriter(in);
			//Header column name
			out.println("Name");
			
			for(String node : nodeList) {
				out.println(node.replace("\"", "").trim());
			}
			out.flush();
		} catch (Exception e) {
			e.printStackTrace();
			ret = false;
		} finally {
			try {
				//close
				in.close();
				out.close();
				//Change flag from false to true.
				ret = true;
			} catch (IOException e) {
				ret = false;
				e.printStackTrace();
			}
		}
		
		//return value.
		return ret;
		
	}	

}
