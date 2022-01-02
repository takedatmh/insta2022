package dataflow.converter;

import java.io.IOException;

import org.junit.Test;

public class ConverterTest {


	@Test
	public void test() throws IOException {
		
		Converter c = new Converter();
		
		boolean result = c.convertDot2Igraph("./CallGraph/CallGraph_sample.functionA.MainA_main.dot");
		
		System.out.println(result);
		
	}

}
