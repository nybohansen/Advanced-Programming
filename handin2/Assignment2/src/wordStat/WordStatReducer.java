// (c) Copyright 2009 Cloudera, Inc.

package wordStat;

import java.io.IOException;
import java.util.Iterator;

import org.apache.hadoop.io.Text;
import org.apache.hadoop.mapred.MapReduceBase;
import org.apache.hadoop.mapred.OutputCollector;
import org.apache.hadoop.mapred.Reducer;
import org.apache.hadoop.mapred.Reporter;
import org.apache.hadoop.mapred.JobConf;

/**
 * LineIndexReducer
 *
 * Takes a list of filename@offset entries for a single word and concatenates
 * them into a list.
 *
 */
public class WordStatReducer extends MapReduceBase
    implements Reducer<Text, Text, Text, Text> {

  public WordStatReducer() { }

  public void reduce(Text key, Iterator<Text> values,
      OutputCollector<Text, Text> output, Reporter reporter) throws IOException {
	  
	  int N = 0;
	  int n_i = 0;
	  String w_i;
	  while (values.hasNext()) {
		  //Extract the pair from the string
		  w_i = values.next().toString().split(" ")[0];
		  n_i = Integer.parseInt(values.next().toString().split(" ")[1]);  
		  N += n_i;
	  }

	  String pickedWord = "SomeWord";
	  String wordArray[];
	  
	  output.collect(key, new Text(Integer.toString(N) + " " + pickedWord));
	  
  }
}

