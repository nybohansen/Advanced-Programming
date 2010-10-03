// (c) Copyright 2009 Cloudera, Inc.

package wordStat;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Random;

import org.apache.hadoop.io.Text;
import org.apache.hadoop.mapred.MapReduceBase;
import org.apache.hadoop.mapred.OutputCollector;
import org.apache.hadoop.mapred.Reducer;
import org.apache.hadoop.mapred.Reporter;
import org.apache.hadoop.mapred.JobConf;

import java.util.Random;

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
	  List<String> wordList = new ArrayList<String>();
	  List<Integer> sumList = new ArrayList<Integer>(); 
	  
	  while (values.hasNext()) {
		  //Extract the pair from the string
		  w_i = values.next().toString().split(" ")[0];
		  n_i = Integer.parseInt(values.next().toString().split(" ")[1]);  
		  if(n_i>0){
			  N += n_i;
			  wordList.add(w_i);
			  sumList.add(N);
		  }
	  }

	  Random rndGenerator = new Random(java.util.Calendar.getInstance().getTimeInMillis());
	   
	  int rndNumber = rndGenerator.nextInt(N);
	  int i = 0;
	  while(sumList.get(i)<rndNumber){
		i++;    
	  }
		  
	  String pickedWord = wordList.get(i);
	  
	  output.collect(key, new Text(Integer.toString(N) + " " + pickedWord));
	  
	  


  }
}

