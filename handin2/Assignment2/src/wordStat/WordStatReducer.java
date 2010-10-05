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

public class WordStatReducer extends MapReduceBase
    implements Reducer<Text, Text, Text, Text> {

  public WordStatReducer() { }

  public void reduce(Text key, Iterator<Text> values,
      OutputCollector<Text, Text> output, Reporter reporter) throws IOException {
	  
	  int N = 0;
	  int n_i = 0;
	  String w_i;
	  //Array of all the words
	  List<String> wordList = new ArrayList<String>();
	  //Array of the sum 
	  List<Integer> sumList = new ArrayList<Integer>(); 
	  
	  while (values.hasNext()) {
		  //Create the array Payload containin the word and the number of words
		  String payload[] = values.next().toString().split(" ");
		  //Extract the pair from the string
		  w_i = payload[0];
		  n_i = Integer.parseInt(payload[1]);  
		  //If the mapper processed a string containing no chars, we want
		  //n_i is 0. 
		  if(n_i>0){
			  N += n_i;
			  wordList.add(w_i);
			  //Add the accumulated sum
			  sumList.add(N);
		  }
	  }

	  //Seed random generator as explained in the assignment
	  Random rndGenerator = new Random(java.util.Calendar.getInstance().getTimeInMillis()); 
	  //Select random number in the interval [0,N]
	  int rndNumber = rndGenerator.nextInt(N);
	  
	  //Find the place in the word array where the picked word is
	  //The word is again picked with uniform probability, since
	  //the sumList contains the accumulated number of occurrences the number of words
	  int i = 0;
	  while(sumList.get(i)<rndNumber){
		i++;    
	  }
	  
	  //Store the picked word
	  String pickedWord = wordList.get(i);
	  //Send it to the output
	  output.collect(key, new Text(Integer.toString(N) + " " + pickedWord));  
  }
}




