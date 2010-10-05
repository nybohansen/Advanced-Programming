package wordStat;

import java.io.IOException;
import java.util.StringTokenizer;

import org.apache.hadoop.io.LongWritable;
import org.apache.hadoop.io.Text;
import org.apache.hadoop.mapred.FileSplit;
import org.apache.hadoop.mapred.MapReduceBase;
import org.apache.hadoop.mapred.Mapper;
import org.apache.hadoop.mapred.OutputCollector;
import org.apache.hadoop.mapred.Reporter;
import org.apache.hadoop.mapred.JobConf;

import java.util.Random;

public class WordStatMapper extends MapReduceBase
    implements Mapper<LongWritable, Text, Text, Text> {

  public WordStatMapper() { }

  public void map(LongWritable key, Text value, OutputCollector<Text, Text> output,
      Reporter reporter) throws IOException {
	 
	  //Split string into word array
	  String wordArray[] = value.toString().split("\\s+");
	  
	  //number of words
	  int numberOfWords = wordArray.length;
	  
	  //Pick one random word from the word array. This works because the word array
	  //contains all the words including any duplicates. E.x. "one on one" becomes ["one", "on", "one"]
	  //when selecting a random word in this array, the probability to hit "one" is still 2/3.
	  String pickedWord = "";
	  
	  if(numberOfWords>0){
		  //If we are not operating on a blank line, i.e. a line which only contains blank chars.
		  Random rndGenerator = new Random( key.get() + java.util.Calendar.getInstance().getTimeInMillis());
		  //The word is here picked with uniform probability, from the set consisting
		  //of all the words in the string. 
		  pickedWord = wordArray[rndGenerator.nextInt(numberOfWords)];
	  }
	 
	  //Send it to the reducer
	  output.collect( new Text("key"), new Text(pickedWord +" " + Integer.toString(numberOfWords)));
	 
  }
}

