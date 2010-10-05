package letter;

import java.io.IOException;
import java.util.Iterator;
import org.apache.hadoop.io.Text;
import org.apache.hadoop.mapred.MapReduceBase;
import org.apache.hadoop.mapred.OutputCollector;
import org.apache.hadoop.mapred.Reducer;
import org.apache.hadoop.mapred.Reporter;
import org.apache.hadoop.mapred.JobConf;


public class LetterCountReducer extends MapReduceBase
    implements Reducer<Text, Text, Text, Text> {

  public LetterCountReducer() { }

  public void reduce(Text key, Iterator<Text> values,
      OutputCollector<Text, Text> output, Reporter reporter) throws IOException {
	  	int sum = 0;
	    while (values.hasNext()) {
	    	//Sum is total number of occurrences of the letter in key
	    	sum += Integer.parseInt(values.next().toString());
	    }
	    //Output the result
	    output.collect(key, new Text(Integer.toString(sum)));
	  }
}

