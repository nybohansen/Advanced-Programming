// (c) Copyright 2009 Cloudera, Inc.

package letter;

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
public class LetterCountReducer extends MapReduceBase
    implements Reducer<Text, Text, Text, Text> {

  public LetterCountReducer() { }

  public void reduce(Text key, Iterator<Text> values,
      OutputCollector<Text, Text> output, Reporter reporter) throws IOException {
	  	int sum = 0;
	    while (values.hasNext()) {
	    	sum += Integer.parseInt(values.next().toString());
	    }
	    output.collect(key, new Text(Integer.toString(sum)));
	  }
}

