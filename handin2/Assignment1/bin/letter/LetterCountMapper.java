// (c) Copyright 2009 Cloudera, Inc.

package letter;

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

/**
 * LineIndexMapper
 *
 * Maps each observed word in a line to a (filename@offset) string.
 *
 */
public class LetterCountMapper extends MapReduceBase
    implements Mapper<LongWritable, Text, Text, Text> {

  public LetterCountMapper() { }

  public void map(LongWritable key, Text value, OutputCollector<Text, Text> output,
      Reporter reporter) throws IOException {
	    
	    char[] letterArray = value.toString().replaceAll("[^a-z^A-Z]","").toCharArray();
	   
	    for(int i=0; i<letterArray.length; i++){
	    	output.collect( new Text(String.valueOf(letterArray[i])), new Text("1"));
	    }
	  }
}

