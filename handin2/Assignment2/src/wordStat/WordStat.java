// (c) Copyright 2009 Cloudera, Inc.

package wordStat;

import java.io.IOException;

import org.apache.hadoop.conf.Configured;
import org.apache.hadoop.fs.Path;
import org.apache.hadoop.io.Text;
import org.apache.hadoop.mapred.Reducer;
import org.apache.hadoop.mapred.FileInputFormat;
import org.apache.hadoop.mapred.FileOutputFormat;
import org.apache.hadoop.mapred.JobClient;
import org.apache.hadoop.mapred.JobConf;
import org.apache.hadoop.util.ToolRunner;
import org.apache.hadoop.util.Tool;

/**
 * LineIndexer
 *
 * Creates an inverted index over all the words in a document corpus, mapping
 * each observed word to a list of filename@offset locations where it occurs.
 *
 */
public class WordStat extends Configured implements Tool {

  // where to put the data in hdfs when we're done
  private static final String OUTPUT_PATH = "output";

  // where to read the data from.
  private static final String INPUT_PATH = "input";

  /** Driver for the actual MapReduce process */
  private void runJob() throws IOException {

    JobConf conf = new JobConf(getConf(), WordStat.class);

    FileInputFormat.addInputPath(conf, new Path(INPUT_PATH));
    FileOutputFormat.setOutputPath(conf, new Path(OUTPUT_PATH));

    //conf.setMapperClass(LineIndexMapper.class);
    conf.setMapperClass(WordStatMapper.class);
    
    //conf.setReducerClass(LineIndexReducer.class);
    conf.setReducerClass(WordStatReducer.class);
    
    conf.setOutputKeyClass(Text.class);
    conf.setOutputValueClass(Text.class);

    JobClient.runJob(conf);
  }

  public int run(String [] args) throws IOException {
    runJob();
    return 0;
  }

  public static void main(String [] args) throws Exception {
    int ret = ToolRunner.run(new WordStat(), args);
    System.exit(ret);
  }
}

