
Place files in somedir

Make symlink from somedir to workspace dir eg.
ln -s ~/somedir ~/workspace/Assignment2

-- Compile program
cd workspace/Assignment2
ant

-- Run program
cd ~/git/exercises/shakespeare
hadoop jar ~/workspace/Assignment2/wordStat.jar wordStat.WordStat

-- Show results
fs -cat /user/training/output/part-00000


-- Delete results
hadoop fs -rmr /user/training/output

