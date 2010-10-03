
Place files in somedir

Make symlink from somedir to workspace dir eg.
ln -s ~/somedir ~/workspace/Assignment1

-- Compile program
cd workspace/Assignment1
ant

-- Run program
cd ~/git/exercises/shakespeare
hadoop jar ~/workspace/Assignment1/letter.jar letter.LetterCount

-- Show results
fs -cat /user/training/output/part-00000


-- Delete results
hadoop fs -rmr /user/training/output

