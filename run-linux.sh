root_dir=/home/jslee/InferenceEngine/
bin_dir=/home/jslee/InferenceEngine/_build/default/bin/

cd $root_dir
dune build
cp *.txt $bin_dir
cp *.json $bin_dir
$bin_dir/InferenceEngineApplication.exe
