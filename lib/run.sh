root_dir=/Users/jslee/Dropbox/InferenceEngine/
bin_dir=/Users/jslee/Dropbox/InferenceEngine/_build/default/bin/

cd $root_dir
dune build
cp *.txt $bin_dir
cp *.json $bin_dir
$bin_dir/InferenceEngineApplication.exe