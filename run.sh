root_dir=$PWD
bin_dir="$PWD/_build/default/bin/"

cd $root_dir
dune build
#cp *.txt $bin_dir
cp *.json $bin_dir
$bin_dir/InferenceEngineApplication.exe
