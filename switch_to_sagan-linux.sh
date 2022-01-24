#!/bin/zsh

lib="$PWD/lib"

cp $PWD/resources/Annotations.json.sagan $PWD/resources/Annotations.json
cp $PWD/resources/Annotations.json.sagan $lib/resources/Annotations.json
cp $PWD/resources/Callgraph.txt.sagan $PWD/resources/Annotations.json
cp $PWD/resources/Callgraph.txt.sagan $lib/resources/Annotations.json
cp $PWD/resources/Chain.json.sagan $PWD/resources/Chain.json
cp $PWD/resources/Chain.json.sagan $lib/resources/Chain.json
cp $PWD/resources/GetterSetter.json.sagan $PWD/resources/GetterSetter.json
cp $PWD/resources/GetterSetter.json.sagan $lib/resources/GetterSetter.json
cp $PWD/resources/Methods.txt.sagan $PWD/resources/Methods.txt
cp $PWD/resources/Methods.txt.sagan $lib/resources/Methods.txt
cp $PWD/resources/skip_func.txt.sagan $PWD/resources/skip_func.txt
cp $PWD/resources/skip_func.txt.sagan $lib/resources/skip_func.txt
