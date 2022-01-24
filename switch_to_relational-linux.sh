#!/bin/zsh

lib="$PWD/lib"

cp $PWD/resources/Annotations.json.relational $PWD/Annotations.json
cp $PWD/resources/Annotations.json.relational $lib/Annotations.json
cp $PWD/resources/Callgraph.txt.relational $PWD/Callgraph.txt
cp $PWD/resources/Callgraph.txt.relational $lib/Callgraph.txt
cp $PWD/resources/Chain.json.relational $PWD/Chain.json
cp $PWD/resources/Chain.json.relational $lib/Chain.json
cp $PWD/resources/GetterSetter.json.relational $PWD/GetterSetter.json
cp $PWD/resources/GetterSetter.json.relational $lib/GetterSetter.json
cp $PWD/resources/Methods.txt.relational $PWD/Methods.txt
cp $PWD/resources/Methods.txt.relational $lib/Methods.txt
cp $PWD/resources/skip_func.txt.relational $PWD/skip_func.txt
cp $PWD/resources/skip_func.txt.relational $lib/skip_func.txt
