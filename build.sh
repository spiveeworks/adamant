#!/bin/sh

name=$1
cargo run -- data/$name.cln > output/$name.ll
llvm-as output/$name.ll -o output/$name.bc
llc output/$name.bc -o output/$name.s
gcc output/$name.s -o output/$name
./output/$name
echo Result: $?
