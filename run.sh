#!/bin/sh
tcc src/compiler.c -run | tee output/a.ll
