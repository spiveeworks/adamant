#!/bin/sh
tcc -run adamant.c "$1" out/app || exit
hexdump -ve '1/1 "%02X" 15/1 " %02X" "\n"' out/app > out/app.hex

echo
echo "Running output..."
./out/app
echo "Exit code: $?"

