#!/bin/zsh
cd ../../..
./bin/compile.erl
cd -
erl -make
erl -pa ../ebin/ ../../wpart-1.4.1/ebin/ -s datetime_format_tests test -s init stop
