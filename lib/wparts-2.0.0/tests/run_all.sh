#!/bin/zsh
cd ../../..
./bin/compile.erl
cd -
erl -make
erl -pa ../ebin/ ../../wpart-2.0.0/ebin/ -s datetime_format_tests test -s init stop
