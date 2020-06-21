#!/bin/bash
rm -f *.dump

if [[ "$1" == "java" ]]; then
    rm -f *.class
    javac calling.java exchange.java
    java exchange
fi

if [[ "$1" == "erlang" ]]; then
    rm -f *.beam
    erlc calling.erl exchange.erl > /dev/null 2>&1
    erl -noshell -s exchange -s init stop
fi