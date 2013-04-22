#!/bin/bash

erlc boot.erl
nohup erl -pa vortex/ebin/ -pa vortex/deps/*/ebin -noshell -s boot start &

