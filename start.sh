#!/bin/bash

cd `dirname $0`
nohup `erl -pa $PWD/apps/*/ebin -pa $PWD/deps/*/ebin -boot start_sasl -s reloader -s vortex_core -s vortex_web` > server.log &


