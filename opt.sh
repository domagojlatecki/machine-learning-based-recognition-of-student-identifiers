#!/usr/bin/env bash
./mill clean
OPT_BUILD="1" ./mill compile
OPT_BUILD="1" ./mill recognition.launcher
