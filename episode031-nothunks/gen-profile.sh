#!/bin/bash

cabal run --enable-profiling ep31 -- +RTS -hy -l -RTS $@
eventlog2html ep31.eventlog