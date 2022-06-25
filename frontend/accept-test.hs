#!/bin/bash

cabal run exe:frontend -v0 -- $1 > ${1}.result
cat ${1}.result