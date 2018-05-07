#!/bin/bash -e

# Bash script
#
# author: Xavier Devroey

scriptdir=$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )

raw="$scriptdir/raw"
clean="$scriptdir/clean"
working="$scriptdir/tmp"
jhipster="$working/jhipster.csv"

rpackage="$scriptdir/package-jhipster-results.r"

# Clean input files

mkdir -p "$working"

sed -E s/\"\"\"/\"/g "$raw/jhipster.csv" > "$jhipster"

# Call R results packaging

Rscript "$rpackage" "$jhipster" 

rm -rf "$clean"
mkdir -p "$clean"

mv -f jhipster-results.Rda "$clean/jhipster-results.Rda"
mv -f jhipster-results.csv "$clean/jhipster-results.csv"

