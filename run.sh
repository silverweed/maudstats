#!/usr/bin/env bash

ROOT="$(readlink -f $(dirname $0))"
EXE="$ROOT/dist/build/maudstats/maudstats"

[[ -x $(which cabal 2>/dev/null) ]] || {
	>&2 echo "[ FATAL ] Missing cabal executable!"
	exit 1
}
[[ -x $(which awk 2>/dev/null) ]] || {
	>&2 echo "[ FATAL ] Missing awk executable!"
	exit 2
}
[[ -x $EXE ]] || {
	cabal install
	[[ $? == 0 ]] || {
		>&2 echo "[ FATAL ] Errors building maudstats!"
		exit 3
	}
}
$EXE | awk -v 'FS=|' > $ROOT/frontend/data.js '
{
	++i
	labels[i] = $1
	data[i] = $2
}

END {
	printf "\"use strict\";\r\nvar ext = {\r\n\tlabels: ["
	for (j = 1; j < i - 1; ++j) 
		printf "\""labels[j]"\", "
	printf "\""labels[i]"\"],\r\n\tdata: ["
	for (j = 1; j < i - 1; ++j)
		printf data[j]", "
	printf data[i]"]\r\n};"
}'
