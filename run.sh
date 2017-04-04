#!/usr/bin/env bash

ROOT="$(readlink -f $(dirname $0))"
EXE="$ROOT/dist/build/maudstats/maudstats"

[[ -x $(which cabal 2>/dev/null) ]] || {
	>&2 echo "[ FATAL ] Missing cabal executable!"
	exit 1
}
[[ -x $EXE ]] || {
	cabal install
	[[ $? == 0 ]] || {
		>&2 echo "[ FATAL ] Errors building maudstats!"
		exit 3
	}
}
# Get visiting IPs
function visiting {
	awk '/crunchy\.rocks/{printf "%.12s|%s\n",$4,$1}' nginx-access.log |
	cut -f2 -d[ 	|
	awk -v'FS=|' '
# Count duplicate IPs only once within the same day
{
	if (cur != $1) {
		printf "%s|%d\n", cur, n
		n = 0
		delete ips
	}
	cur = $1
	if (!($2 in ips)) {
		n++
		ips[$2] = 1
	}
}'

}

#paste -d'|' <(visiting) <(cabal run) | awk -v'FS=|' '
#NR > 2 {
	#labels[i] = $1
	#visits[$1] = $2
	#if (NF > 2)
		#posts[$3] = $4
	#++i
#}
#END {
	#for (j = 0; j < i; ++j) {
		#printf "%s: %d | %d\n", labels[j], visits[labels[j]], posts[labels[j]] 
	#}
#}'

paste -d'|' <(visiting) <(cabal run) | awk -v'FS=|' > $ROOT/frontend/data.js '
# Data format is visitDate/visits/postDate/posts
NR > 2 {
	++i
	labels[i] = $1
	visits[$1] = $2
	if (NF > 2)
		posts[$3] = $4
}

END {
	printf "\"use strict\";\r\nvar ext = {\r\n\tlabels: ["
	for (j = 1; j <= i - 1; ++j)
		printf "\""labels[j]"\", "

	printf "\""labels[i]"\"],\r\n\tvisits: ["
	for (j = 1; j <= i - 1; ++j)
		printf visits[labels[j]]", "
	printf visits[labels[i - 1]]"],\r\nposts: ["
	
	for (j = 1; j <= i - 1; ++j) {
		if (labels[j] in posts)
			printf posts[labels[j]]", "
		else
			printf "0, "
	}
	if (labels[i] in posts)
		printf posts[labels[i]]
	else
		printf "0"
	printf "]\r\n};"
}'
