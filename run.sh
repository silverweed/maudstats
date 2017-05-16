#!/usr/bin/env bash

ROOT="$(readlink -f $(dirname $0))"
NGINX_LOG="$ROOT/nginx-access.log"
EXE="$ROOT/dist/build/maudstats/maudstats"

pushd $ROOT

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
	grep -vf crawlers.txt $NGINX_LOG |
	grep -Ev '/(robots.txt|static/)' |
	awk '/\/\/(little\.)?crunchy\.rocks/{printf "%.12s|%s\n",$4,$1}' |
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
}
END {
	printf "%s|%d\n", cur, n
}'

}

# TODO: output ALL labels, even when there are 0 visits on a certain day
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
	printf visits[labels[i]]"],\r\nposts: ["
	
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

popd
