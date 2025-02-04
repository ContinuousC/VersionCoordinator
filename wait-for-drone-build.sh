#!/bin/bash

if [[ $# -ne 2 ]]; then
    echo "Usage: $0 <repo> <tag>" >&2
    exit 1
fi

repo=$1
tag=$2


echo -n "Looking for build $repo $tag..."

for i in $(seq 60); do
    build=$(drone build ls "$repo" --event tag --limit 10 --format $'Build #{{.Number}}\nRef: {{.Ref}}\n' 2>/dev/null |
		grep -B1 -x "Ref: refs/tags/$tag" | head -n1 | sed -e 's/^Build #//')

    if [[ -n "$build" ]]; then
	echo " found"
	break
    fi
	
    echo -n "."
    sleep 1
done

if [[ -z "$build" ]]; then
    echo " timeout"
    exit 1
fi

echo -n "Waiting for $repo $tag (build #$build)..."

for i in $(seq 300); do

    status=$(drone build info "$repo" "$build" --format '{{.Status}}' 2> /dev/null)
    
    if [[ "$status" == "success" ]]; then
	echo " $status"
	exit 0
    elif [[ "$status" != "running" ]]; then
	 echo " $status"
	 exit 1
    fi
	 
    echo -n "."
    sleep 1
done

echo " timeout"
exit 1
