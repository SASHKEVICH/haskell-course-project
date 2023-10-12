#!/bin/bash

db_path=$1

if [[ -z "$db_path" ]]; then
    echo Provide database directory path.
    exit 1
fi

search_dir=`ls $db_path`
for entry in $search_dir
do
    if [[ $entry == "$(basename "$0")" ]]; then
        continue
    fi

    property=$( echo "$entry" | cut -f1 -d".")

    entry_path="$db_path/$entry"

    duplicates=$(jq -r ".${property}[].id" "$entry_path" | sort | uniq -d)
    if [ ! -z "$duplicates" ]; then
        echo "Duplicate ids found for property '${property}' in the '${entry}' file:"
        echo "$duplicates"

        exit 1
    fi
done

echo -e "\n"
echo All files are valid!
echo -e "\n"
