#!/usr/bin/bash

echo "Make sure AOC_COOKIE env var is set with the AoC cookie !"

if [ $# -ne 6 ]; then
	echo "Usage: $0 -y <year> -d <day> -l <aoc lang>"
	exit 1
fi

while getopts y:d:l: flag
do
    case "${flag}" in
        y) year=${OPTARG};;
        d) day=${OPTARG};;
        l) lang=${OPTARG};;
    esac
done

if [ -z $year ] || [ -z $day ] || [ -z $lang ] 
then 
	echo "Usage: $0 -y <year> -d <day> -o <aoc folder>"
	exit 1 
fi

output_folder="./aoc${year}-${lang}/data"
output="${output_folder}/day${day}.txt"
mkdir $output_folder
curl -H "Cookie: ${AOC_COOKIE}" -o ${output}\
	"https://adventofcode.com/${year}/day/${day}/input"

echo "Writtent to : ${output}"
exit 0
