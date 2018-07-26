#!/bin/bash


IFS=$'\n' read -d '' -r -a lines < ./input.txt
length=${#lines[@]}
echo ${length}

counter=0
while [ $counter -lt $length ]
do
	name=${lines[$counter]:8:10}
	#echo "${lines[$counter]}" | cut -d'_' -f 2 >> output.txt
	echo -n $name >> output.txt
	echo -n " " >> output.txt
	Rscript predictingRealData.R "${lines[$counter]}" >> output.txt
	Rscript isVar1.R "${lines[$counter]}" >> output.txt
	((counter++))
done
