#!/bin/bash


IFS=$'\n' read -d '' -r -a lines < ./input.txt
length=${#lines[@]}
echo ${length}
echo "filename      MLEVar  Value" > MLEoutput.txt

counter=0
while [ $counter -lt $length ]
do
	name=${lines[$counter]:8:10}
	#echo $counter
	#echo "${lines[$counter]}" | cut -d'_' -f 2 >> output.txt
	echo -n $name >> MLEoutput.txt
	echo -n " " >> MLEoutput.txt
	Rscript MLEvar.R "${lines[$counter]}" >> MLEoutput.txt
	((counter++))
done

cat MLEoutput.txt | tr -s " " > MLEoutput2.txt