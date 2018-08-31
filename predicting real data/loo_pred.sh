#!/bin/bash


IFS=$'\n' read -d '' -r -a lines < ./input.txt
length=${#lines[@]}
echo ${length}
echo "filename   pred_loo" > MLEoutput.txt

counter=0
while [ $counter -lt $length ]
do
	name=${lines[$counter]:8:10}
	#echo $counter
	#echo "${lines[$counter]}" | cut -d'_' -f 2 >> output.txt
	echo -n $name >> loo_Output.txt
	echo -n " " >> loo_Output.txt
	Rscript loo_predictions.R "${lines[$counter]}" >> loo_Output.txt
	((counter++))
done

cat loo_Output.txt | tr -s " " > loo_Output2.txt