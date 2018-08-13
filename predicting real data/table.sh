#!/bin/bash


IFS=$'\n' read -d '' -r -a lines < ./input.txt
length=${#lines[@]}
echo ${length}
echo "filename     M   obs   obs_nom pred_wc pred_wc_n pred_c pred_c_n var_g var_c1 var_c2" > output.txt

counter=0
while [ $counter -lt $length ]
do
	name=${lines[$counter]:8:10}
	#echo "${lines[$counter]}" | cut -d'_' -f 2 >> output.txt
	echo "......Running the ${counter}th file......"
	echo -n $name >> output.txt
	echo -n " " >> output.txt
	Rscript predictingRealData.R "${lines[$counter]}" >> output.txt 2> /dev/null
	Rscript RealDataFinal_cor.R "${lines[$counter]}" >> output.txt 2> /dev/null
	((counter++))
done

cat output.txt | tr -s " " > output2.txt