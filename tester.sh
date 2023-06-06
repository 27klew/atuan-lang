#!/bin/bash
for filename in good/*.hs; do
	name=${filename##*/}
    base=${name%.hs}
	echo "$base"
	
	timeout 2 ./Atuan/Interpreter  "$filename" > "good/$base.out"
	
	# timeout 2 ./Atuan/Interpreter  "$filename" > a.out

	# if !(diff a.out "good/$base.out"); then
	# 	exit 1
	# fi


done

echo "finished tests of good"

for filename in bad/*.hs; do
	name=${filename##*/}
    base=${name%.hs}
	echo "$base"
	
	# timeout 5 ./Atuan/Interpreter  "$filename" > "bad_out/$base.out"
	
	timeout 2 ./Atuan/Interpreter  "$filename" > a.out

	# diff a.out "good_out/$base.out"

	if !(diff a.out "bad_out/$base.out"); then
		exit 1
	fi

done