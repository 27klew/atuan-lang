#!/bin/bash
for filename in good/*.hs; do
	name=${filename##*/}
    base=${name%.hs}
	echo "$base"
	
	# timeout 2 ./Atuan/Interpreter  "$filename" > "good/$base.out"
	
	timeout 3 ./Atuan/Interpreter  "$filename" > a.out

	if !(diff a.out "good/$base.out"); then
		exit 1
	fi


done

echo "Finished tests of good!"

for filename in bad/*.hs; do
	name=${filename##*/}
    base=${name%.hs}
	echo "$base"
	
	# timeout 2 ./Atuan/Interpreter  "$filename" > "bad/$base.out"
	
	timeout 3 ./Atuan/Interpreter  "$filename" > a.out


	if !(diff a.out "bad/$base.out"); then
		exit 1
	fi
done

rm a.out

echo "Finished tests of bad!"

echo "Testing Ended!"

