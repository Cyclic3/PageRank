for i in $(seq 1 $1);
	do
		LOL=$(expr 9100 + $i);
		echo $LOL;
		mono Crawler.exe $LOL &
		
 done
