for i in $(seq 1 $1); do tmux -c "mono Crawler.exe $(expr 9100+$i)"; done
