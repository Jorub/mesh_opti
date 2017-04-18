#!/bin/bash

for i in $(seq 1 $1); do
	if [ ! -d "$i" ]; then
 		mkdir $i
	fi
	echo $i
	cp -a drutemp/* $i
done

