#!/bin/sh

script=`basename $0`
script=${script%%.sh}

output=`$ERL -pa ../ebin -noshell -eval "${script}:check(), init:stop()."`
case $output in
	PASS)
		exit 0
		;;
	SKIP)
		exit 77
		;;
	*)
		echo $output
		exit 1
		;;
esac
