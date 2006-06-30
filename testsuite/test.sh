#!/bin/sh

script=`basename $0`
script=${script%%.sh}

flags="-top_srcdir ${TOP_SRCDIR} -top_builddir ${TOP_BUILDIR} -tests ${TESTS} -covered_modules ${COVERED_MODULES}"

output=`$ERL -pa ../ebin -noshell -eval "${script}:check(), init:stop()." $flags`

case $output in
	PASS)
		exit 0
		;;
	*PASS)
		echo
		echo "Coverage:"
		echo "${output%%PASS}" | sed -e 's,#NL#,\n,g'
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
