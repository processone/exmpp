#!/bin/sh

coverity_to_html () {
	module=$1

	in=cover_${module}.out
	out=cover_${module}.html

	headline=`head -n 1 "$in"`
	timestamp=${headline#*COVER }

	# Escape existing HTML (documentation)
	cat << EOF > "$out"
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN"
  "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en">
<head>
<title>${module}: testsuite coverity</title>
<meta http-equiv="Content-Type" content="text/html; charset=UTF-8"/>
<style type="text/css">
body {
	color: #000;
	background-color: #fff;
}
h1 {
	margin: 0px;
}
#timestamp {
	margin-top: 0px;
	margin-bottom: 3em;
	color: #777;
	font-style: italic;
	border-bottom: solid 1px #777;
}
.cvsid, .func {
	font-weight: bold;
}
.cvsid {
	color: #461b7e;
}
.cov {
	color: #007a00;
}
.notcov {
	color: #f00;
}
.comment {
	color: #777;
}
</style>
</head>
<body>
<h1>${module}: testsuite coverity</h1>
<p id="timestamp">Date: ${timestamp}</p>
<pre>
EOF

	line_nb=`wc -l "$in"`
	tail -n $((${line_nb% *} - 4)) "$in" | sed -r			\
	-e 's,<,\&lt;,g' -e 's,>,\&gt;,g'				\
	-e 's,(.*0\.\.\|.*),<span class="notcov">\1</span>,'		\
	-e 's,(.*[0-9]+\.\.\|.*),<span class="cov">\1</span>,'		\
	-e 's,(.*\|\s+% \$Id.*),<span class="cvsid">\1</span>,'		\
	-e 's,(.*\|\s+%.*),<span class="comment">\1</span>,'		\
	-e 's,(.*\|\s{2}\w+.*),<span class="func">\1</span>,'		\
	>> "$out"

	cat << EOF >> "$out"
</pre>
</body>
EOF

	rm cover_${module}.out
}

script=`basename $0`

flags="-top_srcdir ${TOP_SRCDIR} -top_builddir ${TOP_BUILDIR} -tests ${TESTS} -covered_modules ${COVERED_MODULES}"

IFS=" " # To avoid newline strip

ret=`$ERL -pa ../ebin -noshell -eval "${script}:check(), init:stop()." $flags`
output=`echo "$ret" | head -n -1`
exit_code=`echo "$ret" | tail -n 1`

case $exit_code in
	PASS)
		if test "$output"; then
			echo
			echo $output
			echo
		fi

		# Post-processing
		case $script in
			check_coverity)
				for module in $COVERED_MODULES; do
					coverity_to_html $module
				done
				;;
			*)
				;;
		esac

		exit 0
		;;
	SKIP)
		exit 77
		;;
	*)
		echo $ret
		exit 1
		;;
esac
