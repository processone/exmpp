#!/bin/sh

coverage_to_html () {
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
<title>${module}: testsuite code coverage</title>
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
	border-bottom: solid 1px #000;
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
<h1>${module}: testsuite code coverage</h1>
<p id="timestamp">Date: ${timestamp}</p>
<pre>
EOF

	line_nb=`wc -l "$in"`
	tail -n $((${line_nb% *} - 4)) "$in" | sed								\
	-e 's,<,\&lt;,g' -e 's,>,\&gt;,g'									\
	-e 's,^\([[:space:]]*0\.\.|.*\),<span class="notcov">\1</span>,'					\
	-e 's,^\([[:space:]]*[1-9][0-9]*\.\.|.*\),<span class="cov">\1</span>,'					\
	-e 's,^\([[:space:]]*|[[:space:]][[:space:]]*% \$Id.*\),<span class="cvsid">\1</span>,'			\
	-e 's,^\([[:space:]]*|[[:space:]][[:space:]]*%.*\),<span class="comment">\1</span>,'			\
	-e 's,^\([[:space:]]*|[[:space:]]\{2\}[[:alpha:]][[:alpha:]]*.*\),<span class="func">\1</span>,'	\
	>> "$out"

	cat << EOF >> "$out"
</pre>
<p><a href="index.html">Back to modules list</a></p>
</body>
EOF

	# Create/update index.
	if [ ! -e index.html ]; then
		cat << EOF > index.html
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN"
  "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en">
<head>
<title>Testsuite code coverage</title>
<meta http-equiv="Content-Type" content="text/html; charset=UTF-8"/>
<style type="text/css">
body {
	color: #000;
	background-color: #fff;
}
h1 {
	margin: 0px;
}
#subtitle {
	margin-top: 0px;
	margin-bottom: 3em;
	color: #777;
	font-style: italic;
	border-bottom: solid 1px #000;
}
table {
	border: 1px solid #777;
	border-collapse: collapse;
}
td {
	border: 1px solid #777;
	padding: 4px;
}
.module {
	width: 400px;
}
.percent {
	text-align: right;
}
hr {
	text-align: left;
	margin: 0px auto 0px 0px;
	border: none;
	height: 8px;
}
</style>
</head>
<body>
<h1>Testsuite code coverage</h1>
<p id="subtitle">Modules list</p>
<table cellspacing="0" cellpadding="0">
<!-- MODULE -->
</table>
</body>
EOF
	fi

	coverage=`cat cover_${module}.percent`
	bar_length=$((${coverage%%.*} * 2))
	if [ $bar_length -gt 190 ]; then
		bar_color="#00a300"
	elif [ $bar_length -gt 100 ]; then
		bar_color="#df9601"
	else
		bar_color="#a32400"
	fi

	sed -i.bak -e "s|<!-- MODULE -->|<tr><td class=\"module\"><a href=\"$out\">$module</a></td><td class=\"percent\">$coverage%</td><td class=\"bar\"><hr style=\"width: ${bar_length}px; background-color: $bar_color;\"/></td></tr>\\
<!-- MODULE -->|" index.html && rm index.html.bak

	rm cover_${module}.percent
	rm cover_${module}.out
}

script=`basename $0`

flags="-tests ${TESTS} -covered_modules ${COVERED_MODULES}"

flags="$flags -top_srcdir ${TOP_SRCDIR:-.}"
flags="$flags -top_builddir ${TOP_BUILDDIR:-.}"

IFS=" 	" # To avoid newline strip

ret=`$ERL -pa ${TOP_BUILDDIR}/ebin -noshell -eval "${script}:check(), init:stop()." $flags`
linecount=`echo "$ret" | wc -l`
if test $linecount -gt 1; then
	output=`echo "$ret" | head -n $((linecount - 1))`
fi
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
					coverage_to_html $module
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
