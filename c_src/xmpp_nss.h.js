/* $Id$ */

var fso, fd, line, re;

if (WScript.Arguments.length  != 1) {
	WScript.Echo("Syntax: cscript xmpp_ns.h.js <xmpp_ns.h.in>");
} else {
	/* Open the input file for reading (1). */
	fso = new ActiveXObject("Scripting.FileSystemObject");
	fd = fso.OpenTextFile(WScript.Arguments.Item(0), 1, false);

	/* This regexp filters out comments. */
	re = /^[^#]/;

	/* Fill in the C header file. */
	WScript.Echo("char * xmpp_ns_list[] = {");

	while (!fd.AtEndOfStream) {
		line = fd.ReadLine()
			if (line.match(re))
				WScript.Echo("\t\"" + line + "\",");
	}
	WScript.Echo("\tNULL\n};");

	/* Done! */
	fd.close();
}
