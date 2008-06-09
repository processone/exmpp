/* $Id$ */

var fso, fd, line, re, dict;

if (WScript.Arguments.length  != 1) {
	WScript.Echo("Syntax: cscript xmpp_attrs.h.js <xmpp_attrs.h.in>");
} else {
	/* Open the input file for reading (1). */
	fso = new ActiveXObject("Scripting.FileSystemObject");
	fd = fso.OpenTextFile(WScript.Arguments.Item(0), 1, false);

	/* This regexp filters out comments. */
	re = /^[^#]/;

	/* We need a dictionnary to filter out duplicates. */
	dict = new ActiveXObject("Scripting.Dictionary");

	/* Fill in the C header file. */
	WScript.Echo("char * xmpp_attrs_list[] = {");

	while (!fd.AtEndOfStream) {
		line = fd.ReadLine();
		if (!dict.Exists(line) && line.match(re)) {
			dict.Add(line, 1);
			WScript.Echo("\t\"" + line + "\",");
		}
	}
	WScript.Echo("\tNULL\n};");

	/* Done! */
	fd.close();
}
