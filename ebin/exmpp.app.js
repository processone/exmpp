var fso, fd, line, arr, version;
var re_ac_init, re_version;
var re_compat_start, re_compat;

if (WScript.Arguments.length != 2) {
	WScript.Echo("Syntax: cscript " +
	    "exmpp.app.js <configure.ac> <exmpp.app.in>");
} else {
	/* Open configure.ac for reading (1). */
	fso = new ActiveXObject("Scripting.FileSystemObject");
	fd = fso.OpenTextFile(WScript.Arguments.Item(0), 1, false);

        /* Search the version number. */
	re_ac_init = /AC_INIT/;
	re_version = /], [\(\[^]\]\)/;

	while (!fd.AtEndOfStream) {
		line = fd.ReadLine();

		if (line.match(re_ac_init)) {
			arr = line.split(", ");
			arr = arr[1].substr(1).split("]");
			version = arr[0];
			break;
		}
	}

	fd.close();

	/* Open the input file for reading (1). */
	fd = fso.OpenTextFile(WScript.Arguments.Item(1), 1, false);

	/* Regexps used to replace macros. */
	re_version = /@VERSION@/;
	re_compat_start = /@COMPAT_MODULES_START@/;
	re_compat = /@COMPAT_MODULES@/;

	while (!fd.AtEndOfStream) {
		line = fd.ReadLine();

		line = line.replace(re_version, version);
		line = line.replace(re_compat_start, ",");
		line = line.replace(re_compat, "");

		WScript.Echo(line);
	}

	/* Done! */
	fd.close();
}
