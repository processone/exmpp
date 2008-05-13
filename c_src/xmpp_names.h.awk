# $Id$

BEGIN { print "char * xmpp_names_list[] = {"; }

/^[^#]/ {
	if (!already_processed[$1]) {
		print "\t\"" $1 "\",";
		already_processed[$1] = 1;
	}
}

END { print "\tNULL\n};"; }
