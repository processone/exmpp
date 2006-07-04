# $Id$

BEGIN { print "char * xmpp_ns_list[] = {"; }

/^[^#]/ {
	print "\t\"" $1 "\",";
}

END { print "\tNULL\n};"; }
