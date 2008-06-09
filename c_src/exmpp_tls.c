/* $Id$ */

#include <stdlib.h>
#include <string.h>

#include "exmpp_tls.h"

#if defined(_WIN32)
#define	strcasecmp(s1, s2) _stricmp(s1, s2)
#endif

int
match_hostname(const char *cert_id, const char *expected_id)
{
	size_t cert_id_len;
	char *id;

	cert_id_len = strlen(cert_id);

	if (cert_id_len > 2 && cert_id[0] == '*' && cert_id[1] == '.') {
		/* The certificate contains a pattern like:
		 *     *.example.org
		 * Therefore, we look for the first dot in the expected_id.
		 */
		id = strchr(expected_id, '.');
		if (id == NULL)
			return (0);

		if (strcasecmp(&cert_id[1], id) == 0)
			return (1);
	} else {
		/* The certificate requires an exact match. */
		if (strcasecmp(cert_id, expected_id) == 0)
			return (1);
	}

	return (0);
}
