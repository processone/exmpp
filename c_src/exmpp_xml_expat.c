/* $Id$ */

#include <ei.h>
#include <erl_driver.h>
#include <erl_interface.h>
#include <expat.h>
#include <string.h>

#include "contrib/hashtable.h"

#include "xmpp_nss.h"
#include "xmpp_names.h"
#include "xmpp_attrs.h"

#if defined(_WIN32)
#define	strdup(s) _strdup(s)
#endif

#define	DRIVER_NAME	exmpp_xml_expat
#define	_S(s)		#s
#define	S(s)		_S(s)

#define	TUPLE_DRV_OK			"ok"
#define	TUPLE_DRV_ERROR			"error"

#define	TUPLE_XML_ELEMENT		"xmlelement"
#define	TUPLE_XML_NS_ELEMENT		"xmlel"
#define	TUPLE_XML_ATTR			"xmlattr"
#define	TUPLE_XML_CDATA			"xmlcdata"
#define	TUPLE_XML_END_TAG		"xmlendtag"
#define	TUPLE_XML_ERROR			"xmlerror"

#define	NS_SEP				'|'

#define	SKIP_VERSION(buf)	do {					\
	index = 0;							\
	ei_decode_version(buf, &index, &version);			\
} while (0)

#define	MAKE_ATOM_OK(var)	do {					\
	var = driver_alloc(sizeof(ei_x_buff));				\
	if (var == NULL)						\
		return (-1);						\
	ei_x_new_with_version(var);					\
									\
	ei_x_encode_atom(var, TUPLE_DRV_OK);				\
} while (0)

/* Control operations. */
enum {
	EXPAT_SET_NSPARSER = 1,
	EXPAT_SET_NAMEASATOM,
	EXPAT_SET_CHECK_NS,
	EXPAT_SET_CHECK_NAMES,
	EXPAT_SET_CHECK_ATTRS,
	EXPAT_SET_MAXSIZE,
	EXPAT_SET_ROOTDEPTH,
	EXPAT_SET_ENDTAG,
	EXPAT_ADD_KNOWN_NS,
	EXPAT_ADD_KNOWN_NAME,
	EXPAT_ADD_KNOWN_ATTR,
	EXPAT_PARSE,
	EXPAT_PARSE_FINAL,
	EXPAT_SVN_REVISION
};

/* Structure to handle default namespace stack. */
struct ns_entry {
	char		*ns;
	struct ns_entry	*parent;
};

/* Driver data (also, user data for expat). */
struct exmpp_xml_expat_data {
	/* Internal state. */
	ErlDrvPort		 port;
	XML_Parser		 parser;
	unsigned long		 depth;
	long			 cur_size;
	ei_x_buff		*current_tree;
	ei_x_buff		*complete_trees;
	ei_x_buff		*declared_ns;
	struct ns_entry		*default_ns_stack;

	/* Lookup tables. */
	struct hashtable	*prefixes;
	struct hashtable	*new_ns;
	struct hashtable	*known_ns;
	struct hashtable	*known_names;
	struct hashtable	*known_attrs;

	/* Options. */
	int			 use_ns_parser;
	int			 name_as_atom;
	int			 check_ns;
	int			 check_names;
	int			 check_attrs;
	long			 max_size;
	long			 root_depth;
	int			 send_endtag;
};

/* Expat handler prototypes */
static void		expat_cb_start_namespace(void *user_data,
			    const char *prefix, const char *uri);
static void		expat_cb_end_namespace(void *user_data,
			    const char *prefix);
static void		expat_cb_start_element(void *user_data,
			    const char *name, const char **attrs);
static void		expat_cb_end_element(void *user_data,
			    const char *name);
static void		expat_cb_character_data(void *user_data,
			    const char *data, int len);

static int		create_parser(struct exmpp_xml_expat_data *ed);
static int		destroy_parser(struct exmpp_xml_expat_data *ed);
static int		current_tree_finished(
			    struct exmpp_xml_expat_data *ed);
static unsigned int	hash_djb2(void *key);
static int		hash_equalkeys(void *k1, void *k2);
static int		initialize_lookup_tables(
			    struct exmpp_xml_expat_data *ed);
static int		add_known_ns(struct exmpp_xml_expat_data *ed,
			    char *ns);
static int		add_known_name(struct exmpp_xml_expat_data *ed,
			    char *name);
static int		add_known_attr(struct exmpp_xml_expat_data *ed,
			    char *attr);
static int		is_a_known_ns(struct exmpp_xml_expat_data *ed,
			    const char *ns);
static int		is_a_known_name(struct exmpp_xml_expat_data *ed,
			    const char *name);
static int		is_a_known_attr(struct exmpp_xml_expat_data *ed,
			    const char *attr);

/* This constant is used as value in known_ns, known_names and known_attrs
 * hashtable. */
const int	KNOWN = 1;

/* XML namespace is implicitly declared. This constant is used in known_ns
 * and prefixes hashtables. */
#define	XML_NS	"http://www.w3.org/XML/1998/namespace"

/* -------------------------------------------------------------------
 * Workaround for EI encode_string bug.
 * ------------------------------------------------------------------- */

#if defined(EI_ENCODE_STRING_BUG)

#define	put8(s, n) do {							\
	(s)[0] = (char)((n) & 0xff);					\
	(s) += 1;							\
} while (0)

#define	put16be(s, n) do {						\
	(s)[0] = ((n) >>  8) & 0xff;					\
	(s)[1] = (n) & 0xff;						\
	(s) += 2;							\
} while (0)

#define	put32be(s, n) do {						\
	(s)[0] = ((n) >>  24) & 0xff;					\
	(s)[1] = ((n) >>  16) & 0xff;					\
	(s)[2] = ((n) >>  8) & 0xff;					\
	(s)[3] = (n) & 0xff;						\
	(s) += 4;							\
} while (0)

static int
ei_encode_string_len_fixed(char *buf, int *index, const char *p, int len)
{
	int i;
	char *s, *s0;

	s = buf + *index;
	s0 = s;

	if (len <= 0xffff) {
		if (!buf) {
			s += 3;
		} else {
			put8(s, ERL_STRING_EXT);
			put16be(s, len);
			memmove(s, p, len); /* Unterminated string. */
		}
		s += len;
	} else {
		if (!buf) {
			s += 6 + (2 * len);
		} else {
			/* Strings longer than 65535 are encoded as lists. */
			put8(s, ERL_LIST_EXT);
			put32be(s, len);

			for (i = 0; i < len; i++) {
				put8(s, ERL_SMALL_INTEGER_EXT);
				put8(s, p[i]);
			}

			put8(s, ERL_NIL_EXT);
		}
	}

	*index += s - s0;

	return (0);
}

/* x_fix_buff is an internal function of libei_st.a. */
extern int	x_fix_buff(ei_x_buff* x, int szneeded);

static int
ei_x_encode_string_len_fixed(ei_x_buff *x, const char *s, int len)
{
	int i;

	i = x->index;

	ei_encode_string_len_fixed(NULL, &i, s, len);
	if (!x_fix_buff(x, i))
		return (-1);
	return (ei_encode_string_len_fixed(x->buff, &x->index, s, len));
}

static int
ei_x_encode_string_fixed(ei_x_buff *x, const char *s)
{

	return (ei_x_encode_string_len_fixed(x, s, strlen(s)));
}

#else /* if !defined(EI_ENCODE_STRING_BUG) */

#define	ei_encode_string_len_fixed(buf, index, p, len)			\
    ei_encode_string_len(buf, index, p, len)
#define	ei_encode_string_fixed(buf, index, p)				\
    ei_encode_string(buf, index, p)

#define	ei_x_encode_string_len_fixed(x, s, len)				\
    ei_x_encode_string_len(x, s, len)
#define	ei_x_encode_string_fixed(x, s)					\
    ei_x_encode_string(x, s)

#endif /* defined(EI_ENCODE_STRING_BUG) */

/* -------------------------------------------------------------------
 * Erlang port driver callbacks.
 * ------------------------------------------------------------------- */

static ErlDrvData
exmpp_xml_expat_start(ErlDrvPort port, char *command)
{
	struct exmpp_xml_expat_data *ed;

	/* Set binary mode. */
	set_port_control_flags(port, PORT_CONTROL_FLAG_BINARY);

	/* Allocate driver data structure. */
	ed = driver_alloc(sizeof(*ed));
	if (ed == NULL)
		return (NULL);

	/* Fill in the structure. */
	ed->port             = port;
	ed->parser           = NULL;
	ed->depth            = 0;
	ed->cur_size         = 0;
	ed->current_tree     = NULL;
	ed->complete_trees   = NULL;
	ed->declared_ns      = NULL;
	ed->default_ns_stack = NULL;

	ed->prefixes         = NULL;
	ed->new_ns           = NULL;

	/* Without namespace support by default. */
	ed->use_ns_parser = 0;

	/* Encode tag and attribute names as string. */
	ed->name_as_atom = 0;

	/* Because namespaces (and maybe names) are encoded as atom(),
	 * they should require some verifications. */
	ed->check_ns    = 1;
	ed->check_names = 1;
	ed->check_attrs = 1;

	/* DOM-like behaviour by default. */
	ed->root_depth = 0;

	/* No size limit. */
	ed->max_size = -1;

	/* Do not send end element. */
	ed->send_endtag = 0;

	/* Initialize lookup tables. */
	if (initialize_lookup_tables(ed) != 0) {
		return (NULL);
	}

	return ((ErlDrvData)ed);
}

static void
exmpp_xml_expat_stop(ErlDrvData drv_data)
{
	struct exmpp_xml_expat_data *ed;

	ed = (struct exmpp_xml_expat_data *)drv_data;

	/* Destroy lookup tables, the Expat parser and the driver data
	 * structure. */
	if (ed->declared_ns) {
		ei_x_free(ed->declared_ns);
		driver_free(ed->declared_ns);
		ed->declared_ns = NULL;
	}
	if (ed->prefixes) {
		hashtable_destroy(ed->prefixes, 1);
		ed->prefixes = NULL;
	}
	if (ed->new_ns) {
		hashtable_destroy(ed->new_ns, 0);
		ed->new_ns = NULL;
	}
	if (ed->known_ns) {
		hashtable_destroy(ed->known_ns, 0);
		ed->known_ns = NULL;
	}
	if (ed->known_names) {
		hashtable_destroy(ed->known_names, 0);
		ed->known_names = NULL;
	}
	if (ed->known_attrs) {
		hashtable_destroy(ed->known_attrs, 0);
		ed->known_attrs = NULL;
	}

	destroy_parser(ed);

	driver_free(drv_data);
}

static int
exmpp_xml_expat_control(ErlDrvData drv_data, unsigned int command,
    char *buf, int len, char **rbuf, int rlen)
{
	size_t size;
	char *errmsg, *known;
	int ret, errcode, index, version, type;
	struct exmpp_xml_expat_data *ed;
	ErlDrvBinary *b;
	ei_x_buff *to_send;

	ed = (struct exmpp_xml_expat_data *)drv_data;

	ret = -1;
	*rbuf = NULL;
	to_send = NULL;

	switch (command) {
	case EXPAT_SET_MAXSIZE:
		/* Get the max size value. */
		SKIP_VERSION(buf);
		ei_decode_long(buf, &index, &(ed->max_size));

		MAKE_ATOM_OK(to_send);
		break;
	case EXPAT_SET_ROOTDEPTH:
		/* Get the root depth value. */
		SKIP_VERSION(buf);
		ei_decode_long(buf, &index, &(ed->root_depth));

		MAKE_ATOM_OK(to_send);
		break;
	case EXPAT_SET_ENDTAG:
		/* Get the "send end element" flag. */
		SKIP_VERSION(buf);
		ei_decode_boolean(buf, &index, &(ed->send_endtag));

		MAKE_ATOM_OK(to_send);
		break;
	case EXPAT_SET_NAMEASATOM:
		/* Get the "name as atom()" flag. */
		SKIP_VERSION(buf);
		ei_decode_boolean(buf, &index, &(ed->name_as_atom));

		MAKE_ATOM_OK(to_send);
		break;
	case EXPAT_SET_CHECK_NS:
		/* Get the "check namespaces" flag. */
		SKIP_VERSION(buf);
		ei_decode_boolean(buf, &index, &(ed->check_ns));

		MAKE_ATOM_OK(to_send);
		break;
	case EXPAT_SET_CHECK_NAMES:
		/* Get the "check names" flag. */
		SKIP_VERSION(buf);
		ei_decode_boolean(buf, &index, &(ed->check_names));

		MAKE_ATOM_OK(to_send);
		break;
	case EXPAT_SET_CHECK_ATTRS:
		/* Get the "check attributes" flag. */
		SKIP_VERSION(buf);
		ei_decode_boolean(buf, &index, &(ed->check_attrs));

		MAKE_ATOM_OK(to_send);
		break;
	case EXPAT_ADD_KNOWN_NS:
	case EXPAT_ADD_KNOWN_NAME:
	case EXPAT_ADD_KNOWN_ATTR:
		SKIP_VERSION(buf);
		ei_get_type(buf, &index, &type, &ret);

		known = malloc(ret + 1);
		if (known == NULL)
			return (-1);
		ei_decode_string(buf, &index, known);

		switch (command) {
		case EXPAT_ADD_KNOWN_NS:
			add_known_ns(ed, known);
			break;
		case EXPAT_ADD_KNOWN_NAME:
			add_known_name(ed, known);
			break;
		case EXPAT_ADD_KNOWN_ATTR:
			add_known_attr(ed, known);
			break;
		}

		/* 'known' is freed when the hashtable is destroyed. */

		MAKE_ATOM_OK(to_send);

		break;
	case EXPAT_SET_NSPARSER:
		/* Get the flag value. */
		SKIP_VERSION(buf);
		ei_decode_boolean(buf, &index, &(ed->use_ns_parser));

		/* Create the Expat parser. */
		destroy_parser(ed);

		/* Prepare prefixes cache table. */
		if (ed->prefixes != NULL) {
			hashtable_destroy(ed->prefixes, 1);
			ed->prefixes = NULL;
		}
		if (ed->new_ns != NULL) {
			hashtable_destroy(ed->new_ns, 0);
			ed->new_ns = NULL;
		}

		if (ed->use_ns_parser) {
			ed->prefixes = create_hashtable(16,
			    hash_djb2, hash_equalkeys);
			if (ed->prefixes == NULL)
				return (-1);

			/* Already add `xml' prefix which is implied. */
			hashtable_insert(ed->prefixes,
			    strdup(XML_NS),
			    strdup("xml"));

			ed->new_ns = create_hashtable(16,
			    hash_djb2, hash_equalkeys);
			if (ed->new_ns == NULL)
				return (-1);
		}

		/* Initialize the ei_x_buff buffer used to store the
		 * error. */
		to_send = driver_alloc(sizeof(ei_x_buff));
		if (to_send == NULL)
			return (-1);
		ei_x_new_with_version(to_send);

		if (create_parser(ed) != 0) {
			/* Store this information in the buffer. */
			ei_x_encode_tuple_header(to_send, 2);
			ei_x_encode_atom(to_send, TUPLE_DRV_ERROR);
			ei_x_encode_atom(to_send, "setup_parser_failed");

			if (ed->prefixes) {
				hashtable_destroy(ed->prefixes, 0);
				ed->prefixes = NULL;
			}
			if (ed->new_ns) {
				hashtable_destroy(ed->new_ns, 0);
				ed->new_ns = NULL;
			}
		} else {
			/* Store this information in the buffer. */
			ei_x_encode_atom(to_send, TUPLE_DRV_OK);
		}

		break;
	case EXPAT_PARSE:
	case EXPAT_PARSE_FINAL:
		if (ed->parser == NULL) {
			to_send = driver_alloc(sizeof(ei_x_buff));
			if (to_send == NULL)
				return (-1);
			ei_x_new_with_version(to_send);

			/* Store this information in the buffer. */
			ei_x_encode_tuple_header(to_send, 2);
			ei_x_encode_atom(to_send, TUPLE_DRV_ERROR);
			ei_x_encode_atom(to_send, "no_parser_configured");

			break;
		} else if (ed->max_size > -1 &&
		    len + ed->cur_size > ed->max_size) {
			to_send = driver_alloc(sizeof(ei_x_buff));
			if (to_send == NULL)
				return (-1);
			ei_x_new_with_version(to_send);

			if (create_parser(ed) == 0) {
				/* Store this information in the buffer. */
				ei_x_encode_tuple_header(to_send, 2);
				ei_x_encode_atom(to_send, TUPLE_DRV_ERROR);
				ei_x_encode_atom(to_send, "stanza_too_big");
			}

			break;
		}

		/* Start XML document parsing. */
		ret = XML_Parse(ed->parser, buf, len,
		    command == EXPAT_PARSE_FINAL);

		if (!ret) {
			/* Initialize the ei_x_buff buffer used to store the
			 * error code. */
			to_send = driver_alloc(sizeof(ei_x_buff));
			if (to_send == NULL)
				return (-1);
			ei_x_new_with_version(to_send);

			/* An error occured while parsing the document. */
			errcode = XML_GetErrorCode(ed->parser);
			errmsg = (char *)XML_ErrorString(errcode);

			/* Store this information in the buffer. */
			ei_x_encode_tuple_header(to_send, 2);
			ei_x_encode_atom(to_send, TUPLE_XML_ERROR);
			ei_x_encode_tuple_header(to_send, 2);
			ei_x_encode_long(to_send, errcode);
			ei_x_encode_string_fixed(to_send, errmsg);
		}

		/* Update the size of processed data. */
		if (command == EXPAT_PARSE_FINAL)
			ed->cur_size = 0;
		else
			ed->cur_size += len;

		if (to_send == NULL) {
			if (ed->complete_trees != NULL) {
				to_send = ed->complete_trees;
				ei_x_encode_empty_list(to_send);
			} else {
				/* Initialize the ei_x_buff buffer used to
				 * store the `continue' message. */
				to_send = driver_alloc(sizeof(ei_x_buff));
				if (to_send == NULL)
					return (-1);
				ei_x_new_with_version(to_send);

				/* Store this information in the buffer. */
				ei_x_encode_tuple_header(to_send, 2);
				ei_x_encode_atom(to_send, "ok");
				ei_x_encode_atom(to_send,
				    command == EXPAT_PARSE ?
				    "continue" : "done");
			}
		}

		break;
	case EXPAT_SVN_REVISION:
		to_send = driver_alloc(sizeof(ei_x_buff));
		if (to_send == NULL)
			return (-1);
		ei_x_new_with_version(to_send);

		/* Store this information in the buffer. */
		ei_x_encode_tuple_header(to_send, 2);
		ei_x_encode_atom(to_send, TUPLE_DRV_OK);
		ei_x_encode_string(to_send, "$Revision$");

		break;
	default:
		/* Initialize the ei_x_buff buffer used to store the
		 * error. */
		to_send = driver_alloc(sizeof(ei_x_buff));
		if (to_send == NULL)
			return (-1);
		ei_x_new_with_version(to_send);

		/* Store this information in the buffer. */
		ei_x_encode_tuple_header(to_send, 2);
		ei_x_encode_atom(to_send, TUPLE_DRV_ERROR);
		ei_x_encode_atom(to_send, "badarg");

		break;
	}

	/* Copy the final tuple in the response buffer. */
	size = to_send->index;
	b = driver_alloc_binary(size);
	memcpy(b->orig_bytes, to_send->buff, size);
	*rbuf = (char *)b;

	/* Free the returned tuple. */
	ei_x_free(to_send);
	driver_free(to_send);

	if (command == EXPAT_PARSE || command == EXPAT_PARSE_FINAL)
		ed->complete_trees = NULL;

	/* If necessary, reset the Expat parser. */
	/* XXX Necessary? */
	if (command == EXPAT_PARSE_FINAL) {
		destroy_parser(ed);
		create_parser(ed);
	}

	/* The return value is the size of the response buffer. */
	ret = (int)size;

	return (ret);
}

/* -------------------------------------------------------------------
 * Expat handlers.
 * ------------------------------------------------------------------- */

static void
expat_cb_start_namespace(void *user_data,
    const char *prefix, const char *uri)
{
	struct exmpp_xml_expat_data *ed;
	struct ns_entry *entry;
	size_t uri_len;

	ed = (struct exmpp_xml_expat_data *)user_data;

#if defined(DRV_DEBUG)
	printf("===> namespace (start): prefix=%s, uri=%s\r\n",
	    prefix, uri);
#endif

	if (ed->prefixes == NULL || ed->new_ns == NULL)
		return;

	if (prefix == NULL) {
		/* Push this default namespace on the stack. */
		entry = driver_alloc(sizeof(*entry));
		if (entry == NULL)
			return;

		/* We store an empty default namespace too, because the
		 * end_namespace callback will want to pop it from the
		 * stack. The element using this empty namespace won't
		 * use it because it won't be associated to any
		 * namespace. */
		uri_len = uri != NULL ? strlen(uri) : 0;
		entry->ns = driver_alloc(uri_len + 1);
		if (entry->ns != NULL) {
			if (uri_len > 0)
				memcpy(entry->ns, uri, uri_len);
			entry->ns[uri_len] = '\0';
		}
		entry->parent = ed->default_ns_stack;
		ed->default_ns_stack = entry;
	} else {
		/* Store the namespace and its prefix in the lookup table.
		 * We make a copy of `uri' because hashtable_remove and
		 * hashtable_destroy will free it. */
		if (uri == NULL)
			/* An empty prefixed namespace is illegal. */
			return;

		hashtable_insert(ed->prefixes, strdup(uri), strdup(prefix));
	}

	/* Build the declared_ns list. This list will be reset
	 * in expat_sb_start_element(). */
	if (ed->declared_ns == NULL) {
		ed->declared_ns = driver_alloc(sizeof(*(ed->declared_ns)));
		if (ed->declared_ns == NULL)
			return;
		ei_x_new(ed->declared_ns);
	}

	ei_x_encode_list_header(ed->declared_ns, 1);
	ei_x_encode_tuple_header(ed->declared_ns, 2);
	if (uri == NULL)
		ei_x_encode_atom(ed->declared_ns, "undefined");
	else if (is_a_known_ns(ed, uri))
		ei_x_encode_atom(ed->declared_ns, uri);
	else
		ei_x_encode_string_fixed(ed->declared_ns, uri);

	if (prefix == NULL)
		ei_x_encode_atom(ed->declared_ns, "none");
	else
		ei_x_encode_string_fixed(ed->declared_ns, prefix);

	/* Store the new namespace in the new_ns table. */
	if (uri != NULL)
		hashtable_insert(ed->new_ns, strdup(uri), "");
}

static void
expat_cb_end_namespace(void *user_data,
    const char *prefix)
{
	struct exmpp_xml_expat_data *ed;
	struct ns_entry *entry;

	ed = (struct exmpp_xml_expat_data *)user_data;

#if defined(DRV_DEBUG)
	printf("---> namespace (end): prefix=%s\r\n", prefix);
#endif

	if (prefix == NULL) {
		/* Pop namespace on top of the stack. */
		entry = ed->default_ns_stack;
		ed->default_ns_stack = entry->parent;

		driver_free(entry->ns);
		driver_free(entry);
	} else {
		/* We can't easily remove terminated namespaces from
		 * the lookup table because Expat only provide the
		 * prefix (we use the URI as the key). */
	}
}

static void
expat_cb_start_element(void *user_data,
    const char *name, const char **attrs)
{
	int i, is_known;
	char *ns_sep, *prefix;
	ei_x_buff *tree;
	struct exmpp_xml_expat_data *ed;

	ed = (struct exmpp_xml_expat_data *)user_data;

#if defined(DRV_DEBUG)
	printf("===> [depth=%02lu] element (start): name=%s\r\n",
	    ed->depth, name);
#endif

	/* If we're not at the required depth, we treat each node
	 * independantly.
	 * For instance, if ed->root_depth is 1, <stream/> node is
	 * sent separately but for <message/>, we are building a
	 * complete tree with the children. */

	if (ed->root_depth == -1 ||
	    ed->depth <= (unsigned long)ed->root_depth) {
		/* Initialize a buffer to work on a new tree. */
		tree = driver_alloc(sizeof(ei_x_buff));
		if (tree == NULL)
			return;
		ei_x_new(tree);
		ed->current_tree = tree;
	} else {
		/* Continue with the current tree. */
		tree = ed->current_tree;
		ei_x_encode_list_header(tree, 1);
	}

	/* With namespace support, the tuple will be of the form:
	 *   {xmlnselement, URI, [Declared_NS],
	 *     Node_Name, [Attrs], [Children]}
	 * Without namespace support, it will be:
	 *   {xmlelement, Node_Name, [Attrs], [Children]} */
	if (ed->use_ns_parser) {
		ei_x_encode_tuple_header(tree, 6);
		ei_x_encode_atom(tree, TUPLE_XML_NS_ELEMENT);
		ns_sep = strchr(name, NS_SEP);
		if (ns_sep == NULL) {
			/* Neither a namespace, nor a prefix. */
			ei_x_encode_atom(tree, "undefined");

			/* Copy the declared_ns list and terminate it. */
			if (ed->declared_ns != NULL &&
			    ed->declared_ns->index > 0)
				ei_x_append(tree, ed->declared_ns);
			ei_x_encode_empty_list(tree);

			/* Encode the element name. */
			if (ed->name_as_atom && is_a_known_name(ed, name)) {
				ei_x_encode_atom(tree, name);
			} else {
				ei_x_encode_string_fixed(tree, name);
			}
		} else {
			/* Terminate the namespace with a NUL character.
			 * This will be restored later. */
			*ns_sep = '\0';

			/* Check if the namespace is known, to decide if we
			 * encode it as an atom() or a string(). */
			is_known = is_a_known_ns(ed, name);
			if (is_known) {
				ei_x_encode_atom(tree, name);
			} else {
				ei_x_encode_string_fixed(tree, name);
			}

			/* Lookup a prefix and eventually encode it as a
			 * string() at the beginning of the declared_ns
			 * list. This namespace/prefix pair may already
			 * be in the declared_ns list, so do it only when
			 * required. */
			if (hashtable_search(ed->new_ns,
			    (char *)name) == NULL) {
				if (ed->prefixes) {
					prefix = (char *)hashtable_search(
					    ed->prefixes, (char *)name);
				} else {
					prefix = NULL;
				}

				if (prefix != NULL) {
					ei_x_encode_list_header(tree, 1);
					ei_x_encode_tuple_header(tree, 2);
					if (is_known) {
						ei_x_encode_atom(tree, name);
					} else {
						ei_x_encode_string_fixed(tree,
						    name);
					}
					ei_x_encode_string_fixed(tree, prefix);
				}
			}

			/* Copy the declared_ns list and terminate it. */
			ei_x_append(tree, ed->declared_ns);
			ei_x_encode_empty_list(tree);

			/* Restore the namespace separator. */
			*ns_sep = NS_SEP;

			/* Encode the element name. */
			if (ed->name_as_atom &&
			    is_a_known_name(ed, ns_sep + 1)) {
				ei_x_encode_atom(tree, ns_sep + 1);
			} else {
				ei_x_encode_string_fixed(tree, ns_sep + 1);
			}
		}

		/* We can now reset the declared_ns list. We only reset the
		 * index to avoid memory free/alloc. */
		if (ed->declared_ns != NULL)
			ed->declared_ns->index = 0;
	} else {
		ei_x_encode_tuple_header(tree, 4);
		ei_x_encode_atom(tree, TUPLE_XML_ELEMENT);

		/* Encode the element name. */
		if (ed->name_as_atom && is_a_known_name(ed, name)) {
			ei_x_encode_atom(tree, name);
		} else {
			ei_x_encode_string_fixed(tree, name);
		}
	}

	/* Count the number of attributes. */
	for (i = 0; attrs[i] != NULL; i += 2)
		;

	if (i > 0) {
		/* Attributes name/value pair are stored in a list. */
		ei_x_encode_list_header(tree, i/2);

		for (i = 0; attrs[i] != NULL; i += 2) {
			/* With namespace support, the tuple will be of
			 * the form:
			 *   {xmlattr, URI, Name, Value}
			 * Without namespace support, it will be:
			 *   {Name, Value} */
			if (ed->use_ns_parser) {
				ei_x_encode_tuple_header(tree, 5);
				ei_x_encode_atom(tree, TUPLE_XML_ATTR);
				ns_sep = strchr(attrs[i], NS_SEP);
				if (ns_sep == NULL) {
					/* Neither a namespace, nor a
					 * prefix. */
					ei_x_encode_atom(tree, "undefined");
					ei_x_encode_atom(tree, "undefined");

					/* Encode the attribute name. */
					if (ed->name_as_atom &&
					    is_a_known_attr(ed, attrs[i])) {
						ei_x_encode_atom(tree,
						    attrs[i]);
					} else {
						ei_x_encode_string_fixed(tree,
						    attrs[i]);
					}
				} else {
					/* Terminate the namespace with a NUL
					 * character. This will be restored
					 * later. */
					*ns_sep = '\0';

					/* Check if the namespace is known,
					 * to decide if wa encode it as an
					 * atom() or a string(). */
					if (is_a_known_ns(ed, attrs[i])) {
						ei_x_encode_atom(tree,
						    attrs[i]);
					} else {
						ei_x_encode_string_fixed(tree,
						    attrs[i]);
					}

					/* Lookup a prefix and eventually
					 * encode it as a string() in the
					 * buffer. */
					if (ed->prefixes) {
						prefix =
						    (char *)hashtable_search(
						    ed->prefixes,
						    (char *)attrs[i]);
					} else {
						prefix = NULL;
					}

					if (prefix != NULL) {
						ei_x_encode_string_fixed(tree,
						    prefix);
					} else {
						ei_x_encode_atom(tree,
						    "undefined");
					}

					/* Restore the namespace separator. */
					*ns_sep = NS_SEP;

					/* Encode the attribute name. */
					if (ed->name_as_atom &&
					    is_a_known_attr(ed, ns_sep + 1)) {
						ei_x_encode_atom(tree,
						    ns_sep + 1);
					} else {
						ei_x_encode_string_fixed(tree,
						    ns_sep + 1);
					}
				}
			} else {
				ei_x_encode_tuple_header(tree, 2);

				/* Encode the atttribute name. */
				if (ed->name_as_atom &&
				    is_a_known_attr(ed, attrs[i])) {
					ei_x_encode_atom(tree,
					    attrs[i]);
				} else {
					ei_x_encode_string_fixed(tree,
					    attrs[i]);
				}
			}

			/* Encode the attribute value. */
			ei_x_encode_string_fixed(tree, attrs[i + 1]);
		}
	}

	ei_x_encode_empty_list(tree);

	if (ed->root_depth == -1 ||
	    ed->depth < (unsigned long)ed->root_depth) {
		/* Standalone node are moved to the final list. */
		ei_x_encode_atom(tree, "undefined");
		current_tree_finished(ed);
	}

	ed->depth++;
}

static void
expat_cb_end_element(void *user_data,
    const char *name)
{
	char *ns_sep, *prefix;
	ei_x_buff *tree;
	struct exmpp_xml_expat_data *ed;

	ed = (struct exmpp_xml_expat_data *)user_data;

	if (ed->depth > 0)
		ed->depth--;

#if defined(DRV_DEBUG)
	printf("---> [depth=%02lu] element (end): name=%s\r\n",
	    ed->depth, name);
#endif

	if ((ed->root_depth == -1 ||
	    ed->depth < (unsigned long)ed->root_depth) &&
	    ed->send_endtag) {
		/* Initialize a buffer to work on a new tree. */
		tree = driver_alloc(sizeof(ei_x_buff));
		if (tree == NULL)
			return;
		ei_x_new(tree);
		ed->current_tree = tree;

		/* With namespace support, the tuple will be of the form:
		 *   {xmlendtag, URI, Node_Name}
		 * Without namespace support, it will be:
		 *   {xmlendtag, undefined, undefined, Node_Name} */
		if (ed->use_ns_parser) {
			ei_x_encode_tuple_header(tree, 4);
			ei_x_encode_atom(tree, TUPLE_XML_END_TAG);
			ns_sep = strchr(name, NS_SEP);
			if (ns_sep == NULL) {
				/* Neither a namespace, nor a prefix. */
				ei_x_encode_atom(tree, "undefined");
				ei_x_encode_atom(tree, "undefined");

				/* Encode the element name. */
				if (ed->name_as_atom &&
				    is_a_known_name(ed, name)) {
					ei_x_encode_atom(tree, name);
				} else {
					ei_x_encode_string_fixed(tree, name);
				}
			} else {
				/* Terminate the namespace with a NUL
				 * character. This will be restored later. */
				*ns_sep = '\0';

				/* Check if the namespace is known, to
				 * decide if we encode it as an atom()
				 * or a string(). */
				if (is_a_known_ns(ed, name)) {
					ei_x_encode_atom(tree, name);
				} else {
					ei_x_encode_string_fixed(tree, name);
				}

				/* Lookup a prefix and eventually encode it
				 * as a string() in the buffer. */
				if (ed->prefixes) {
					prefix = (char *)hashtable_search(
					    ed->prefixes, (char *)name);
				} else {
					prefix = NULL;
				}

				if (prefix != NULL) {
					ei_x_encode_string_fixed(tree, prefix);
				} else {
					ei_x_encode_atom(tree, "undefined");
				}

				/* Restore the namespace separator. */
				*ns_sep = NS_SEP;

				/* Encode the element name. */
				if (ed->name_as_atom &&
				    is_a_known_name(ed, ns_sep + 1)) {
					ei_x_encode_atom(tree, ns_sep + 1);
				} else {
					ei_x_encode_string_fixed(tree,
					    ns_sep + 1);
				}
			}
		} else {
			ei_x_encode_tuple_header(tree, 4);
			ei_x_encode_atom(tree, TUPLE_XML_END_TAG);

			/* Namespace and prefix are not filled. */
			ei_x_encode_atom(tree, "undefined");
			ei_x_encode_atom(tree, "undefined");

			/* Encode the element name. */
			if (ed->name_as_atom && is_a_known_name(ed, name)) {
				ei_x_encode_atom(tree, name);
			} else {
				ei_x_encode_string_fixed(tree, name);
			}
		}

		current_tree_finished(ed);
	} else if (ed->root_depth != -1 &&
	    ed->depth >= (unsigned long)ed->root_depth &&
	    ed->current_tree != NULL) {
		ei_x_encode_empty_list(ed->current_tree);

		if (ed->depth == ed->root_depth) {
			current_tree_finished(ed);
		}
	}
}

static void
expat_cb_character_data(void *user_data,
    const char *data, int len)
{
	struct exmpp_xml_expat_data *ed;

	ed = (struct exmpp_xml_expat_data *)user_data;

#if defined(DRV_DEBUG)
	printf("     character data: data=%.*s\r\n", len, data);
#endif

	if (ed->current_tree == NULL)
		return;

	ei_x_encode_list_header(ed->current_tree, 1);
	ei_x_encode_tuple_header(ed->current_tree, 2);
	ei_x_encode_atom(ed->current_tree, TUPLE_XML_CDATA);
	ei_x_encode_binary(ed->current_tree, data, len);
}

/* -------------------------------------------------------------------
 * Internal functions.
 * ------------------------------------------------------------------- */

static int
create_parser(struct exmpp_xml_expat_data *ed)
{

	if (ed->use_ns_parser) {
		ed->parser = XML_ParserCreateNS("UTF-8", NS_SEP);
	} else {
		ed->parser = XML_ParserCreate("UTF-8");
	}

	if (ed->parser == NULL)
		return (-1);

	/* Set Expat user data to be this structure. */
	XML_SetUserData(ed->parser, ed);

	/* Configure the Expat parser. */
	XML_SetElementHandler(ed->parser,
	    expat_cb_start_element,
	    expat_cb_end_element);
	XML_SetCharacterDataHandler(ed->parser,
	    expat_cb_character_data);

	if (ed->use_ns_parser) {
		XML_SetNamespaceDeclHandler(ed->parser,
		    expat_cb_start_namespace,
		    expat_cb_end_namespace);
	}

	return (0);
}

static int
destroy_parser(struct exmpp_xml_expat_data *ed)
{

	if (ed->parser != NULL)
		XML_ParserFree(ed->parser);

	ed->depth = 0;

	return (0);
}

static int
current_tree_finished(struct exmpp_xml_expat_data *ed)
{
	int ret;

	if (ed->complete_trees == NULL) {
		/* Allocate the complete trees list. */
		ed->complete_trees = driver_alloc(sizeof(ei_x_buff));
		if (ed->complete_trees == NULL)
			return (-1);
		ei_x_new_with_version(ed->complete_trees);
		ei_x_encode_tuple_header(ed->complete_trees, 2);
		ei_x_encode_atom(ed->complete_trees, "ok");
	}

	/* Add the current tree to the complete trees list. */
	ei_x_encode_list_header(ed->complete_trees, 1);
	ret = ei_x_append(ed->complete_trees, ed->current_tree);

	/* Clear the current tree. */
	ei_x_free(ed->current_tree);
	driver_free(ed->current_tree);
	ed->current_tree = NULL;

	return (ret);
}

static unsigned int
hash_djb2(void *key)
{
	int c;
	unsigned int hash;
	unsigned char *str;

	str = key;
	hash = 5381;
	while ((c = *str++))
		hash = ((hash << 5) + hash) + c; /* hash * 33 + c */

	return (hash);
}

static int
hash_equalkeys(void *k1, void *k2)
{

	return (strcmp((char *)k1, (char *)k2) == 0);
}

static int
initialize_lookup_tables(struct exmpp_xml_expat_data *ed)
{
	int i;

	/* Create the 3 tables. */
	ed->known_ns = create_hashtable(16, hash_djb2, hash_equalkeys);
	if (ed->known_ns == NULL)
		return (-1);

	ed->known_names = create_hashtable(16, hash_djb2, hash_equalkeys);
	if (ed->known_names == NULL)
		return (-1);

	ed->known_attrs = create_hashtable(16, hash_djb2, hash_equalkeys);
	if (ed->known_attrs == NULL)
		return (-1);

	/* Load XML known tokens. */
	hashtable_insert(ed->known_ns,    strdup(XML_NS), (int *)&KNOWN);
	hashtable_insert(ed->known_attrs, strdup("lang"), (int *)&KNOWN);

	/* Load custom namespaces. */
	for (i = 0; xmpp_ns_list[i] != NULL; ++i) {
		hashtable_insert(ed->known_ns,
		    strdup(xmpp_ns_list[i]), (int *)&KNOWN);
	}

	/* Load custom names. */
	for (i = 0; xmpp_names_list[i] != NULL; ++i) {
		hashtable_insert(ed->known_names,
		    strdup(xmpp_names_list[i]), (int *)&KNOWN);
	}

	/* Load custom attributes. */
	for (i = 0; xmpp_attrs_list[i] != NULL; ++i) {
		hashtable_insert(ed->known_attrs,
		    strdup(xmpp_attrs_list[i]), (int *)&KNOWN);
	}

	return (0);
}

static int
add_known_ns(struct exmpp_xml_expat_data *ed, char *ns)
{

	if (!ed->check_ns || ed->known_ns == NULL)
		return (1);

	if (is_a_known_ns(ed, ns))
		return (0);

	hashtable_insert(ed->known_ns, ns, (int *)&KNOWN);
	return (0);
}

static int
add_known_name(struct exmpp_xml_expat_data *ed, char *name)
{

	if (!ed->check_names || ed->known_names == NULL)
		return (1);

	if (is_a_known_name(ed, name))
		return (0);

	hashtable_insert(ed->known_names, name, (int *)&KNOWN);
	return (0);
}

static int
add_known_attr(struct exmpp_xml_expat_data *ed, char *attr)
{

	if (!ed->check_attrs || ed->known_attrs == NULL)
		return (1);

	if (is_a_known_attr(ed, attr))
		return (0);

	hashtable_insert(ed->known_attrs, attr, (int *)&KNOWN);
	return (0);
}

static int
is_a_known_ns(struct exmpp_xml_expat_data *ed, const char *ns)
{
	int *is_known;

	if (!ed->check_ns || ed->known_ns == NULL)
		return (1);

	is_known = hashtable_search(ed->known_ns, (char *)ns);
	return (is_known == NULL ? 0 : 1);
}

static int
is_a_known_name(struct exmpp_xml_expat_data *ed, const char *name)
{
	int *is_known;

	if (!ed->check_names || ed->known_names == NULL)
		return (1);

	is_known = hashtable_search(ed->known_names, (char *)name);
	return (is_known == NULL ? 0 : 1);
}

static int
is_a_known_attr(struct exmpp_xml_expat_data *ed, const char *attr)
{
	int *is_known;

	if (!ed->check_attrs || ed->known_attrs == NULL)
		return (1);

	is_known = hashtable_search(ed->known_attrs, (char *)attr);
	return (is_known == NULL ? 0 : 1);
}

/* -------------------------------------------------------------------
 * Driver declaration.
 * ------------------------------------------------------------------- */

static ErlDrvEntry expat_driver_entry = {
	NULL,				/* init */
	exmpp_xml_expat_start,		/* start */
	exmpp_xml_expat_stop,		/* stop */
	NULL,				/* output */
	NULL,				/* ready_input */
	NULL,				/* ready_output */
	S(DRIVER_NAME),			/* driver name */
	NULL,				/* finish */
	NULL,				/* handle */
	exmpp_xml_expat_control,	/* control */
	NULL,				/* timeout */
	NULL				/* outputv */
};

DRIVER_INIT(DRIVER_NAME)
{

	return &expat_driver_entry;
}
