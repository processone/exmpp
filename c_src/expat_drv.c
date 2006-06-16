/* $Id$ */

#include <ei.h>
#include <erl_driver.h>
#include <erl_interface.h>
#include <expat.h>
#include <string.h>

#define	DRIVER_NAME	expat_drv
#define	_S(s)		#s
#define S(s)		_S(s)

/* Control operations. */
enum {
	EXPAT_SET_NSPARSER = 1,
	EXPAT_SET_MAXSIZE,
	EXPAT_SET_ROOTDEPTH,
	EXPAT_SET_ENDELEMENT,
	EXPAT_PARSE,
	EXPAT_PARSE_FINAL
};

/* Driver data (also, user data for expat). */
struct expat_drv_data {
	/* Internal state. */
	ErlDrvPort	 port;
	XML_Parser	 parser;
	unsigned long	 depth;
	long		 cur_size;
	ei_x_buff	*current_tree;
	ei_x_buff	*complete_trees;

	/* Options. */
	int		 use_ns_parser;
	long		 max_size;
	unsigned long	 root_depth;
	int		 send_endelement;
};

#define	TUPLE_DRV_OK			"ok"
#define	TUPLE_DRV_ERROR			"error"
#define	TUPLE_XML_ELEMENT		"xmlelement"
#define	TUPLE_XML_NS_ELEMENT		"xmlnselement"
#define	TUPLE_XML_CDATA			"xmlcdata"
#define	TUPLE_XML_END_ELEMENT		"xmlendelement"
#define	TUPLE_XML_NS_END_ELEMENT	"xmlnsendelement"
#define	TUPLE_XML_ERROR			"xmlerror"

#define	NS_SEP				'|'

/* Expat handler prototypes */
void	expat_drv_start_namespace(void *user_data,
	    const char *prefix, const char *uri);
void	expat_drv_end_namespace(void *user_data,
	    const char *prefix);
void	expat_drv_start_element(void *user_data,
	    const char *name, const char **attrs);
void	expat_drv_end_element(void *user_data,
	    const char *name);
void	expat_drv_character_data(void *user_data,
	    const char *data, int len);

int	create_parser(struct expat_drv_data *ed);
int	destroy_parser(struct expat_drv_data *ed);
int	current_tree_finished(struct expat_drv_data *ed);

/* -------------------------------------------------------------------
 * Workaround for EI encode_string bug.
 * -------------------------------------------------------------------*/

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

int
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

int
ei_encode_string_fixed(char *buf, int *index, const char *p)
{

	return (ei_encode_string_len_fixed(buf, index, p, strlen(p)));
}

/* x_fix_buff is an internal function of libei_st.a. */
extern int	x_fix_buff(ei_x_buff* x, int szneeded);

int ei_x_encode_string_len_fixed(ei_x_buff *x, const char *s, int len)
{
	int i;

	i = x->index;

	ei_encode_string_len_fixed(NULL, &i, s, len);
	if (!x_fix_buff(x, i))
		return (-1);
	return (ei_encode_string_len_fixed(x->buff, &x->index, s, len));
}

int
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
 * -------------------------------------------------------------------*/

static ErlDrvData
expat_drv_start(ErlDrvPort port, char *command)
{
	struct expat_drv_data *ed;

	/* Set binary mode. */
	set_port_control_flags(port, PORT_CONTROL_FLAG_BINARY);

	/* Allocate driver data structure. */
	ed = driver_alloc(sizeof(struct expat_drv_data));
	if (ed == NULL)
		return (NULL);

	/* Fill in the structure. */
	ed->port           = port;
	ed->parser         = NULL;
	ed->depth          = 0;
	ed->cur_size       = 0;
	ed->current_tree   = NULL;
	ed->complete_trees = NULL;

	/* Without namespace support by default. */
	ed->use_ns_parser = 0;

	/* DOM-like behaviour by default. */
	ed->root_depth = 0;

	/* No size limit. */
	ed->max_size = -1;

	/* Do not send end element. */
	ed->send_endelement = 0;

	return ((ErlDrvData)ed);
}

static void
expat_drv_stop(ErlDrvData drv_data)
{

	/* Destroy the Expat parser and the driver data structure. */
	destroy_parser((struct expat_drv_data *)drv_data);
	driver_free(drv_data);

	return;
}

static int
expat_drv_control(ErlDrvData drv_data, unsigned int command,
    char *buf, int len, char **rbuf, int rlen)
{
	size_t size;
	char *errmsg;
	int ret, errcode, index, version;
	struct expat_drv_data *ed;
	ErlDrvBinary *b;
	ei_x_buff *to_send;

	ed = (struct expat_drv_data *)drv_data;

	ret = -1;
	*rbuf = NULL;
	to_send = NULL;

	switch (command) {
	case EXPAT_SET_MAXSIZE:
		/* Get the max size value. */
		index = 0;
		ei_decode_version(buf, &index, &version); /* Skip version. */
		ei_decode_long(buf, &index, &(ed->max_size));

		/* Initialize the ei_x_buff buffer used to store the
		 * error. */
		to_send = driver_alloc(sizeof(ei_x_buff));
		if (to_send == NULL)
			return (-1);
		ei_x_new_with_version(to_send);

		/* Store this information in the buffer. */
		ei_x_encode_atom(to_send, TUPLE_DRV_OK);

		break;
	case EXPAT_SET_ROOTDEPTH:
		/* Get the root depth value. */
		index = 0;
		ei_decode_version(buf, &index, &version); /* Skip version. */
		ei_decode_ulong(buf, &index, &(ed->root_depth));

		/* Initialize the ei_x_buff buffer used to store the
		 * error. */
		to_send = driver_alloc(sizeof(ei_x_buff));
		if (to_send == NULL)
			return (-1);
		ei_x_new_with_version(to_send);

		/* Store this information in the buffer. */
		ei_x_encode_atom(to_send, TUPLE_DRV_OK);

		break;
	case EXPAT_SET_ENDELEMENT:
		/* Get the send end element flag. */
		index = 0;
		ei_decode_version(buf, &index, &version); /* Skip version. */
		ei_decode_boolean(buf, &index, &(ed->send_endelement));

		/* Initialize the ei_x_buff buffer used to store the
		 * error. */
		to_send = driver_alloc(sizeof(ei_x_buff));
		if (to_send == NULL)
			return (-1);
		ei_x_new_with_version(to_send);

		/* Store this information in the buffer. */
		ei_x_encode_atom(to_send, TUPLE_DRV_OK);

		break;
	case EXPAT_SET_NSPARSER:
		/* Get the flag value. */
		index = 0;
		ei_decode_version(buf, &index, &version); /* Skip version. */
		ei_decode_boolean(buf, &index, &(ed->use_ns_parser));

		/* Create the Expat parser. */
		destroy_parser(ed);

		/* Initialize the ei_x_buff buffer used to store the
		 * error. */
		to_send = driver_alloc(sizeof(ei_x_buff));
		if (to_send == NULL)
			return (-1);
		ei_x_new_with_version(to_send);

		if (create_parser(ed) == 0) {
			/* Store this information in the buffer. */
			ei_x_encode_tuple_header(to_send, 2);
			ei_x_encode_atom(to_send, TUPLE_DRV_ERROR);
			ei_x_encode_atom(to_send, "setup_parser_failed");
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
			ei_x_encode_atom(to_send,
			    "no_parser_configured");

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
				ei_x_encode_atom(to_send,
				    "stanza_too_big");
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
	default:
		/* Initialize the ei_x_buff buffer used to store the
		 * error. */
		to_send = driver_alloc(sizeof(ei_x_buff));
		if (to_send == NULL)
			return (-1);
		ei_x_new_with_version(to_send);

		/* Store this information in the buffer. */
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

void
expat_drv_start_namespace(void *user_data,
    const char *prefix, const char *uri)
{
	struct expat_drv_data *ed;

	ed = (struct expat_drv_data *)user_data;

#if defined(DRV_DEBUG)
	printf("===> namespace (start): prefix=%s, uri=%s\r\n",
	    prefix, uri);
#endif
}

void
expat_drv_end_namespace(void *user_data,
    const char *prefix)
{
	struct expat_drv_data *ed;

	ed = (struct expat_drv_data *)user_data;

#if defined(DRV_DEBUG)
	printf("---> namespace (end): prefix=%s\r\n", prefix);
#endif
}

void
expat_drv_start_element(void *user_data,
    const char *name, const char **attrs)
{
	int i;
	size_t attr_name_len;
	char *ns_sep, *attr_name;
	ei_x_buff *tree;
	struct expat_drv_data *ed;

	ed = (struct expat_drv_data *)user_data;

#if defined(DRV_DEBUG)
	printf("===> [depth=%02lu] element (start): name=%s\r\n",
	    ed->depth, name);
#endif

	/* If we're not at the required depth, we treat each node
	 * independantly.
	 * For instance, if ed->root_depth is 1, <stream/> node is
	 * sent separately but for <message/>, we are building a
	 * complete tree with the children. */

	if (ed->depth <= ed->root_depth) {
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
	 *   {xmlnselement, URI, Node_Name, [Attrs], [Children]}
	 * Without namespace support, it will be:
	 *   {xmlelement, Node_Name, [Attrs], [Children]} */
	if (ed->use_ns_parser) {
		ei_x_encode_tuple_header(tree, 5);
		ei_x_encode_atom(tree, TUPLE_XML_NS_ELEMENT);
		ns_sep = strchr(name, NS_SEP);
		if (ns_sep == NULL) {
			ei_x_encode_atom(tree, "undefined");
			ei_x_encode_string_fixed(tree, name);
		} else {
			ei_x_encode_atom_len(tree, name,
			    ns_sep - name);
			ei_x_encode_string_fixed(tree, ns_sep + 1);
		}
	} else {
		ei_x_encode_tuple_header(tree, 4);
		ei_x_encode_atom(tree, TUPLE_XML_ELEMENT);
		ei_x_encode_string_fixed(tree, name);
	}

	/* Count the number of attributes. */
	for (i = 0; attrs[i] != NULL; i += 2)
		;

	if (i > 0) {
		/* Attributes name/value pair are stored in a list. */
		ei_x_encode_list_header(tree, i/2);

		for (i = 0; attrs[i] != NULL; i += 2) {
			/* With namespace support enabled, prefixed
			 * attributes, like xml:lang, will have a name
			 * of the form uri|name. */
			ns_sep = strchr(attrs[i], NS_SEP);
			if (ns_sep == NULL) {
				ei_x_encode_tuple_header(tree, 2);
				ei_x_encode_string_fixed(tree, attrs[i]);
				ei_x_encode_string_fixed(tree, attrs[i + 1]);
			} else if (strncmp(
			    "http://www.w3.org/XML/1998/namespace",
			    attrs[i], ns_sep - attrs[i]) == 0) {
				/* Namespace prefix was "xml", restore it
				 * because nobody use the URI. */
				attr_name_len = strlen(ns_sep + 1);
				attr_name = driver_alloc(attr_name_len + 4);
				if (attr_name == NULL) {
					ei_x_encode_tuple_header(tree, 2);
					ei_x_encode_string_fixed(tree,
					    attrs[i]);
					ei_x_encode_string_fixed(tree,
					    attrs[i + 1]);
				}
				memcpy(attr_name, "xml:", 4);
				memcpy(attr_name + 4, ns_sep + 1,
				    attr_name_len);
				ei_x_encode_tuple_header(tree, 2);
				ei_x_encode_string_len_fixed(tree, attr_name,
				    attr_name_len + 4);
				ei_x_encode_string_fixed(tree, attrs[i + 1]);
				driver_free(attr_name);
			} else {
				/* Unknown namespace and prefix lost... */
				ei_x_encode_tuple_header(tree, 2);
				ei_x_encode_string_fixed(tree, attrs[i]);
				ei_x_encode_string_fixed(tree, attrs[i + 1]);
			}
		}
	}

	ei_x_encode_empty_list(tree);

	if (ed->depth < ed->root_depth) {
		/* Standalone node are moved to the final list. */
		ei_x_encode_empty_list(tree);
		current_tree_finished(ed);
	}

	ed->depth++;
}

void
expat_drv_end_element(void *user_data,
    const char *name)
{
	char *ns_sep;
	ei_x_buff *tree;
	struct expat_drv_data *ed;

	ed = (struct expat_drv_data *)user_data;

	if (ed->depth > 0)
		ed->depth--;

#if defined(DRV_DEBUG)
	printf("---> [depth=%02lu] element (end): name=%s\r\n",
	    ed->depth, name);
#endif

	if (ed->depth < ed->root_depth && ed->send_endelement) {
		/* Initialize a buffer to work on a new tree. */
		tree = driver_alloc(sizeof(ei_x_buff));
		if (tree == NULL)
			return;
		ei_x_new(tree);
		ed->current_tree = tree;

		/* With namespace support, the tuple will be of the form:
		 *   {xmlendnselement, URI, Node_Name}
		 * Without namespace support, it will be:
		 *   {xmlendelement, Node_Name} */
		if (ed->use_ns_parser) {
			ei_x_encode_tuple_header(tree, 3);
			ei_x_encode_atom(tree, TUPLE_XML_NS_END_ELEMENT);
			ns_sep = strchr(name, NS_SEP);
			if (ns_sep == NULL) {
				ei_x_encode_atom(tree, "undefined");
				ei_x_encode_string_fixed(tree, name);
			} else {
				ei_x_encode_atom_len(tree, name,
				    ns_sep - name);
				ei_x_encode_string_fixed(tree, ns_sep + 1);
			}
		} else {
			ei_x_encode_tuple_header(tree, 2);
			ei_x_encode_atom(tree, TUPLE_XML_END_ELEMENT);
			ei_x_encode_string_fixed(tree, name);
		}

		current_tree_finished(ed);
	} else if (ed->depth >= ed->root_depth && ed->current_tree != NULL) {
		ei_x_encode_empty_list(ed->current_tree);

		if (ed->depth == ed->root_depth) {
			current_tree_finished(ed);
		}
	}
}

void
expat_drv_character_data(void *user_data,
    const char *data, int len)
{
	struct expat_drv_data *ed;

	ed = (struct expat_drv_data *)user_data;

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
 * -------------------------------------------------------------------*/

int
create_parser(struct expat_drv_data *ed)
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
	    expat_drv_start_element,
	    expat_drv_end_element);
	XML_SetCharacterDataHandler(ed->parser,
	    expat_drv_character_data);

	if (ed->use_ns_parser) {
		XML_SetNamespaceDeclHandler(ed->parser,
		    expat_drv_start_namespace,
		    expat_drv_end_namespace);
	}

	return (0);
}

int destroy_parser(struct expat_drv_data *ed)
{

	if (ed->parser != NULL)
		XML_ParserFree(ed->parser);

	ed->depth = 0;

	return (0);
}

int
current_tree_finished(struct expat_drv_data *ed)
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
	ed->current_tree = NULL;

	return (ret);
}

/* -------------------------------------------------------------------
 * Driver declaration.
 * ------------------------------------------------------------------- */

static ErlDrvEntry expat_driver_entry = {
	NULL,			/* init */
	expat_drv_start,	/* start */
	expat_drv_stop,		/* stop */
	NULL,			/* output */
	NULL,			/* ready_input */
	NULL,			/* ready_output */
	S(DRIVER_NAME),		/* driver name */
	NULL,			/* finish */
	NULL,			/* handle */
	expat_drv_control,	/* control */
	NULL,			/* timeout */
	NULL			/* outputv */
};

DRIVER_INIT(DRIVER_NAME)
{

	return &expat_driver_entry;
}
