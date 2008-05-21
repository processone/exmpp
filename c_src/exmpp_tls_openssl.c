/* $Id$ */

#include <string.h>
#include <erl_driver.h>
#include <ei.h>
#include <openssl/ssl.h>

#define	DRIVER_NAME	exmpp_tls_openssl
#define	_S(s)		#s
#define	S(s)		_S(s)

#define	BUF_SIZE	1024

/* Control operations. */
enum {
	COMMAND_SET_MODE = 1,
	COMMAND_SET_IDENTITY,
	COMMAND_SET_PEER_VERIF,
	COMMAND_SET_TRUSTED_CERTS,
	COMMAND_SET_OPTIONS,
	COMMAND_PREPARE_HANDSHAKE,
	COMMAND_HANDSHAKE,
	COMMAND_SET_ENCRYPTED_INPUT,
	COMMAND_GET_DECRYPTED_INPUT,
	COMMAND_SET_DECRYPTED_OUTPUT,
	COMMAND_GET_ENCRYPTED_OUTPUT,
	COMMAND_SVN_REVISION
};

/* Mode. */
enum {
	TLS_MODE_UNKNOWN = 0,
	TLS_MODE_SERVER,
	TLS_MODE_CLIENT
};

/* Driver data. */
struct exmpp_tls_openssl_data {
	int		 mode;

	/* Identity. */
	char		*certificate;
	char		*private_key;

	/* Peer verification. */
	int		 verify_peer;
	char		*expected_id;

	/* Options. */
	int		 peer_cert_required;

	SSL_CTX		*ctx;
	SSL		*ssl;
	BIO		*bio_read;
	BIO		*bio_write;
};

static int	init_library(struct exmpp_tls_openssl_data *edd,
		    ei_x_buff **to_send, size_t *size, ErlDrvBinary **b);
static int	verify_callback(int preverify_ok, X509_STORE_CTX *x509_ctx);
static void	msg_callback(int write_p, int version, int content_type,
		    const void *buf, size_t len, SSL *ssl, void *arg);

#define	SKIP_VERSION(buf, index, version)	do {			\
	index = 0;							\
	ei_decode_version(buf, &index, &version);			\
} while (0)

#define	NEW_SEND_BUF(to_send)						\
	(to_send) = driver_alloc(sizeof(ei_x_buff));			\
	if ((to_send) == NULL)						\
		return (-1);						\
	ei_x_new_with_version((to_send));

#define	COPY_AND_FREE_BUF(to_send, size, b)				\
	(size) = (to_send)->index + 1;					\
	(b) = driver_alloc_binary((size));				\
	(b)->orig_bytes[0] = 1;						\
	memcpy((b)->orig_bytes + 1, (to_send)->buff,			\
	    (to_send)->index);						\
	ei_x_free((to_send));						\
	driver_free((to_send));

/* -------------------------------------------------------------------
 * Erlang port driver callbacks.
 * ------------------------------------------------------------------- */

static ErlDrvData
exmpp_tls_openssl_start(ErlDrvPort port, char *command)
{
	struct exmpp_tls_openssl_data *edd;

	/* Set binary mode. */
	set_port_control_flags(port, PORT_CONTROL_FLAG_BINARY);

	/* Allocate driver data structure. */
	edd = driver_alloc(sizeof(*edd));
	if (edd == NULL)
		return (NULL);

	edd->mode = TLS_MODE_UNKNOWN;
	edd->certificate = edd->private_key = NULL;
	edd->verify_peer = 0;
	edd->expected_id = NULL;
	edd->peer_cert_required = 0;

	edd->ctx = NULL;
	edd->ssl = NULL;

	return (ErlDrvData)edd;
}

static void
exmpp_tls_openssl_stop(ErlDrvData drv_data)
{
	struct exmpp_tls_openssl_data *edd;

	edd = (struct exmpp_tls_openssl_data *)drv_data;

	if (edd->certificate != NULL)
		driver_free(edd->certificate);
	if (edd->private_key != NULL)
		driver_free(edd->private_key);
	if (edd->expected_id != NULL)
		driver_free(edd->expected_id);

	driver_free(edd);
}

static int
exmpp_tls_openssl_control(ErlDrvData drv_data, unsigned int command,
    char *buf, int len, char **rbuf, int rlen)
{
	struct exmpp_tls_openssl_data *edd;
	int ret, index, version, arity, type, type_size;
	char auth_method[MAXATOMLEN];
	size_t size;
	long mode;
	unsigned long data_len;
	ErlDrvBinary *b;
	ei_x_buff *to_send;

	edd = (struct exmpp_tls_openssl_data *)drv_data;

	size = 0;
	b = NULL;

	switch (command) {
	case COMMAND_SET_MODE:
		SKIP_VERSION(buf, index, version);

		/* Get the mode (client vs. server). */
		ei_decode_long(buf, &index, &mode);
		edd->mode = mode;

		break;
	case COMMAND_SET_IDENTITY:
		SKIP_VERSION(buf, index, version);

		/* Get auth method. */
		ei_decode_tuple_header(buf, &index, &arity);
		ei_decode_atom(buf, &index, auth_method);
		if (strcmp(auth_method, "x509") != 0) {
			/* Only X.509 is supported by this port driver. */
			NEW_SEND_BUF(to_send);
			ei_x_encode_tuple_header(to_send, 2);
			ei_x_encode_atom(to_send, "unsupported_auth_method");
			ei_x_encode_string(to_send, auth_method);

			COPY_AND_FREE_BUF(to_send, size, b);

			break;
		}

		/* Get certificate filename. */
		ei_get_type(buf, &index, &type, &type_size);
		edd->certificate = driver_alloc(type_size + 1);
		if (edd->certificate == NULL)
			return (-1);
		ei_decode_string(buf, &index, edd->certificate);

		/* Get private key filename. */
		ei_get_type(buf, &index, &type, &type_size);
		edd->private_key = driver_alloc(type_size + 1);
		if (edd->private_key == NULL)
			return (-1);
		ei_decode_string(buf, &index, edd->private_key);

		break;
	case COMMAND_SET_PEER_VERIF:
		SKIP_VERSION(buf, index, version);

		/* Check if the identity of the remote peer must be
		 * verified. */
		ei_get_type(buf, &index, &type, &type_size);
		switch (type) {
		case ERL_ATOM_EXT:
			/* The peer will be checked by OpenSSL. */
			ei_decode_boolean(buf, &index, &(edd->verify_peer));
		case ERL_STRING_EXT:
			/* The peer will be checked by OpenSSL, then
			 * the certificate will be compared to the
			 * given expected identity. */
			edd->expected_id = driver_alloc(type_size + 1);
			if (edd->expected_id == NULL)
				return (-1);
			ei_decode_string(buf, &index, edd->expected_id);
			edd->verify_peer = 1;
		}

		break;
	case COMMAND_PREPARE_HANDSHAKE:
		ret = init_library(edd, &to_send, &size, &b);
		if (ret != 0) {
			/* Initialization failed. */
			break;
		}

		break;
	case COMMAND_HANDSHAKE:
		/* Try handshake. */
		ret = SSL_do_handshake(edd->ssl);
		if (ret <= 0) {
			switch (SSL_get_error(edd->ssl, ret)) {
			case SSL_ERROR_WANT_READ:
				/* OpenSSL is waiting for more data. */
				size = 1;
				b = driver_alloc_binary(size);
				b->orig_bytes[0] = 2;

				break;
			case SSL_ERROR_WANT_WRITE:
				/* OpenSSL need to send more data. */
				size = 1;
				b = driver_alloc_binary(size);
				b->orig_bytes[0] = 3;

				break;
			default:
				/* An error occured. */
				NEW_SEND_BUF(to_send);
				ei_x_encode_atom(to_send, "handshake_failed");

				COPY_AND_FREE_BUF(to_send, size, b);
			}
		}

		break;
	case COMMAND_SET_ENCRYPTED_INPUT:
		BIO_write(edd->bio_read, buf, len);

		break;
	case COMMAND_GET_DECRYPTED_INPUT:
		SKIP_VERSION(buf, index, version);

		/* Get data length the caller is waiting for. */
		ei_decode_ulong(buf, &index, &data_len);
		if (data_len == 0)
			data_len = BUF_SIZE;

		/* Allocate binary to copy decrypted data. */
		rlen = data_len + 1;
		size = 1;
		b = driver_alloc_binary(rlen);
		b->orig_bytes[0] = 0;

		/* Copy data. */
		ret = SSL_read(edd->ssl, b->orig_bytes + size, data_len);

		/* Check for errors. */
		if (ret > 0) {
			size += ret;
			b = driver_realloc_binary(b, size);
		} else {
			driver_free_binary(b);
			b = NULL;

			switch (SSL_get_error(edd->ssl, ret)) {
			case SSL_ERROR_WANT_READ:
				/* OpenSSL is waiting for more data. */
				size = 1;
				b = driver_alloc_binary(size);
				b->orig_bytes[0] = 2;

				break;
			case SSL_ERROR_WANT_WRITE:
				/* OpenSSL need to send more data. */
				size = 1;
				b = driver_alloc_binary(size);
				b->orig_bytes[0] = 3;

				break;
			default:
				/* An error occured. */
				NEW_SEND_BUF(to_send);
				ei_x_encode_atom(to_send, "decrypt_failed");

				COPY_AND_FREE_BUF(to_send, size, b);
			}
		}

		break;
	case COMMAND_SET_DECRYPTED_OUTPUT:
		ret = SSL_write(edd->ssl, buf, len);
		if (ret <= 0) {
			switch (SSL_get_error(edd->ssl, ret)) {
			case SSL_ERROR_WANT_READ:
				/* OpenSSL is waiting for more data. */
				size = 1;
				b = driver_alloc_binary(size);
				b->orig_bytes[0] = 2;

				break;
			case SSL_ERROR_WANT_WRITE:
				/* OpenSSL need to send more data. */
				size = 1;
				b = driver_alloc_binary(size);
				b->orig_bytes[0] = 3;

				break;
			default:
				/* An error occured. */
				NEW_SEND_BUF(to_send);
				ei_x_encode_atom(to_send, "encrypt_failed");

				COPY_AND_FREE_BUF(to_send, size, b);
			}
		}

		break;
	case COMMAND_GET_ENCRYPTED_OUTPUT:
		/* Allocate binary to copy encrypted data. */
		size = BUF_SIZE + 1;
		rlen = 1;
		b = driver_alloc_binary(size);
		b->orig_bytes[0] = 0;

		/* Copy data. */
		while ((ret = BIO_read(edd->bio_write,
		    b->orig_bytes + rlen, BUF_SIZE)) > 0) {
			rlen += ret;
			size += BUF_SIZE;
			b = driver_realloc_binary(b, size);
		}

		size = rlen;
		b = driver_realloc_binary(b, size);

		break;
	case COMMAND_SVN_REVISION:
		/* Store the revision in the buffer. */
		NEW_SEND_BUF(to_send);
		ei_x_encode_string(to_send, "$Revision$");

		COPY_AND_FREE_BUF(to_send, size, b);

		break;
	default:
		/* Commad not recognized. */
		NEW_SEND_BUF(to_send);
		ei_x_encode_tuple_header(to_send, 2);
		ei_x_encode_atom(to_send, "unknown_command");
		ei_x_encode_ulong(to_send, command);

		COPY_AND_FREE_BUF(to_send, size, b);
	}

	if (b == NULL) {
		size = 1;
		b = driver_alloc_binary(size);
		b->orig_bytes[0] = 0;
	}

	*rbuf = (char *)b;

	return (size);
}

/* -------------------------------------------------------------------
 * Internal functions.
 * ------------------------------------------------------------------- */

static int
init_library(struct exmpp_tls_openssl_data *edd,
    ei_x_buff **to_send, size_t *size, ErlDrvBinary **b)
{
	int ret, verify;

	/* Create an SSL context. */
	edd->ctx = SSL_CTX_new(SSLv23_method());
	if (edd->ctx == NULL) {
		NEW_SEND_BUF(*to_send);
		ei_x_encode_atom(*to_send,
		    "ssl_context_init_failed");

		COPY_AND_FREE_BUF(*to_send, *size, *b);

		goto err;
	}

	/* Set our certificate. */
	ret = SSL_CTX_use_certificate_chain_file(edd->ctx, edd->certificate);
	if (ret != 1) {
		NEW_SEND_BUF(*to_send);
		ei_x_encode_atom(*to_send,
		    "load_cert_failed");

		COPY_AND_FREE_BUF(*to_send, *size, *b);

		goto err;
	}

	/* Set the private key. */
	ret = SSL_CTX_use_PrivateKey_file(edd->ctx, edd->private_key,
	    SSL_FILETYPE_PEM);
	if (ret != 1) {
		NEW_SEND_BUF(*to_send);
		ei_x_encode_atom(*to_send,
		    "load_pk_failed");

		COPY_AND_FREE_BUF(*to_send, *size, *b);

		goto err;
	}

	/* Prepare OpenSSL for verification. */
	verify = edd->verify_peer ?
	    SSL_VERIFY_PEER | SSL_VERIFY_CLIENT_ONCE : SSL_VERIFY_NONE;
	verify |= edd->peer_cert_required ?
	    SSL_VERIFY_FAIL_IF_NO_PEER_CERT : 0;
	SSL_CTX_set_verify(edd->ctx, verify, verify_callback);

	/* Set debugging callback. */
	SSL_CTX_set_msg_callback(edd->ctx, msg_callback);

	/* Create an SSL connection handle. */
	edd->ssl = SSL_new(edd->ctx);
	if (edd->ssl == NULL) {
		NEW_SEND_BUF(*to_send);
		ei_x_encode_atom(*to_send,
		    "ssl_init_failed");

		COPY_AND_FREE_BUF(*to_send, *size, *b);

		goto err;
	}

	/* Associate buffers. */
	edd->bio_read = BIO_new(BIO_s_mem());
	edd->bio_write = BIO_new(BIO_s_mem());
	SSL_set_bio(edd->ssl, edd->bio_read, edd->bio_write);

	/* Set SSL state. */
	switch (edd->mode) {
	case TLS_MODE_SERVER:
		SSL_set_accept_state(edd->ssl);
		break;
	case TLS_MODE_CLIENT:
		SSL_set_connect_state(edd->ssl);
		break;
	}

	return (0);

err:
	if (edd->ssl != NULL)
		SSL_free(edd->ssl);
	if (edd->ctx != NULL)
		SSL_CTX_free(edd->ctx);

	return (-1);
}

static int
verify_callback(int preverify_ok, X509_STORE_CTX *x509_ctx)
{

	return (preverify_ok);
}

static void
msg_callback(int write_p, int version, int content_type,
    const void *buf, size_t len, SSL *ssl, void *arg)
{

	printf("%5d bytes %s\r\n", len, write_p == 1 ? "sent" : "received");
}

/* -------------------------------------------------------------------
 * Driver declaration.
 * ------------------------------------------------------------------- */

static ErlDrvEntry tls_openssl_driver_entry = {
	NULL,				/* init */
	exmpp_tls_openssl_start,	/* start */
	exmpp_tls_openssl_stop,		/* stop */
	NULL,				/* output */
	NULL,				/* ready_input */
	NULL,				/* ready_output */
	S(DRIVER_NAME),			/* driver name */
	NULL,				/* finish */
	NULL,				/* handle */
	exmpp_tls_openssl_control,	/* control */
	NULL,				/* timeout */
	NULL				/* outputv */
};

DRIVER_INIT(DRIVER_NAME)
{

	/* Initialize OpenSSL. */
	SSL_library_init();
	SSL_load_error_strings();

	return &tls_openssl_driver_entry;
}
