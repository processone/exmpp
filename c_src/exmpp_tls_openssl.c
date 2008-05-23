/* $Id$ */

#include <string.h>
#include <erl_driver.h>
#include <ei.h>
#include <openssl/ssl.h>
#include <openssl/err.h>
#include <openssl/x509v3.h>

#include "exmpp_tls.h"

#define	DRIVER_NAME	exmpp_tls_openssl
#define	_S(s)		#s
#define	S(s)		_S(s)

#define	BUF_SIZE	1024

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
	char		*trusted_certs;
	int		 peer_cert_required;
	int		 accept_expired_cert;
	int		 accept_revoked_cert;
	int		 accept_non_trusted_cert;
	int		 accept_corrupted_cert;

	SSL_CTX		*ctx;
	SSL		*ssl;
	BIO		*bio_read;
	BIO		*bio_write;
};

static int	init_library(struct exmpp_tls_openssl_data *edd,
		    ei_x_buff **to_send, size_t *size, ErlDrvBinary **b);
static int	verify_callback(int preverify_ok, X509_STORE_CTX *x509_ctx);

static int	ssl_ex_index;

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
	edd->trusted_certs = NULL;
	edd->peer_cert_required = 0;
	edd->accept_expired_cert = 0;
	edd->accept_revoked_cert = 0;
	edd->accept_non_trusted_cert = 0;
	edd->accept_corrupted_cert = 0;

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
	int ret, index, version, arity, type, type_size, flag;
	char atom[MAXATOMLEN];
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
		ei_decode_atom(buf, &index, atom);
		if (strcmp(atom, "x509") != 0) {
			/* Only X.509 is supported by this port driver. */
			NEW_SEND_BUF(to_send);
			ei_x_encode_tuple_header(to_send, 2);
			ei_x_encode_atom(to_send, "unsupported_auth_method");
			ei_x_encode_string(to_send, atom);

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
			break;
		case ERL_STRING_EXT:
			/* The peer will be checked by OpenSSL, then
			 * the certificate will be compared to the
			 * given expected identity. */
			edd->expected_id = driver_alloc(type_size + 1);
			if (edd->expected_id == NULL)
				return (-1);
			ei_decode_string(buf, &index, edd->expected_id);
			edd->verify_peer = 1;
			break;
		}

		break;
	case COMMAND_SET_TRUSTED_CERTS:
		SKIP_VERSION(buf, index, version);

		/* Get auth method. */
		ei_decode_tuple_header(buf, &index, &arity);
		ei_decode_atom(buf, &index, atom);
		if (strcmp(atom, "x509") != 0) {
			/* Only X.509 is supported by this port driver. */
			NEW_SEND_BUF(to_send);
			ei_x_encode_tuple_header(to_send, 2);
			ei_x_encode_atom(to_send, "unsupported_auth_method");
			ei_x_encode_string(to_send, atom);

			COPY_AND_FREE_BUF(to_send, size, b);

			break;
		}

		/* Get the filename for the trusted certificates. */
		ei_get_type(buf, &index, &type, &type_size);
		edd->trusted_certs = driver_alloc(type_size + 1);
		if (edd->trusted_certs == NULL)
			return (-1);
		ei_decode_string(buf, &index, edd->trusted_certs);

		break;
	case COMMAND_SET_OPTIONS:
		SKIP_VERSION(buf, index, version);

		/* Get auth method. */
		ei_decode_tuple_header(buf, &index, &arity);
		ei_decode_atom(buf, &index, atom);
		ei_decode_boolean(buf, &index, &flag);

		if (strcmp(atom, "peer_cert_required") == 0)
			edd->peer_cert_required = flag;
		else if (strcmp(atom, "accept_expired_cert") == 0)
			edd->accept_expired_cert = flag;
		else if (strcmp(atom, "accept_non_trusted_cert") == 0)
			edd->accept_non_trusted_cert = flag;
		else if (strcmp(atom, "accept_revoked_cert") == 0)
			edd->accept_revoked_cert = flag;
		else if (strcmp(atom, "accept_corrupted_cert") == 0)
			edd->accept_corrupted_cert = flag;
		else {
			NEW_SEND_BUF(to_send);
			ei_x_encode_tuple_header(to_send, 2);
			ei_x_encode_atom(to_send, "unsupported_option");
			ei_x_encode_atom(to_send, atom);

			COPY_AND_FREE_BUF(to_send, size, b);

			break;
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
				ret = ERR_get_error();

				NEW_SEND_BUF(to_send);
				ei_x_encode_tuple_header(to_send, 2);
				ei_x_encode_long(to_send, ret);
				ei_x_encode_string(to_send,
				    ERR_error_string(ret, NULL));

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
	case COMMAND_SHUTDOWN:
		SSL_shutdown(edd->ssl);

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
	if (edd->certificate != NULL) {
		ret = SSL_CTX_use_certificate_chain_file(edd->ctx,
		    edd->certificate);
		if (ret != 1) {
			NEW_SEND_BUF(*to_send);
			ei_x_encode_atom(*to_send,
			    "load_cert_failed");

			COPY_AND_FREE_BUF(*to_send, *size, *b);

			goto err;
		}
	}

	/* Set the private key. */
	if (edd->private_key != NULL) {
		ret = SSL_CTX_use_PrivateKey_file(edd->ctx,
		    edd->private_key, SSL_FILETYPE_PEM);
		if (ret != 1) {
			NEW_SEND_BUF(*to_send);
			ei_x_encode_atom(*to_send,
			    "load_pk_failed");

			COPY_AND_FREE_BUF(*to_send, *size, *b);

			goto err;
		}
	}

	/* Prepare OpenSSL for verification. */
	verify = edd->verify_peer ?
	    SSL_VERIFY_PEER | SSL_VERIFY_CLIENT_ONCE : SSL_VERIFY_NONE;
	verify |= edd->peer_cert_required ?
	    SSL_VERIFY_FAIL_IF_NO_PEER_CERT : 0;
	SSL_CTX_set_verify(edd->ctx, verify, verify_callback);

	/* Set trusted certificates. */
	if (edd->trusted_certs != NULL) {
		ret = SSL_CTX_load_verify_locations(edd->ctx,
		    edd->trusted_certs, NULL);
		if (ret != 1) {
			NEW_SEND_BUF(*to_send);
			ei_x_encode_atom(*to_send,
			    "load_trusted_certs_failed");

			COPY_AND_FREE_BUF(*to_send, *size, *b);

			goto err;
		}
	}

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

	/* Add our own data to SSL. This will be used by verify_callback. */
	SSL_set_ex_data(edd->ssl, ssl_ex_index, edd);

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
	SSL *ssl;
	struct exmpp_tls_openssl_data *edd;
	int err, depth, gens_count, i;
	X509 *cert;
	STACK_OF(GENERAL_NAME) *gens;
	const GENERAL_NAME *gen;
	char *dnsname;

	/* Get the port driver's private data. We need it to access
	 * verification options. */
	ssl = X509_STORE_CTX_get_ex_data(x509_ctx,
	    SSL_get_ex_data_X509_STORE_CTX_idx());
	edd = SSL_get_ex_data(ssl, ssl_ex_index);

	if (!preverify_ok) {
		/* The verification done by OpenSSL failed. We check if the
		 * error is acceptable for the user. */

		/* Get the verification error code. */
		err = X509_STORE_CTX_get_error(x509_ctx);

		/* Is this kind of error is accepted? */
		switch (err) {
		case X509_V_ERR_UNABLE_TO_GET_ISSUER_CERT:
		case X509_V_ERR_UNABLE_TO_GET_CRL:
		case X509_V_ERR_CERT_SIGNATURE_FAILURE:
		case X509_V_ERR_CRL_SIGNATURE_FAILURE:
		case X509_V_ERR_DEPTH_ZERO_SELF_SIGNED_CERT:
		case X509_V_ERR_SELF_SIGNED_CERT_IN_CHAIN:
		case X509_V_ERR_UNABLE_TO_GET_ISSUER_CERT_LOCALLY:
		case X509_V_ERR_UNABLE_TO_VERIFY_LEAF_SIGNATURE:
		case X509_V_ERR_CERT_CHAIN_TOO_LONG:
		case X509_V_ERR_INVALID_CA:
		case X509_V_ERR_PATH_LENGTH_EXCEEDED:
		case X509_V_ERR_INVALID_PURPOSE:
		case X509_V_ERR_CERT_UNTRUSTED:
		case X509_V_ERR_CERT_REJECTED:
		case X509_V_ERR_SUBJECT_ISSUER_MISMATCH:
		case X509_V_ERR_AKID_SKID_MISMATCH:
		case X509_V_ERR_AKID_ISSUER_SERIAL_MISMATCH:
		case X509_V_ERR_KEYUSAGE_NO_CERTSIGN:
			/* The peer certificate isn't trusted. */
			if (!edd->accept_non_trusted_cert)
				return (0);
			break;
		case X509_V_ERR_CERT_NOT_YET_VALID:
		case X509_V_ERR_CERT_HAS_EXPIRED:
		case X509_V_ERR_CRL_NOT_YET_VALID:
		case X509_V_ERR_CRL_HAS_EXPIRED:
			/* The peer certificate or a CA certificate has
			 * expired. */
			if (!edd->accept_expired_cert)
				return (0);
			break;
		case X509_V_ERR_CERT_REVOKED:
			/* The peer certificate or a CA certificate was
			 * revoked. */
			if (!edd->accept_revoked_cert)
				return (0);
			break;
		case X509_V_ERR_UNABLE_TO_DECRYPT_CERT_SIGNATURE:
		case X509_V_ERR_UNABLE_TO_DECRYPT_CRL_SIGNATURE:
		case X509_V_ERR_UNABLE_TO_DECODE_ISSUER_PUBLIC_KEY:
		case X509_V_ERR_ERROR_IN_CERT_NOT_BEFORE_FIELD:
		case X509_V_ERR_ERROR_IN_CERT_NOT_AFTER_FIELD:
		case X509_V_ERR_ERROR_IN_CRL_LAST_UPDATE_FIELD:
		case X509_V_ERR_ERROR_IN_CRL_NEXT_UPDATE_FIELD:
			/* The verification couldn't be made because of
			 * bad data. */
			if (!edd->accept_corrupted_cert)
				return (0);
			break;
		default:
			/* Other types of error are not accepted. */
			return (0);
		}
	}

	/* Basic verification was successful. We now proceed with a more
	 * XMPP-oriented verification, only on the peer certificate and
	 * only if the caller specified an expected ID. */

	/* XXX We need an option to accept non-matching certificates. */

	/* Peer certificate is at depth 0. */
	depth = X509_STORE_CTX_get_error_depth(x509_ctx);
	if (depth > 0 || edd->expected_id == NULL)
		return (1);

	/* Get the peer certificate. */
	cert = X509_STORE_CTX_get_current_cert(x509_ctx);

	/* Get 'subjectAltName' extensions. */
	gens = X509_get_ext_d2i(cert, NID_subject_alt_name, 0, 0);
	if (gens) {
		gens_count = sk_GENERAL_NAME_num(gens);
		dnsname = NULL;

		/* XXX We must first check for the 'xmpp' extension. */

		/* We walk the extensions list to find the 'dNSName'
		 * extension. */
		for (i = 0; i < gens_count; ++i) {
			gen = sk_GENERAL_NAME_value(gens, i);
			if (gen->type != GEN_DNS)
				/* This one is not a 'dNSName' extension. */
				continue;

			/* We expect the OpenSSL library to construct
			 * GEN_DNS extesion objects as ASN1_IA5STRING
			 * values. Check we got the right union member. */
			if (ASN1_STRING_type(gen->d.ia5) != V_ASN1_IA5STRING)
				continue;

			dnsname = (char *)ASN1_STRING_data(gen->d.ia5);

			/* ASN1_IA5STRING may contain NUL character; check
			 * it. */
			if (ASN1_STRING_length(gen->d.ia5) != strlen(dnsname))
				continue;

			/* Safe to treat as an ASCII string possibly
			 * holding a DNS name */
			if (match_hostname(dnsname, edd->expected_id)) {
				sk_GENERAL_NAME_pop_free(gens,
				    GENERAL_NAME_free);
				return (1);
			}
		}

		if (dnsname != NULL) {
			/* A 'dNSName' extension was found but didn't
			 * match. The certificate is not acceptable. */
			/* XXX We must set the relevant X.509 error. */
			sk_GENERAL_NAME_pop_free(gens, GENERAL_NAME_free);
			return (0);
		}

		sk_GENERAL_NAME_pop_free(gens, GENERAL_NAME_free);
	}

	/* XXX We now fallback on the commonName extension. */

	return (0);
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

	/* This index is used to store application-specific data inside
	 * SSL structs. This index is the same for each SSL instance,
	 * that's why we keep it global.
	 *
	 * See:
	 * http://www.mail-archive.com/openssl-users@openssl.org/msg52326.html
	 */
	ssl_ex_index = SSL_get_ex_new_index(0, "exmpp_tls_openssl_data",
	    NULL, NULL, NULL);

	return &tls_openssl_driver_entry;
}
