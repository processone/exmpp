# $Id$

include Makefile.inc

ERL ?= erl

ERL_ROOT_DIR=`$(ERL) -noshell -eval 'io:format("~s~n", [[code:root_dir()]])' -s erlang halt`
ERL_INTERFACE_LIB_DIR=`$(ERL) -noshell -eval 'io:format("~s~n", [[code:lib_dir(erl_interface)]])' -s erlang halt`

CPPFLAGS += -g -Wall
CPPFLAGS += -I${ERL_ROOT_DIR}/usr/include
CPPFLAGS += -I$(ERL_INTERFACE_LIB_DIR)/include
CPPFLAGS += -DEI_ENCODE_STRING_BUG

LDFLAGS += -shared -fPIC
LDFLAGS += -lexpat
LDFLAGS += -L$(ERL_INTERFACE_LIB_DIR)/lib -lerl_interface_st -lei_st

all: portdrivers modules doc

portdrivers: priv/lib/expat_drv.so

modules: priv/lib/expat_drv.so priv/lib/stringprep_drv.so priv/lib/tls_drv.so priv/lib/iconv_erl.so
	@echo "===> Erlang modules"
	$(ERL) -make

priv/lib/expat_drv.so: c_src/expat_drv.c
	@echo "===> Expat port driver"
	$(CC) c_src/expat_drv.c -Werror $(CPPFLAGS) $(CFLAGS) $(LDFLAGS) -o $@

priv/lib/stringprep_drv.so: c_src/stringprep_drv.c c_src/uni_data.c c_src/uni_norm.c
	@echo "===> Stringprep port driver"
	$(CC) c_src/stringprep_drv.c c_src/uni_data.c c_src/uni_norm.c $(CPPFLAGS) $(CFLAGS) $(LDFLAGS) -o $@

priv/lib/tls_drv.so: c_src/tls_drv.c
	@echo "===> TLS port driver"
	$(CC) c_src/tls_drv.c $(CPPFLAGS) $(CFLAGS) $(LDFLAGS) -o $@

priv/lib/iconv_erl.so: c_src/iconv_erl.c
	@echo "===> libiconv port driver"
	$(CC) c_src/iconv_erl.c $(CPPFLAGS) $(CFLAGS) $(LDFLAGS) -o $@

doc:
	@echo "===> Documentation"
	$(ERL) -noshell -run edoc_run application "'$(APP_NAME)'" '"."' '[{def, {vsn, "$(VSN)"}}, {title, "$(DOC_TITLE)"}, {stylesheet, "stylesheet.css"}, {preprocess, true}]' -s erlang halt

clean:
	rm -f priv/lib/expat_drv.so priv/lib/stringprep_drv.so priv/lib/tls_drv.so priv/lib/iconv_erl.so
	rm -f ebin/*.beam
	rm -f doc/*.html doc/edoc-info

.PHONY: doc clean
