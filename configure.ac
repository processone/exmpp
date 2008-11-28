dnl $Id$

dnl ------------------------------------------------------------------
dnl Autoconf initialisation.
dnl ------------------------------------------------------------------

AC_INIT([exmpp], [0.9.1], [js.pedron@meetic-corp.com], [exmpp])
AC_CONFIG_SRCDIR([include/exmpp.hrl])
AC_CONFIG_MACRO_DIR([m4])
AC_CONFIG_AUX_DIR([ac-aux])

AM_INIT_AUTOMAKE([foreign])

AC_CANONICAL_HOST

AC_PREREQ([2.60])
AC_REVISION([$Revision$])

dnl ------------------------------------------------------------------
dnl Internal functions for this configure script.
dnl ------------------------------------------------------------------

dnl Prepare fancy console output, taken from GNU shtools.
term_bold=''
term_norm=''

case $TERM in
xterm|xterm*|vt220|vt220*)
	dnl For the most important terminal types we directly know the
	dnl sequences.
	term_bold=`awk 'BEGIN { printf("%c%c%c%c", 27, 91, 49, 109); }' </dev/null 2>/dev/null`
	term_norm=`awk 'BEGIN { printf("%c%c%c", 27, 91, 109); }' </dev/null 2>/dev/null`
	;;
vt100|vt100*|cygwin)
	term_bold=`awk 'BEGIN { printf("%c%c%c%c%c%c", 27, 91, 49, 109, 0, 0); }' </dev/null 2>/dev/null`
	term_norm=`awk 'BEGIN { printf("%c%c%c%c%c", 27, 91, 109, 0, 0); }' </dev/null 2>/dev/null`
	;;
* )
	dnl For all others, we try to use a possibly existing `tput' or
	dnl `tcout' utility.
	paths=`echo $PATH | sed -e 's/:/ /g'`
	for tool in tput tcout; do
		for dir in $paths; do
			if test -r "$dir/$tool" ; then
				for seq in bold md smso; do
					bold="`$dir/$tool $seq 2>/dev/null`"
					if test ".$bold" != . ; then
						term_bold="$bold"
						break
					fi
				done
				if test ".$term_bold" != . ; then
					for seq in sgr0 me rmso init reset; do
						norm="`$dir/$tool $seq 2>/dev/null`"
						if test ".$norm" != . ; then
							term_norm="$norm"
							break
						fi
					done
				fi
				break
			fi
		done
		if test ".$term_bold" != . && test ".$term_norm" != . ; then
			break;
		fi
	done
	;;
esac

bold () {
        text=`echo $seo "$1" | sed -e "s/%B/${term_bold}/g" -e "s/%b/${term_norm}/g" 2>/dev/null`
        echo $seo "$text"
}

dnl EMKOPTS is used by Emakefile(s).
append_to_EMKOPTS () {
	if test -z "[$]EMKOPTS"; then
		EMKOPTS="[$]1"
	else
		EMKOPTS="[$]{EMKOPTS% }, [$]1"
	fi
}

dnl Expand shell variables to have a nice output in the final report.
expand_var () {
	local v=`eval echo '$'[$]1`
	while test "`echo [$]v | grep [[$]] > /dev/null && echo nok`"; do
		v=`eval echo [$]v`
	done
	echo [$]v
}

dnl Extract Erlang application version from its directory name.
erlang_guess_version () {
	echo "[$]1" | sed -e 's/.*-\([[0-9.]]\{1,\}\)[$]/\1/'
}

dnl Find an Erlang application without running erl(1).
erlang_check_lib_noexec () {
	local dir="not found"
	local ver="0"

	AC_MSG_CHECKING([for Erlang/OTP '[$]2' library subdirectory])
	if test -d "[$]1/lib"; then
		for d in `ls -d [$]1/lib/[$]2-* 2> /dev/null`; do
			v=`erlang_guess_version [$]d`
			AX_COMPARE_VERSION([[$]v], [gt], [[$]ver],
			    [dir=[$]d; ver=[$]v])
		done
	fi

	if test "x[$]{dir}" = "xnot found"; then
		ver="not found"
	fi

	AC_MSG_RESULT([[$]dir])
	AC_MSG_CHECKING([for Erlang/OTP '[$]2' library version])
	AC_MSG_RESULT([[$]ver])

	eval "ERLANG_LIB_DIR_[$]2='[$]dir'"
	eval "ERLANG_LIB_VER_[$]2='[$]ver'"

	AC_SUBST(ERLANG_LIB_DIR_[$]2)
	AC_SUBST(ERLANG_LIB_VER_[$]2)
}

dnl ------------------------------------------------------------------
dnl Versionning
dnl ------------------------------------------------------------------

dnl Release date.
dnl EXMPP_RELEASE_DATE=

dnl Is this a final release?
is_release=0
if test "x${EXMPP_RELEASE_DATE}" != "x"; then
	is_release=1
fi

dnl Subversion revision.
if test $is_release -eq 0; then
	EXMPP_SVNREV=`svnversion $srcdir | sed 's/^[[^:]]*://'`
	which svnversion > /dev/null; \
	if test $? -ne 0 -o "x$EXMPP_SVNREV" = "xexported"; then
		EXMPP_SVNREV=`cat $srcdir/SVNREV`
	else
		echo $EXMPP_SVNREV > $srcdir/SVNREV
	fi
	if test $EXMPP_SVNREV; then
		EXMPP_DISTSUFFIX="-r$EXMPP_SVNREV"
	fi
fi

dnl Compile environment and parameters.
EXMPP_BUILD_ARCH="$build"
EXMPP_HOST_ARCH="$host"

dnl ------------------------------------------------------------------
dnl Compilateur/outils
dnl ------------------------------------------------------------------

AC_PROG_CC_STDC

AC_PROG_LD
AC_PROG_INSTALL
AC_PROG_MAKE_SET
AC_PROG_SED

dnl ------------------------------------------------------------------
dnl libtool
dnl ------------------------------------------------------------------

dnl Hack to skip C++/Fortran tests (stolen from Beep Media Player)
m4_undefine([AC_PROG_CXX])
m4_defun([AC_PROG_CXX],[])
m4_undefine([AC_PROG_F77])
m4_defun([AC_PROG_F77],[])

AM_DISABLE_STATIC
AM_ENABLE_SHARED
AC_LIBTOOL_WIN32_DLL

AM_PROG_LIBTOOL

dnl ------------------------------------------------------------------
dnl Options.
dnl ------------------------------------------------------------------

dnl Debugging option.
if test $is_release -eq 0; then
	default_enable_debug="yes"
else
	default_enable_debug="no"
fi
AC_ARG_ENABLE([debug],
	AC_HELP_STRING([--enable-debug],
	    [turn on debugging [[default=auto]]]),,
	enable_debug=$default_enable_debug)
if test "x${enable_debug}" = "xyes"; then
	dnl AC_DEFINE([DEBUG], [], [Enable debug])
	append_to_EMKOPTS "debug_info"
	append_to_EMKOPTS "{d, debug}"
else
	CPPFLAGS="-DNDEBUG ${CPPFLAGS# }"
fi

dnl Print any warnings.
CFLAGS="${CFLAGS% } -Wall"
append_to_EMKOPTS "report_warnings"
append_to_EMKOPTS "{warn_format, 1}"
append_to_EMKOPTS "warn_export_vars"
append_to_EMKOPTS "warn_shadow_vars"
append_to_EMKOPTS "warn_unused_import"

dnl Treat warnings as errors.
AC_ARG_ENABLE([warnings],
	AC_HELP_STRING([--enable-warnings],
	    [treat warnings as errors [[default=yes]]]),,
	enable_warnings="yes")
if test "x${enable_warnings}" = "xyes" -a "x${GCC}" = "xyes"; then
	CFLAGS="${CFLAGS% } -Werror"
fi

dnl Compatibility modules.
AC_ARG_ENABLE([compat],
	AC_HELP_STRING([--enable-compat],
	    [build compatibility modules [[default=no]]]),,
	enable_compat="no")
if test "x${enable_compat}" = "xyes"; then
	COMPAT_MODULES_START=","
	COMPAT_MODULES=""
else
	COMPAT_MODULES_START=" % Compatibility modules disabled:"
	COMPAT_MODULES="%"
fi
AM_CONDITIONAL(BUILD_COMPAT, test "x${enable_compat}" = "xyes")
AC_SUBST(COMPAT_MODULES_START)
AC_SUBST(COMPAT_MODULES)

dnl Documentation.
AC_ARG_ENABLE([documentation],
	AC_HELP_STRING([--enable-documentation],
	    [build documentation [[default=yes]]]),,
	enable_documentation="yes")
AM_CONDITIONAL(ENABLE_DOCUMENTATION, test "x${enable_documentation}" = "xyes")

dnl Examples.
AC_ARG_ENABLE([examples],
	AC_HELP_STRING([--enable-examples],
	    [build examples [[default=no]]]),,
	enable_examples="no")
AM_CONDITIONAL(ENABLE_EXAMPLES, test "x${enable_examples}" = "xyes")

dnl Internal escaping function to use.
AC_ARG_ENABLE([escaping-using-cdata],
	AC_HELP_STRING([--enable-escaping-using-cdata],
	    [escape XML text node with CDATA sections [[default=off]]]),,
	enable_escaping_using_cdata="no")
if test "x${enable_escaping_using_cdata}" = "xyes"; then
	append_to_EMKOPTS "{d, 'ESCAPE_USING_CDATA_SECTIONS'}"
fi

dnl ------------------------------------------------------------------
dnl Erlang environment.
dnl ------------------------------------------------------------------

echo
bold "%BErlang environment%b"

dnl Available flags.
AC_ARG_WITH([erlang],
	AC_HELP_STRING([--with-erlang=PREFIX],
	    [prefix where build machine's Erlang is installed (optional)]),
	with_erlang=${withval%/},
	with_erlang="")

dnl Available flags (when cross-compiling only).
AC_ARG_WITH([host-erlang],
	AC_HELP_STRING([--with-host-erlang=PREFIX],
	    [prefix where host machine's Erlang is installed ]
	    [(mandatory when cross-compiling)]),
	with_host_erlang=${withval%/},
	with_host_erlang="")

dnl erl(1) is used to compile Erlang modules (regardless of the host).
if test "x${with_erlang}" = "x"; then
	AC_ERLANG_PATH_ERL
	AC_ERLANG_PATH_ERLC
else
	erl_path="${with_erlang}/bin"
	AC_ERLANG_PATH_ERL(, [$erl_path$PATH_SEPARATOR$PATH])
	AC_ERLANG_PATH_ERLC(, [$erl_path$PATH_SEPARATOR$PATH])
fi

if test "x${ERL}" = "x"; then
	AC_MSG_ERROR([
Erlang not found. Fill the ERL variable with erl(1) path or provide
Erlang prefix with --with-erlang.])
fi

dnl When cross-compiling, we need to access the host machine Erlang root
dnl directory.
if test "x${cross_compiling}" = "xyes" -a "x${with_host_erlang}" = "x"; then
	AC_MSG_ERROR([
Host machine's Erlang not found. When cross-compiling, indicate host's
Erlang path using --with-host-erlang. Build machine's and host's Erlang
versions are supposed to match.])
fi

dnl When NOT cross-compiling, the use of --with-host-erlang isn't allowed.
if test "x${cross_compiling}" = "xno" -a "x${with_host_erlang}" != "x"; then
	AC_MSG_ERROR([
If you're not cross-compiling, do not use --with-host-erlang. Please
use --with-erlang instead.])
fi

dnl escript(1) is used by the testsuite.
AC_ARG_VAR([ESCRIPT], [Erlang/OTP interpreter command [autodetected]])

if test "x${ESCRIPT}" = "x"; then
	if test "x${with_erlang}" = "x"; then
		AC_PATH_PROG([ESCRIPT], [escript],,)
	else
		erl_path="${with_erlang}/bin"
		AC_PATH_PROG([ESCRIPT], [escript],,
		    [$erl_path$PATH_SEPARATOR$PATH])
	fi
else
	AC_MSG_CHECKING([for escript])
	AC_MSG_RESULT([$ESCRIPT])
fi

if test "x${ESCRIPT}" = "x"; then
	AC_MSG_WARN([
escript(1) not found. Fill the ESCRIPT variable with escript(1) path if
you want to use the testsuite.])
fi

dnl Get Erlang $ROOT dir and lib dir.
dnl
dnl A bug in erlang.m4 in Autoconf prevents us from using Erlang macros
dnl while cross-compiling.
if test "x${cross_compiling}" = "xno"; then
	AC_ERLANG_SUBST_ROOT_DIR
	AC_ERLANG_SUBST_LIB_DIR
else
	ERLANG_ROOT_DIR=$with_host_erlang
	ERLANG_LIB_DIR="${ERLANG_ROOT_DIR}/lib"
	AC_SUBST(ERLANG_ROOT_DIR)
	AC_SUBST(ERLANG_LIB_DIR)
fi
dnl For ERTS, take the default path under Erlang ROOT directory.
ERLANG_ERTS_DIR="${ERLANG_ROOT_DIR}/usr"
AC_SUBST(ERLANG_ERTS_DIR)

dnl Check for Erlang applications.
if test "x${cross_compiling}" = "xno"; then
	dnl Check for erl_interface (used by port drivers).
	AC_ERLANG_CHECK_LIB([erl_interface],,
	    [AC_MSG_ERROR([erl_interface was not found!])])

	dnl Check for kernel (to verify if erl_ddll is broken).
	AC_ERLANG_CHECK_LIB([kernel],,)

	dnl Check for inets (to verify if http_base_64 is present).
	AC_ERLANG_CHECK_LIB([inets],,)

	dnl Check for EUnit (used for the testsuite).
	if test "x${ESCRIPT}" = "x"; then
		AC_ERLANG_CHECK_LIB([eunit],,)
	fi
else
	dnl Check for erl_interface (used by port drivers).
	erlang_check_lib_noexec "${ERLANG_ROOT_DIR}" erl_interface
	if test "x${ERLANG_LIB_DIR_erl_interface}" = "xnot found"; then
		AC_MSG_ERROR([erl_interface was not found!])
	fi

	dnl Check for kernel (to verify if erl_ddll is broken).
	erlang_check_lib_noexec "${ERLANG_ROOT_DIR}" kernel

	dnl Check for inets (to verify if http_base_64 is present).
	erlang_check_lib_noexec "${ERLANG_ROOT_DIR}" inets
fi

dnl Determine directories for installation.
if test "x${prefix}" = "xNONE"; then
	dnl Inside Erlang lib directory.
	dnl If cross-compiling, the host's Erlang lib directory is used.
	ERLANG_INSTALL_LIB_DIR="${ERLANG_LIB_DIR}"
else
	dnl Under $prefix
	ERLANG_INSTALL_LIB_DIR="${prefix}"
fi

AC_ERLANG_SUBST_INSTALL_LIB_DIR
AC_ERLANG_SUBST_INSTALL_LIB_SUBDIR(exmpp, ${VERSION})

dnl SMP support.
dnl
dnl Support is enabled by default for Erlang R12B and later ; disabled
dnl otherwise.
AC_MSG_CHECKING([for SMP support])

if test "x${ERLANG_LIB_VER_kernel}" != "xnot found"; then
	AX_COMPARE_VERSION([${ERLANG_LIB_VER_kernel}], [ge], [2.12],
	    [default_enable_smp_support="yes"],
	    [default_enable_smp_support="no"])
else
	default_enable_smp_support="no"
fi

AC_ARG_ENABLE([smp-support],
	AC_HELP_STRING([--enable-smp-support],
	    [enable SMP support [[default=auto]]]),,
	enable_smp_support=$default_enable_smp_support)
AM_CONDITIONAL(ENABLE_SMP_SUPPORT, test "x${enable_smp_support}" = "xyes")

if test "x${enable_smp_support}" = "xyes"; then
	AC_DEFINE(SMP_SUPPORT, [], [SMP support enabled in port drivers.])
	AC_DEFINE(USE_RWLOCK, [], [Use rwlocks.])
	AC_DEFINE(_REENTRANT, [], [_REENTRANT])

	if test "x${default_enable_smp_support}" = "xyes"; then
		AC_MSG_RESULT([yes (kernel >= 2.12)])
	else
		AC_MSG_RESULT([yes (forced by user)])
	fi
else
	if test "x${default_enable_smp_support}" = "xno"; then
		if test "x${ERLANG_LIB_VER_kernel}" != "xnot found"; then
			AC_MSG_RESULT([no (kernel < 2.12)])
		else
			AC_MSG_RESULT([no (kernel version unknown)])
		fi
	else
		AC_MSG_RESULT([no (disabled by user)])
	fi
fi

dnl Broken erl_ddll handling.
dnl
dnl Before kernel-2.11, port drivers refcount is not updated correctly,
dnl leading to premature unload of port drivers. If an older kernel is
dnl found, port drivers are never unloaded.
dnl
dnl The user can for the latter behaviour with --enable-erl_ddll-workaround.
AC_MSG_CHECKING([for the need for erl_ddll workaround])

if test "x${ERLANG_LIB_VER_kernel}" != "xnot found"; then
	AX_COMPARE_VERSION([${ERLANG_LIB_VER_kernel}], [lt], [2.11],
	    [default_enable_erl_ddll_workaround="yes"],
	    [default_enable_erl_ddll_workaround="no"])
else
	default_enable_erl_ddll_workaround="yes"
fi

AC_ARG_ENABLE([erl_ddll-workaround],
	AC_HELP_STRING([--enable-erl_ddll-workaround],
	    [add a workaround for broken erl_ddll [[default=auto]]]),,
	enable_erl_ddll_workaround=$default_enable_erl_ddll_workaround)
if test "x${enable_erl_ddll_workaround}" = "xyes"; then
	append_to_EMKOPTS "{d, 'ENABLE_ERL_DDLL_WORKAROUND'}"
fi

if test "x${enable_erl_ddll_workaround}" = "xyes"; then
	if test "x${default_enable_erl_ddll_workaround}" = "xyes"; then
		if test "x${ERLANG_LIB_VER_kernel}" != "xnot found"; then
			AC_MSG_RESULT([yes (kernel < 2.11)])
		else
			AC_MSG_RESULT([yes (kernel version unknown)])
		fi
	else
		AC_MSG_RESULT([yes (forced by user)])
	fi
else
	if test "x${default_enable_erl_ddll_workaround}" = "xno"; then
		AC_MSG_RESULT([no (kernel >= 2.11)])
	else
		AC_MSG_RESULT([no (disabled by user)])
	fi
fi

dnl Support for deprecated http_base_64 module.
dnl
dnl Prior to stdlib-1.14.4 (R11B-4), there was no base64 module:
dnl http_base_64 had to be used. But starting with inets-5.0 (R12B-0),
dnl http_base_64 doesn't exist anymore and the compiler will print a
dnl warning.
dnl
dnl Here, if R12B+ is used to compile exmpp, we won't use http_base_64
dnl at all by default. Else, exmpp will first try http_base_64 and
dnl fallback to base64 (at runtime).
dnl
dnl The user can force the latter behaviour with --enable-http_base_64.
AC_MSG_CHECKING([for the need to support http_base_64])

if test "x${ERLANG_LIB_VER_inets}" != "xnot found"; then
	AX_COMPARE_VERSION([${ERLANG_LIB_VER_inets}], [lt], [5.0],
	    [default_enable_http_base_64="yes"],
	    [default_enable_http_base_64="no"])
else
	default_enable_http_base_64="yes"
fi

AC_ARG_ENABLE([http_base_64],
	AC_HELP_STRING([--enable-http_base_64],
	    [use http_base_64 when base64 isn't available [[default=auto]]]),,
	enable_http_base_64=$default_enable_http_base_64)
if test "x${enable_http_base_64}" = "xyes"; then
	append_to_EMKOPTS "{d, 'ENABLE_HTTP_BASE_64'}"
fi

if test "x${enable_http_base_64}" = "xyes"; then
	if test "x${default_enable_http_base_64}" = "xyes"; then
		if test "x${ERLANG_LIB_VER_inets}" != "xnot found"; then
			AC_MSG_RESULT([yes (inets < 5.0)])
		else
			AC_MSG_RESULT([yes (inets version unknown)])
		fi
	else
		AC_MSG_RESULT([yes (forced by user)])
	fi
else
	if test "x${default_enable_http_base_64}" = "xno"; then
		AC_MSG_RESULT([no (inets >= 5.0)])
	else
		AC_MSG_RESULT([no (disabled by user)])
	fi
fi

dnl Do we have all the tools for the testsuite to be used.
if test "x${ERLANG_LIB_DIR_eunit}" != "xnot found" -a "x${ESCRIPT}" != "x"; then
	enable_testsuite="yes"
else
	enable_testsuite="no"
fi
AM_CONDITIONAL([ENABLE_TESTSUITE], [test "x${enable_testsuite}" = "xyes"])

dnl ------------------------------------------------------------------
dnl Plateform-specific cases.
dnl ------------------------------------------------------------------

case $host_os in
	*mingw*)
		dnl To compile a Win32 DLL, libtool requires -no-undefined.
		ERL_DRIVER_LDFLAGS="${ERL_DRIVER_LDFLAGS% } -no-undefined"

		canonical_host="win32"
		;;
esac

AM_CONDITIONAL([HOST_WIN32], [test "x${canonical_host}" = "xwin32"])

dnl ------------------------------------------------------------------
dnl Dependencies.
dnl ------------------------------------------------------------------

echo
bold "%BXML parsers dependencies%b"

dnl Expat port driver.
EXMPP_EXPAT(build_with_expat="yes", build_with_expat="no")
AM_CONDITIONAL(WITH_EXPAT, test "x${build_with_expat}" = "xyes")
if test "x${build_with_expat}" = "xyes"; then
	append_to_EMKOPTS "{d, 'HAVE_EXPAT'}"
fi

dnl LibXML2 port driver.
PKG_CHECK_MODULES([LIBXML2], [libxml-2.0],
    build_with_libxml2="yes", build_with_libxml2="no")
AM_CONDITIONAL(WITH_LIBXML2, test "x${build_with_libxml2}" = "xyes")
if test "x${build_with_libxml2}" = "xyes"; then
	append_to_EMKOPTS "{d, 'HAVE_LIBXML2'}"
fi

echo
bold "%BTLS engines dependencies%b"

dnl OpenSSL TLS port driver.
EXMPP_OPENSSL(build_with_openssl="yes", build_with_openssl="no")
AM_CONDITIONAL(WITH_OPENSSL, test "x${build_with_openssl}" = "xyes")
if test "x${build_with_openssl}" = "xyes"; then
	append_to_EMKOPTS "{d, 'HAVE_OPENSSL'}"
fi

echo
bold "%BCompression engines dependencies%b"

dnl Zlib compression port driver.
EXMPP_ZLIB(build_with_zlib="yes", build_with_zlib="no")
AM_CONDITIONAL(WITH_ZLIB, test "x${build_with_zlib}" = "xyes")
if test "x${build_with_zlib}" = "xyes"; then
	append_to_EMKOPTS "{d, 'HAVE_ZLIB'}"
fi

dnl ------------------------------------------------------------------
dnl Finale substitutions.
dnl ------------------------------------------------------------------

CPPFLAGS="${CPPFLAGS# }"
CFLAGS="${CFLAGS# }"
LDFLAGS="${LDFLAGS# }"

CPPFLAGS="${CPPFLAGS% }"
CFLAGS="${CFLAGS% }"
LDFLAGS="${LDFLAGS% }"

AC_SUBST(EMKOPTS)

AC_SUBST(EXMPP_RELEASE_DATE)

AC_SUBST(EXMPP_SVNREV)
AC_SUBST(EXMPP_DISTSUFFIX)

AC_SUBST(EXMPP_BUILD_ARCH)
AC_SUBST(EXMPP_HOST_ARCH)

exp_ERLANG_INSTALL_LIB_DIR_exmpp=`expand_var ERLANG_INSTALL_LIB_DIR_exmpp`
AC_SUBST(exp_ERLANG_INSTALL_LIB_DIR_exmpp)

AC_SUBST(ERL_DRIVER_LDFLAGS)

dnl ------------------------------------------------------------------
dnl Autoconf output.
dnl ------------------------------------------------------------------

echo
AM_CONFIG_HEADER([config.h])
AC_CONFIG_FILES([
	c_src/Makefile
	include/Makefile
	include/internal/Makefile
	src/Makefile
	src/Emakefile
	ebin/Makefile
	ebin/exmpp.app
	ebin/exmpp.appup
	doc/Makefile
	testsuite/Makefile
	examples/Makefile
	examples/Emakefile
	Makefile
])
AC_CONFIG_FILES([
	testsuite/etest
], [chmod +x testsuite/etest])
AC_OUTPUT

dnl --------------------------------------------------
dnl Configuration report
dnl --------------------------------------------------

echo
bold " %B== exmpp ${EXMPP_VERSION} ==%b"
echo
echo "Configuration:"
echo "    Prefix:             ${prefix}"
echo "    Application dir.:   ${exp_ERLANG_INSTALL_LIB_DIR_exmpp}"
echo
echo "    C compiler:         ${CC} ${CFLAGS}"
echo "    Erlang emulator:    ${ERL}"
echo "    Erlang compiler:    ${ERLC}"
echo "    Erlang interpreter: ${ESCRIPT}"
echo
echo "    Debug/warnings:     ${enable_debug}/${enable_warnings}"
echo "    Compat modules:     ${enable_compat}"
echo "    Documentation:      ${enable_documentation}"
echo "    Testsuite:          ${enable_testsuite}"
echo "    Examples:           ${enable_examples}"
echo
echo "    XML parsers:"
echo "      . Expat:          ${build_with_expat}"
echo "      . LibXML2:        ${build_with_libxml2}"
echo
echo "    TLS engines:"
echo "      . OpenSSL:        ${build_with_openssl}"
echo
echo "    Compression engines:"
echo "      . Zlib:           ${build_with_zlib}"
echo
