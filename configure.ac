dnl $Id$

dnl ------------------------------------------------------------------
dnl Autoconf initialisation.
dnl ------------------------------------------------------------------

AC_INIT([exmpp], [0.6.0], [js.pedron@meetic-corp.com], [exmpp])
AC_CONFIG_SRCDIR([include/exmpp.hrl])
AC_CONFIG_MACRO_DIR([m4])
AC_CONFIG_AUX_DIR([ac-aux])

configure_paramters="[$]@"

AM_INIT_AUTOMAKE([-Wall -Werror foreign])

AC_PREREQ([2.59])
AC_REVISION([$Revision$])

dnl ------------------------------------------------------------------
dnl Versionning
dnl ------------------------------------------------------------------

EXMPP_MAJOR=0
EXMPP_MINOR=6
EXMPP_MICRO=0
EXMPP_VERSION="$EXMPP_MAJOR.$EXMPP_MINOR.$EXMPP_MICRO"

dnl Compile environment and parameters.
EXMPP_BUILD_ARCH="$build"
EXMPP_HOST_ARCH="$host"
EXMPP_CONFIGURE_ARGS="$configure_parameters"

dnl ------------------------------------------------------------------
dnl Compilateur/outils
dnl ------------------------------------------------------------------

cflags_save="$CFLAGS"
AC_PROG_CC
#AC_PROG_CC_C99
CFLAGS="$cflags_save"

AC_PROG_INSTALL
AC_PROG_MAKE_SET

#AC_C_CONST
#AC_C_INLINE

#AC_C_BIGENDIAN

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

AM_PROG_LIBTOOL

dnl ------------------------------------------------------------------
dnl Headers & functions
dnl ------------------------------------------------------------------

AC_HEADER_STDC
AC_PROG_CC_STDC
AC_HEADER_MAJOR

with_zlib="yes"

AC_CHECK_HEADERS([	\
	zlib.h		\
],, with_zlib="no")

AC_CHECK_LIB(z, zlibVersion,, with_zlib="no")

dnl ------------------------------------------------------------------
dnl Options.
dnl ------------------------------------------------------------------

append_to_EMKOPTS () {
	if test -z "[$]EMKOPTS"; then
		EMKOPTS="[$]1"
	else
		EMKOPTS="[$]{EMKOPTS% }, [$]1"
	fi
}

expand_var () {
	local v=`eval echo '$'[$]1`
	while test "`echo [$]v | grep [[$]] > /dev/null && echo nok`"; do
		v=`eval echo [$]v`
	done
	echo [$]v
}

dnl Debugging option.
if test "x${CHARON_RELEASE_DATE}" = "x"; then
	enable_debug="yes"
else
	enable_debug="no"
fi
AC_ARG_ENABLE([debug],
	AC_HELP_STRING([--enable-debug],
	    [turn on debugging [[default=yes]]]),,)
if test "x${enable_debug}" = "xyes"; then
	CPPFLAGS="-DDEBUG ${CPPFLAGS# }"
	CFLAGS="-g ${CFLAGS# }"
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
	    [treat warnings as errors [[default=no]]]),,
	enable_warnings="no")
if test "x${enable_warnings}" = "xyes" -a "x${GCC}" = "xyes"; then
	CFLAGS="${CFLAGS% } -Werror"
fi

dnl Broken erl_ddll.
AC_ARG_WITH([broken-erl_ddl],
	AC_HELP_STRING([--with-broken-erl_ddll],
	    [add a workaround for broken erl_ddll [[default=yes]]]),,
	with_broken_erl_ddll="yes")
if test "x${with_broken_erl_ddll}" = "xyes"; then
	append_to_EMKOPTS "{d, 'WITH_BROKEN_ERL_DDLL'}"
fi
AM_CONDITIONAL(WITH_BROKEN_ERL_DDLL, test "x${with_broken_erl_ddll}" = "xyes")

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
AC_ARG_WITH([documentation],
	AC_HELP_STRING([--with-documentation],
	    [build documentation [[default=yes]]]),,
	with_documentation="yes")
AM_CONDITIONAL(BUILD_DOCUMENTATION, test "x${with_documentation}" = "xyes")

dnl ------------------------------------------------------------------
dnl Erlang environment.
dnl ------------------------------------------------------------------

dnl Get Erlang $ROOT dir and lib dir.
AC_ERLANG_SUBST_ROOT_DIR
AC_ERLANG_SUBST_LIB_DIR

dnl Check for standard Erlang/OTP libraries.
AC_ERLANG_CHECK_LIB([kernel], [],
	[AC_MSG_ERROR([kernel was not found!])]
)

AC_ERLANG_CHECK_LIB([stdlib], [],
	[AC_MSG_ERROR([stdlib was not found!])]
)

AC_ERLANG_CHECK_LIB([erl_interface], [],
	[AC_MSG_ERROR([erl_interface was not found!])]
)

dnl Determine directories for installation.
if test "x${prefix}" = "xNONE"; then
	# Inside Erlang $ROOT dir
	ERLANG_INSTALL_LIB_DIR="${ERLANG_LIB_DIR}"
else
	# Under $prefix
	ERLANG_INSTALL_LIB_DIR="${prefix}/lib"
fi

AC_ERLANG_SUBST_INSTALL_LIB_DIR
AC_ERLANG_SUBST_INSTALL_LIB_SUBDIR(exmpp, ${VERSION})

dnl ------------------------------------------------------------------
dnl Dependencies.
dnl ------------------------------------------------------------------

dnl Expat port driver
AM_EXPAT

dnl iconv port driver
AM_ICONV

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

AC_SUBST(EXMPP_MAJOR)
AC_SUBST(EXMPP_MINOR)
AC_SUBST(EXMPP_MICRO)
AC_SUBST(EXMPP_VERSION)
AC_SUBST(EXMPP_RELEASE_DATE)

AC_SUBST(EXMPP_BUILD_ARCH)
AC_SUBST(EXMPP_HOST_ARCH)
AC_SUBST(EXMPP_CONFIGURE_ARGS)

exp_ERLANG_INSTALL_LIB_DIR_exmpp=`expand_var ERLANG_INSTALL_LIB_DIR_exmpp`
AC_SUBST(exp_ERLANG_INSTALL_LIB_DIR_exmpp)

dnl ------------------------------------------------------------------
dnl Autoconf output.
dnl ------------------------------------------------------------------

AM_CONFIG_HEADER([config.h])
AC_CONFIG_FILES([
	c_src/Makefile
	include/Makefile
	src/Makefile
	src/Emakefile
	ebin/Makefile
	ebin/exmpp.app
	ebin/exmpp.appup
	priv/Makefile
	priv/lib/Makefile
	doc/Makefile
	testsuite/Makefile
	testsuite/Emakefile
	Makefile
])
AC_OUTPUT

dnl --------------------------------------------------
dnl Configuration report
dnl --------------------------------------------------

echo
echo " == exmpp ${EXMPP_VERSION} =="
echo
echo "Configuration:"
echo "    Prefix:             ${prefix}"
echo
echo "    Compiler flags:     ${CFLAGS}"
echo "    Debug/warnings:     ${enable_debug}/${enable_warnings}"
echo "    Compat modules:     ${enable_compat}"
echo
echo "    ERL:                ${ERL}"
echo "    Install directory:  ${exp_ERLANG_INSTALL_LIB_DIR_exmpp}"
