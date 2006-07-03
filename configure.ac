dnl $Id$

dnl ------------------------------------------------------------------
dnl Initialisation d'Autoconf
dnl ------------------------------------------------------------------

AC_INIT([exmpp], [0.3.0], [js.pedron@meetic-corp.com], [exmpp])
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
EXMPP_MINOR=3
EXMPP_MICRO=0
EXMPP_VERSION="$EXMPP_MAJOR.$EXMPP_MINOR.$EXMPP_MICRO"

dnl On gère un build id qui permet de différencier les versions en
dnl développements (entre chaque releases) ou encore préparer une
dnl version spécifique à un projet ou un client.
AC_ARG_VAR([LIBCALIMERO_BUILD_ID], [Token specific to this build])

if test "x${LIBCALIMERO_RELEASE_DATE}" = "x" &&
    test "x${LIBCALIMERO_BUILD_ID}" = "x"; then
	LIBCALIMERO_BUILD_ID=`date '+%Y%m%d'`-${USER:-unknown}
fi

if test "x${LIBCALIMERO_BUILD_ID}" != "x"; then
	LIBCALIMERO_VERSION="${LIBCALIMERO_VERSION}-${LIBCALIMERO_BUILD_ID}"
fi

AC_SUBST(LIBCALIMERO_MAJOR)
AC_SUBST(LIBCALIMERO_MINOR)
AC_SUBST(LIBCALIMERO_MICRO)
AC_SUBST(LIBCALIMERO_VERSION)
AC_SUBST(LIBCALIMERO_RELEASE_DATE)

dnl Environnement et paramètres de compilation
LIBCALIMERO_BUILD_ARCH="$build"
LIBCALIMERO_HOST_ARCH="$host"
LIBCALIMERO_CONFIGURE_ARGS="$configure_parameters"

AC_SUBST(LIBCALIMERO_BUILD_ARCH)
AC_SUBST(LIBCALIMERO_HOST_ARCH)
AC_SUBST(LIBCALIMERO_CONFIGURE_ARGS)

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

dnl Hack pour empécher les tests C++/Fortran (source : Beep Media Player)
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

dnl ------------------------------------------------------------------
dnl Options
dnl ------------------------------------------------------------------

append_to_EMKOPTS () {
	if test -z "[$]EMKOPTS"; then
		EMKOPTS="[$]1"
	else
		EMKOPTS="[$]{EMKOPTS% }, [$]1"
	fi
}

dnl Debugging option
AC_ARG_ENABLE([debug],
	AC_HELP_STRING([--enable-debug],
	    [turn on debugging [[default=yes]]]),,
	enable_debug="yes")
if test "x${enable_debug}" = "xyes"; then
	CPPFLAGS="-DDEBUG ${CPPFLAGS# }"
	CFLAGS="-g ${CFLAGS# }"
	append_to_EMKOPTS "debug_info"
else
	CPPFLAGS="-DNDEBUG ${CPPFLAGS# }"
fi

dnl Print any warnings
CFLAGS="${CFLAGS% } -Wall"

dnl Treat warnings as errors
AC_ARG_ENABLE([warnings],
	AC_HELP_STRING([--enable-warnings],
	    [treat warnings as errors [[default=no]]]),,
	enable_warnings="no")
if test "x${enable_warnings}" = "xyes" -a "x${GCC}" = "xyes"; then
	CFLAGS="${CFLAGS% } -Werror"
fi

dnl What to do with the compatibility layer?
AC_ARG_ENABLE([compat-layer],
	AC_HELP_STRING([--enable-compat-layer],
	    [enable compatility layer [[default=yes]]]),,
	enable_compat_layer="yes")
if test "x${enable_compat_layer}" = "xyes"; then
	append_to_EMKOPTS "{d, 'WITH_DEPRECATED_API'}"
fi

dnl ------------------------------------------------------------------
dnl Environnement Erlang
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

#AC_ERLANG_CHECK_LIB([sasl], [],
#	[AC_MSG_ERROR([sasl was not found!])]
#)

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
dnl Dependencies
dnl ------------------------------------------------------------------

dnl Expat port driver
AM_EXPAT

dnl iconv port driver
AM_ICONV

dnl ------------------------------------------------------------------
dnl Substitutions finales
dnl ------------------------------------------------------------------

CPPFLAGS="${CPPFLAGS# }"
CFLAGS="${CFLAGS# }"
LDFLAGS="${LDFLAGS# }"

CPPFLAGS="${CPPFLAGS% }"
CFLAGS="${CFLAGS% }"
LDFLAGS="${LDFLAGS% }"

AC_SUBST(EMKOPTS)

dnl ------------------------------------------------------------------
dnl Sortie d'Autoconf
dnl ------------------------------------------------------------------

AM_CONFIG_HEADER([config.h])
AC_CONFIG_FILES([
	c_src/Makefile
	include/Makefile
	src/Makefile
	src/Emakefile
	ebin/Makefile
	ebin/exmpp.app
	priv/Makefile
	priv/lib/Makefile
	testsuite/Makefile
	testsuite/Emakefile
	Makefile
])
AC_OUTPUT

dnl --------------------------------------------------
dnl Configuration report
dnl --------------------------------------------------

expand_var () {
	local v=`eval echo '$'[$]1`
	while test "`echo [$]v | grep [[$]] > /dev/null && echo nok`"; do
		v=`eval echo [$]v`
	done
	echo [$]v
}

exp_ERLANG_INSTALL_LIB_DIR=`expand_var ERLANG_INSTALL_LIB_DIR`
exp_ERLANG_INSTALL_LIB_DIR_exmpp=`expand_var ERLANG_INSTALL_LIB_DIR_exmpp`

echo
echo " == exmpp ${EXMPP_VERSION} =="
echo
echo "Configuration:"
echo "    Prefix:             ${prefix}"
echo
echo "    Compiler flags:     ${CFLAGS}"
echo "    Debug/warnings:     ${enable_debug}/${enable_warnings}"
echo
echo "    ERL:                ${ERL}"
echo "    Install directory:  ${exp_ERLANG_INSTALL_LIB_DIR_exmpp}"
