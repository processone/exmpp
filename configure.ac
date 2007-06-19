dnl $Id$

dnl ------------------------------------------------------------------
dnl Autoconf initialisation.
dnl ------------------------------------------------------------------

AC_INIT([exmpp], [0.7.0], [js.pedron@meetic-corp.com], [exmpp])
AC_CONFIG_SRCDIR([include/exmpp.hrl])
AC_CONFIG_MACRO_DIR([m4])
AC_CONFIG_AUX_DIR([ac-aux])

configure_paramters="[$]@"

AM_INIT_AUTOMAKE([-Wall -Werror foreign])

AC_CANONICAL_HOST

AC_PREREQ([2.60])
AC_REVISION([$Revision$])

dnl ------------------------------------------------------------------
dnl Versionning
dnl ------------------------------------------------------------------

EXMPP_MAJOR=0
EXMPP_MINOR=7
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
CFLAGS="$cflags_save"

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
dnl Headers & functions
dnl ------------------------------------------------------------------

AC_HEADER_STDC
AC_PROG_CC_STDC
AC_HEADER_MAJOR

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
if test "x${EXMPP_RELEASE_DATE}" = "x"; then
	default_enable_debug="yes"
else
	default_enable_debug="no"
fi
AC_ARG_ENABLE([debug],
	AC_HELP_STRING([--enable-debug],
	    [turn on debugging [[default=auto]]]),,
	enable_debug=$default_enable_debug)
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
AC_ARG_WITH([documentation],
	AC_HELP_STRING([--with-documentation],
	    [build documentation [[default=no]]]),,
	with_documentation="no")
AM_CONDITIONAL(BUILD_DOCUMENTATION, test "x${with_documentation}" = "xyes")

dnl ------------------------------------------------------------------
dnl Erlang environment.
dnl ------------------------------------------------------------------

dnl If we're cross-compiling, no Erlang checks will be ran.

AC_ARG_WITH([erlang],
	AC_HELP_STRING([--with-erlang],
	    [prefix where Erlang is installed]),,
	with_erlang="")
AC_ARG_WITH([erl_interface],
	AC_HELP_STRING([--with-erl_interface],
	    [prefix where erl_interface is installed]),,
	with_erl_interface="")

if test "x${cross_compiling}" = "xno"; then
	dnl Get Erlang $ROOT dir and lib dir.
	AC_ERLANG_SUBST_ROOT_DIR
	AC_ERLANG_SUBST_LIB_DIR

	dnl Check for standard Erlang/OTP libraries.
	AC_ERLANG_CHECK_LIB([kernel], [],
		[AC_MSG_ERROR([kernel was not found!])]
	)
	AC_ERLANG_CHECK_LIB([erl_interface], [],
		[AC_MSG_ERROR([erl_interface was not found!])]
	)
else
	if test "x${with_erlang}" = "x"; then
		AC_MSG_ERROR([Erlang not found. Prefix must be provided with --with-erlang])
	fi
	if test "x${with_erl_interface}" = "x"; then
		AC_MSG_ERROR([erl_interface not found. Prefix must be provided with --with-erl_interface])
	fi

	dnl We use the native Erlang binary to compile Erlang sources.
	AC_ERLANG_PATH_ERL

	ERLANG_ROOT_DIR="${with_erlang}"
	ERLANG_LIB_DIR="${ERLANG_ROOT_DIR}/lib"
	ERLANG_LIB_DIR_erl_interface="${with_erl_interface}"
fi

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

dnl Broken erl_ddll.
dnl Before kernel-2.11, port drivers refcount is not updated correctly,
dnl leading to premature unload of the library.
AC_MSG_CHECKING([for the need for erl_ddll workaround])

if test "x${cross_compiling}" = "xno"; then
	AX_COMPARE_VERSION(${ERLANG_LIB_VER_kernel}, [lt], [2.11],
	    [default_with_broken_erl_ddll="yes"],
	    [default_with_broken_erl_ddll="no"])
else
	default_with_broken_erl_ddll="yes"
fi

AC_ARG_WITH([broken-erl_ddll],
	AC_HELP_STRING([--with-broken-erl_ddll],
	    [add a workaround for broken erl_ddll [[default=auto]]]),,
	with_broken_erl_ddll=$default_with_broken_erl_ddll)
if test "x${with_broken_erl_ddll}" = "xyes"; then
	append_to_EMKOPTS "{d, 'WITH_BROKEN_ERL_DDLL'}"
fi
AM_CONDITIONAL(WITH_BROKEN_ERL_DDLL, test "x${with_broken_erl_ddll}" = "xyes")

if test "x${with_broken_erl_ddll}" = "xyes"; then
	if test "x${default_with_broken_erl_ddll}" = "xyes"; then
		if test "x${cross_compiling}" = "xno"; then
			AC_MSG_RESULT([yes (kernel < 2.11)])
		else
			AC_MSG_RESULT([yes (cross-compiling)])
		fi
	else
		AC_MSG_RESULT([yes (forced by user)])
	fi
else
	if test "x${default_with_broken_erl_ddll}" = "xno"; then
		AC_MSG_RESULT([no (kernel >= 2.11)])
	else
		AC_MSG_RESULT([no (disabled by user)])
	fi
fi

dnl ------------------------------------------------------------------
dnl Plateform-specific cases.
dnl ------------------------------------------------------------------

case $host_os in
	*mingw*)
		# Pour compiler une DLL Win32, libtool a besoin
		# de -no-undefined.
		ERL_DRIVER_LDFLAGS="${ERL_DRIVER_LDFLAGS% } -no-undefined"

		canonical_host="win32"
		;;
esac

AM_CONDITIONAL([HOST_WIN32], [test "x${canonical_host}" = "xwin32"])

dnl ------------------------------------------------------------------
dnl Dependencies.
dnl ------------------------------------------------------------------

dnl Expat port driver
EXMPP_EXPAT(build_with_expat="yes", build_with_expat="no")
AM_CONDITIONAL(WITH_EXPAT, test "x${build_with_expat}" = "xyes")

build_with_zlib="no"
build_with_iconv="no"
build_with_openssl="no"

if test "x${enable_compat}" = "xyes"; then
	dnl Zlib
	EXMPP_ZLIB(build_with_zlib="yes", build_with_zlib="no")

	dnl iconv port driver
	EXMPP_ICONV(build_with_iconv="yes", build_with_iconv="no")

	dnl OpenSSL
	EXMPP_OPENSSL(build_with_openssl="yes", build_with_openssl="no")
fi

AM_CONDITIONAL(WITH_ZLIB, test "x${build_with_zlib}" = "xyes")
AM_CONDITIONAL(WITH_ICONV, test "x${build_with_iconv}" = "xyes")
AM_CONDITIONAL(WITH_OPENSSL, test "x${build_with_openssl}" = "xyes")

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

AC_SUBST(ERL_DRIVER_LDFLAGS)

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
