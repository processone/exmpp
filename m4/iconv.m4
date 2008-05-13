dnl $Id$
dnl
dnl Configure path for libiconv
dnl

dnl EXMPP_ICONV([ACTION-IF-FOUND [, ACTION-IF-NOT-FOUND]])
dnl Substitutes
dnl   ICONV_CPPFLAGS
dnl   ICONV_LDFLAGS
dnl   ICONV_LIBS
AC_DEFUN([EXMPP_ICONV],
[
  AC_ARG_WITH(iconv,
    AC_HELP_STRING([--with-iconv=PREFIX],
      [prefix where libiconv is installed (optional)]),
    iconv_prefix="$withval",)

  no_iconv=""
  ICONV_CPPFLAGS=""
  ICONV_LDFLAGS=""
  ICONV_LIBS=""

  if test x"${iconv_prefix:+set}" = "xset"; then
    ICONV_CPPFLAGS="-I${iconv_prefix%%\/}/include ${ICONV_CPPFLAGS}"
    ICONV_LDFLAGS="-L${iconv_prefix%%\/}/lib ${ICONV_LDFLAGS}"
  fi

  ac_save_CPPFLAGS="$CPPFLAGS"
  ac_save_LDFLAGS="$LDFLAGS"
  ac_save_LIBS="$LIBS"
  CPPFLAGS="$CPPFLAGS $ICONV_CPPFLAGS"
  LDFLAGS="$LDFLAGS $ICONV_LDFLAGS"

  AC_CACHE_CHECK(for iconv, am_cv_func_iconv, [
    am_cv_func_iconv="no, consider installing GNU libiconv"
    am_cv_lib_iconv=no
    AC_TRY_LINK([#include <stdlib.h>
#include <iconv.h>],
      [iconv_t cd = iconv_open("","");
       iconv(cd,NULL,NULL,NULL,NULL);
       iconv_close(cd);],
      am_cv_func_iconv=yes)
    if test "$am_cv_func_iconv" != yes; then
      ICONV_LIBS="-liconv"
      LIBS="$LIBS $ICONV_LIBS"
      AC_TRY_LINK([#include <stdlib.h>
#include <iconv.h>],
        [iconv_t cd = iconv_open("","");
         iconv(cd,NULL,NULL,NULL,NULL);
         iconv_close(cd);],
        am_cv_lib_iconv=yes
        am_cv_func_iconv=yes)
    fi
  ])

  CPPFLAGS="$ac_save_CPPFLAGS"
  LDFLAGS="$ac_save_LDFLAGS"
  LIBS="$ac_save_LIBS"

  if test "$am_cv_func_iconv" = yes; then
    AC_DEFINE(HAVE_ICONV, 1, [Define if you have the iconv() function.])
  fi
  if test "$am_cv_lib_iconv" = yes; then
    AC_MSG_CHECKING([how to link with libiconv])
    AC_MSG_RESULT([$ICONV_LIBS])

    ifelse([$1], , :, [$1])
  else
    ifelse([$2], , :, [$2])

    ICONV_CPPFLAGS=""
    ICONV_LDFLAGS=""
    ICONV_LIBS=""
  fi

  AC_SUBST(ICONV_CPPFLAGS)
  AC_SUBST(ICONV_LDFLAGS)
  AC_SUBST(ICONV_LIBS)

  if test "$am_cv_func_iconv" = yes; then
    ac_save_CPPFLAGS="$CPPFLAGS"
    ac_save_LDFLAGS="$LDFLAGS"
    ac_save_LIBS="$LIBS"
    CPPFLAGS="$CPPFLAGS $ICONV_CPPFLAGS"
    LDFLAGS="$LDFLAGS $ICONV_LDFLAGS"
    LIBS="$LIBS $ICONV_LIBS"

    AC_MSG_CHECKING([for iconv declaration])
    AC_CACHE_VAL(am_cv_proto_iconv, [
      AC_TRY_COMPILE([
#include <stdlib.h>
#include <iconv.h>
extern
#ifdef __cplusplus
"C"
#endif
#if defined(__STDC__) || defined(__cplusplus)
size_t iconv (iconv_t cd, char * *inbuf, size_t *inbytesleft, char * *outbuf, size_t *outbytesleft);
#else
size_t iconv();
#endif
], [], am_cv_proto_iconv_arg1="", am_cv_proto_iconv_arg1="const")
      am_cv_proto_iconv="extern size_t iconv (iconv_t cd, $am_cv_proto_iconv_arg1 char * *inbuf, size_t *inbytesleft, char * *outbuf, size_t *outbytesleft);"])
    am_cv_proto_iconv=`echo "[$]am_cv_proto_iconv" | tr -s ' ' | sed -e 's/( /(/'`
    AC_MSG_RESULT([$]{ac_t:-
         }[$]am_cv_proto_iconv)
    AC_DEFINE_UNQUOTED(ICONV_CONST, $am_cv_proto_iconv_arg1,
      [Define as const if the declaration of iconv() needs const.])

    CPPFLAGS="$ac_save_CPPFLAGS"
    LDFLAGS="$ac_save_LDFLAGS"
    LIBS="$ac_save_LIBS"
  fi
])
