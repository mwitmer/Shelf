#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_INIT([Shelf], [1.0], [mark.d.witmer@gmail.com])

AC_CONFIG_AUX_DIR([build_aux])
AM_INIT_AUTOMAKE([gnu std-options -Wall])

AC_PREREQ([2.68])
AC_CONFIG_MACRO_DIR([m4])

LIBGUILE_SHELF_INTERFACE="1:0:0"
AC_SUBST(LIBGUILE_SHELF_INTERFACE)

# Checks for libraries.

AC_ARG_WITH([guilesitedir],
            [AS_HELP_STRING([--with-guilesitedir],
                            [use the specified installation path for Guile modules])],
            [case "x$withval" in
             xyes|xno) guilesitedir="";;
             *)        guilesitedir="$withval";;
             esac],
             [guilesitedir=""])

LT_INIT([dlopen disable-fast-install disable-static win32-dll])	

GUILE_PROGS
GUILE_FLAGS
GUILE_SITE_DIR

pkgdatadir="$datadir/$PACKAGE"
if test "x$guilesitedir" = "x"; then
   guilesitedir="$pkgdatadir"
fi
AC_SUBST([guilesitedir])

AC_CONFIG_FILES([
	Makefile
	shelf/Makefile
])

AC_OUTPUT

if test "$guilesitedir" != "$GUILE_SITE"; then
   # Guile has a different prefix than this module
   AC_MSG_WARN([]
   [The Guile modules will be installed in ${guilesitedir}.]
   [You should probably re-run `configure' with]
   [`--with-guilesitedir=$GUILE_SITE']
   [Otherwise, you will have to adjust the `GUILE_LOAD_PATH' environment]
   [variable.])
fi
