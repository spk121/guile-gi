# Locate Guile.
# Note: `guile-config` ultimately calls pkg-config anyway
# Nothing gets marked `advanced` since there aren't that many variables

# Use PkgConfig when possible to find the version of guile which is available,
# and then use its results as hints toward the actual library/include paths

# NOTE: guile-gi copied this from the trackballs project
#       at https://github.com/trackballs/trackballs
#
# Trackballs is GPL v2


find_package(PkgConfig)
pkg_check_modules(PC_GUILE QUIET guile-3.0)
if (PC_GUILE_FOUND)
    find_program(GUILE_SNARF NAMES guile-snarf3.0 guile-snarf-3.0 guile-snarf)
    find_path(GUILE_INCLUDE_DIR libguile.h
            HINTS ${PC_GUILE_INCLUDEDIR} ${PC_GUILE_INCLUDE_DIRS}
            PATH_SUFFIXES guile/3.0 guile)
    find_library(GUILE_LIBRARY NAMES guile-3.0 guile
                HINTS ${PC_GUILE_LIBDIR} ${PC_GUILE_LIBRARY_DIRS} )
endif(PC_GUILE_FOUND)

if (NOT PC_GUILE_FOUND)
    pkg_check_modules(PC_GUILE QUIET guile-2.2)
    if (PC_GUILE_FOUND)
        find_program(GUILE_SNARF NAMES guile-snarf2.2 guile-snarf-2.2 guile-snarf)
        find_path(GUILE_INCLUDE_DIR libguile.h
                HINTS ${PC_GUILE_INCLUDEDIR} ${PC_GUILE_INCLUDE_DIRS}
                PATH_SUFFIXES guile/2.2 guile)
        find_library(GUILE_LIBRARY NAMES guile-2.2 guile
                    HINTS ${PC_GUILE_LIBDIR} ${PC_GUILE_LIBRARY_DIRS} )
    endif(PC_GUILE_FOUND)
endif(NOT PC_GUILE_FOUND)

if (NOT PC_GUILE_FOUND)
    pkg_check_modules(PC_GUILE QUIET guile-2.0)
    if (PC_GUILE_FOUND)
        find_program(GUILE_SNARF NAMES guile-snarf2.0 guile-snarf-2.0 guile-snarf)
        find_path(GUILE_INCLUDE_DIR libguile.h
                HINTS ${PC_GUILE_INCLUDEDIR} ${PC_GUILE_INCLUDE_DIRS}
                PATH_SUFFIXES guile/2.0 guile)
        find_library(GUILE_LIBRARY NAMES guile-2.0 guile
                    HINTS ${PC_GUILE_LIBDIR} ${PC_GUILE_LIBRARY_DIRS} )
    endif(PC_GUILE_FOUND)
endif(NOT PC_GUILE_FOUND)

if (NOT PC_GUILE_FOUND)
    pkg_check_modules(PC_GUILE QUIET guile)
    if (PC_GUILE_FOUND)
        find_program(GUILE_SNARF NAMES guile-snarf)
        find_path(GUILE_INCLUDE_DIR libguile.h
                HINTS ${PC_GUILE_INCLUDEDIR} ${PC_GUILE_INCLUDE_DIRS}
                PATH_SUFFIXES guile)
        find_library(GUILE_LIBRARY NAMES guile
                    HINTS ${PC_GUILE_LIBDIR} ${PC_GUILE_LIBRARY_DIRS} )
    endif(PC_GUILE_FOUND)
endif(NOT PC_GUILE_FOUND)

if (NOT PC_GUILE_FOUND)
    find_program(GUILE_SNARF NAMES
            guile-snarf3.0 guile-snarf-3.0
            guile-snarf2.2 guile-snarf-2.2
            guile-snarf2.0 guile-snarf-2.0 guile-snarf)
    find_path(GUILE_INCLUDE_DIR libguile.h
            PATH_SUFFIXES guile/3.0 guile/2.2 guile/2.0 guile)
    find_library(GUILE_LIBRARY NAMES guile-3.0 guile-2.2 guile-2.0 guile)
endif(NOT PC_GUILE_FOUND)

set(GUILE_DEFINITIONS ${PC_GUILE_CFLAGS_OTHER})

include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(Guile
        REQUIRED_VARS GUILE_LIBRARY GUILE_INCLUDE_DIR
        VERSION_VAR PC_GUILE_VERSION)

set(GUILE_LIBRARIES ${GUILE_LIBRARY} )
set(GUILE_INCLUDE_DIRS ${GUILE_INCLUDE_DIR} )

mark_as_advanced(GUILE_LIBRARY GUILE_INCLUDE_DIR GUILE_SNARF)
