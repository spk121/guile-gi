/*
  visibility.h

  Copyright 2022 Michael L. Gran

  This file is part of GNU Guile-GI

  Guile-GI is free software: you can redistribute it and/or modify
  it under the terms of the GNU Lesser General Public License as
  published by the Free Software Foundation, either version 3 of the
  License, or (at your option) any later version.

  Guile-GI is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  Lesser General Public License for more details.

  You should have received a copy of the GNU General Public
  License along with Guile-GI.  If not, see
  <https://www.gnu.org/licenses/>.
*/

#ifndef CORE_VISIBILITY_H
#define CORE_VISIBILITY_H

/*
 * Cross-platform symbol visibility macros
 * 
 * Usage:
 *   GIG_API         - Public API functions (exported from DLL/shared library)
 *   GIG_LOCAL       - Internal functions (hidden from public API)
 *   GIG_DEPRECATED  - Mark functions as deprecated
 *   GIG_WEAK        - Weak symbol linkage (where supported)
 * 
 * Build configuration:
 *   BUILDING_GIG    - Define when building the library itself
 *   GIG_STATIC      - Define for static library builds
 *   GIG_DLL         - Define for Windows DLL builds
 */

/* Platform and compiler detection */
#if defined(_WIN32) || defined(__CYGWIN__)
  #define GIG_PLATFORM_WINDOWS 1
#elif defined(__GNUC__) && __GNUC__ >= 4
  #define GIG_PLATFORM_UNIX_GCC 1
#elif defined(__clang__)
  #define GIG_PLATFORM_UNIX_CLANG 1
#elif defined(__SUNPRO_C) || defined(__SUNPRO_CC)
  #define GIG_PLATFORM_SOLARIS 1
#else
  #define GIG_PLATFORM_GENERIC 1
#endif

/* Windows (including MinGW, MSVC, Clang-cl) */
#ifdef GIG_PLATFORM_WINDOWS
  #ifdef GIG_STATIC
    /* Static library - no special handling needed */
    #define GIG_API
    #define GIG_LOCAL
  #else
    /* Dynamic library (DLL) */
    #ifdef BUILDING_GIG
      /* Building the library - export symbols */
      #if defined(__GNUC__) || defined(__clang__)
        /* MinGW/Clang with GCC-style attributes */
        #define GIG_API __attribute__((dllexport))
      #else
        /* MSVC-style */
        #define GIG_API __declspec(dllexport)
      #endif
    #else
      /* Using the library - import symbols */
      #if defined(__GNUC__) || defined(__clang__)
        /* MinGW/Clang with GCC-style attributes */
        #define GIG_API __attribute__((dllimport))
      #else
        /* MSVC-style */
        #define GIG_API __declspec(dllimport)
      #endif
    #endif
    #define GIG_LOCAL
  #endif

/* Unix-like systems with GCC visibility support */
#elif defined(GIG_PLATFORM_UNIX_GCC) || defined(GIG_PLATFORM_UNIX_CLANG)
  #ifdef GIG_STATIC
    /* Static library - no special handling needed */
    #define GIG_API
    #define GIG_LOCAL
  #else
    /* Shared library with visibility attributes */
    #define GIG_API __attribute__((visibility("default")))
    #define GIG_LOCAL __attribute__((visibility("hidden")))
  #endif

/* Solaris with Sun Studio compiler */
#elif defined(GIG_PLATFORM_SOLARIS)
  #ifdef GIG_STATIC
    #define GIG_API
    #define GIG_LOCAL
  #else
    #define GIG_API __global
    #define GIG_LOCAL __hidden
  #endif

/* Generic fallback for unknown platforms */
#else
  #define GIG_API
  #define GIG_LOCAL
#endif

/* Additional utility macros */

/* Deprecation warnings */
#if defined(__GNUC__) || defined(__clang__)
  #define GIG_DEPRECATED __attribute__((deprecated))
  #define GIG_DEPRECATED_MSG(msg) __attribute__((deprecated(msg)))
#elif defined(_MSC_VER)
  #define GIG_DEPRECATED __declspec(deprecated)
  #define GIG_DEPRECATED_MSG(msg) __declspec(deprecated(msg))
#else
  #define GIG_DEPRECATED
  #define GIG_DEPRECATED_MSG(msg)
#endif

/* Weak symbol linkage (for optional dependencies) */
#if defined(__GNUC__) || defined(__clang__)
  #define GIG_WEAK __attribute__((weak))
#else
  #define GIG_WEAK
#endif

/* Function attributes for better optimization */
#if defined(__GNUC__) || defined(__clang__)
  #define GIG_PURE __attribute__((pure))
  #define GIG_CONST __attribute__((const))
  #define GIG_MALLOC __attribute__((malloc))
  #define GIG_NORETURN __attribute__((noreturn))
  #define GIG_PRINTF(fmt_idx, arg_idx) __attribute__((format(printf, fmt_idx, arg_idx)))
  #define GIG_UNUSED __attribute__((unused))
  #define GIG_LIKELY(x) __builtin_expect(!!(x), 1)
  #define GIG_UNLIKELY(x) __builtin_expect(!!(x), 0)
#elif defined(_MSC_VER)
  #define GIG_PURE
  #define GIG_CONST
  #define GIG_MALLOC __declspec(restrict)
  #define GIG_NORETURN __declspec(noreturn)
  #define GIG_PRINTF(fmt_idx, arg_idx)
  #define GIG_UNUSED
  #define GIG_LIKELY(x) (x)
  #define GIG_UNLIKELY(x) (x)
#else
  #define GIG_PURE
  #define GIG_CONST
  #define GIG_MALLOC
  #define GIG_NORETURN
  #define GIG_PRINTF(fmt_idx, arg_idx)
  #define GIG_UNUSED
  #define GIG_LIKELY(x) (x)
  #define GIG_UNLIKELY(x) (x)
#endif

/* C++ linkage handling */
#ifdef __cplusplus
  #define GIG_BEGIN_DECLS extern "C" {
  #define GIG_END_DECLS }
#else
  #define GIG_BEGIN_DECLS
  #define GIG_END_DECLS
#endif

/* Inline function handling */
#if defined(__STDC_VERSION__) && __STDC_VERSION__ >= 199901L
  /* C99 and later */
  #define GIG_INLINE inline
#elif defined(__GNUC__) || defined(__clang__)
  #define GIG_INLINE __inline__
#elif defined(_MSC_VER)
  #define GIG_INLINE __inline
#else
  #define GIG_INLINE
#endif

/* Static inline for header-only functions */
#define GIG_STATIC_INLINE static GIG_INLINE

/* Memory alignment macros */
#if defined(__STDC_VERSION__) && __STDC_VERSION__ >= 201112L
  /* C11 and later */
  #include <stdalign.h>
  #define GIG_ALIGNAS(x) _Alignas(x)
  #define GIG_ALIGNOF(x) _Alignof(x)
#elif defined(__GNUC__) || defined(__clang__)
  #define GIG_ALIGNAS(x) __attribute__((aligned(x)))
  #define GIG_ALIGNOF(x) __alignof__(x)
#elif defined(_MSC_VER)
  #define GIG_ALIGNAS(x) __declspec(align(x))
  #define GIG_ALIGNOF(x) __alignof(x)
#else
  #define GIG_ALIGNAS(x)
  #define GIG_ALIGNOF(x) sizeof(x)
#endif

/* Thread-local storage */
#if defined(__STDC_VERSION__) && __STDC_VERSION__ >= 201112L
  /* C11 and later */
  #define GIG_THREAD_LOCAL _Thread_local
#elif defined(__GNUC__) || defined(__clang__)
  #define GIG_THREAD_LOCAL __thread
#elif defined(_MSC_VER)
  #define GIG_THREAD_LOCAL __declspec(thread)
#else
  #define GIG_THREAD_LOCAL
#endif

/* Unreachable code hints for optimization */
#if defined(__GNUC__) && __GNUC__ >= 4 && __GNUC_MINOR__ >= 5
  #define GIG_UNREACHABLE() __builtin_unreachable()
#elif defined(__clang__)
  #define GIG_UNREACHABLE() __builtin_unreachable()
#elif defined(_MSC_VER)
  #define GIG_UNREACHABLE() __assume(0)
#else
  #define GIG_UNREACHABLE() do { } while(0)
#endif

/* Prefetch hints for performance */
#if defined(__GNUC__) || defined(__clang__)
  #define GIG_PREFETCH_READ(addr) __builtin_prefetch((addr), 0, 3)
  #define GIG_PREFETCH_WRITE(addr) __builtin_prefetch((addr), 1, 3)
#else
  #define GIG_PREFETCH_READ(addr) do { } while(0)
  #define GIG_PREFETCH_WRITE(addr) do { } while(0)
#endif

/* Branch prediction hints */
#if defined(__GNUC__) || defined(__clang__)
  #define GIG_PREDICT_TRUE(x) __builtin_expect(!!(x), 1)
  #define GIG_PREDICT_FALSE(x) __builtin_expect(!!(x), 0)
#else
  #define GIG_PREDICT_TRUE(x) (x)
  #define GIG_PREDICT_FALSE(x) (x)
#endif

/* Compiler version checking macros */
#define GIG_GNUC_CHECK_VERSION(major, minor) \
  (defined(__GNUC__) && \
   ((__GNUC__ > (major)) || \
    (__GNUC__ == (major) && __GNUC_MINOR__ >= (minor))))

#define GIG_CLANG_CHECK_VERSION(major, minor) \
  (defined(__clang__) && \
   ((__clang_major__ > (major)) || \
    (__clang_major__ == (major) && __clang_minor__ >= (minor))))

#define GIG_MSVC_CHECK_VERSION(version) \
  (defined(_MSC_VER) && _MSC_VER >= (version))

/* Common attribute combinations */
#define GIG_PUBLIC_API GIG_API
#define GIG_INTERNAL GIG_LOCAL
#define GIG_DEPRECATED_API GIG_API GIG_DEPRECATED
#define GIG_EXPERIMENTAL_API GIG_API

/* Version information embedding */
#ifdef BUILDING_GIG
  #define GIG_EXPORT_VERSION(major, minor, patch) \
    GIG_API const char gig_version_string[] = #major "." #minor "." #patch; \
    GIG_API const int gig_version_major = major; \
    GIG_API const int gig_version_minor = minor; \
    GIG_API const int gig_version_patch = patch;
#else
  #define GIG_EXPORT_VERSION(major, minor, patch)
#endif

#endif /* CORE_VISIBILITY_H */
