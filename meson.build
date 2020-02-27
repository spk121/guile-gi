project('guile-gi', 'c',
        license: 'gpl3+',
        default_options: ['debug=true',
                          'optimization=2',
                          'warning_level=2'])

cc = meson.get_compiler('c')

_cargs = ['-D_FORTIFY_SOURCE=2',
          '-DSCM_DEBUG_TYPING_STRICTNESS=2',
          '-grecord-gcc-switches',
          # pedantic complains about totally legit (implicit) casts
          '-Wno-pedantic',
          '-Werror=format-security',
          '-Werror=implicit-function-declaration',
          '-Werror=shadow',
          '-fno-omit-frame-pointer',
          '-fasynchronous-unwind-tables',
          '-fexceptions',
          '-fvar-tracking',
          '-fstack-protector-strong',
          '-fstack-clash-protection',
          '-fcf-protection',
          '-fplugin=annobin',
          '-Wsuggest-attribute=format',
          '-Wsuggest-attribute=malloc',
          '-Wsuggest-attribute=noreturn',
          '-Wsuggest-attribute=pure']
_ldargs = ['-Wl,--dynamic-list-data',
           '-Wl,-z,defs',
           '-Wl,-z,now',
           '-Wl,-z,relro']

cargs = []
ldargs = []

foreach arg: _cargs
  if cc.has_argument(arg)
    cargs += arg
  endif
endforeach

foreach arg: _ldargs
  if cc.has_link_argument(arg)
    ldargs += arg
  endif
endforeach

add_project_arguments(cargs, language: 'c')
add_project_link_arguments(ldargs, language: 'c')

glib_dep = dependency('glib-2.0', version: '>=2.48')
gobject_dep = dependency('gobject-2.0')
gobject_introspection_dep = dependency('gobject-introspection-1.0')
libffi_dep = dependency('libffi')

guile3_dep = dependency('guile-3.0', required: false)
guile22_dep = dependency('guile-2.2', required: false)

if guile3_dep.found()
  guile_dep = guile3_dep
  guile_version = '3.0'
else
  if guile22_dep.found()
    guile_dep = guile22_dep
    guile_version = '2.2'
  else
    error('No suitable guile found.')
  endif
endif

if get_option('use_guile_site')
  extensiondir = guile_dep.get_pkgconfig_variable('extensiondir')
else
  extensiondir = get_option('prefix') / get_option('libdir') / 'guile' / guile_version / 'extensions'
endif

shared_library('guile-gi',
               'src/gig_argument.c',
               'src/gig_closure.c',
               'src/gig_data_type.c',
               'src/gig_object.c',
               'src/gig_value.c',
               'src/gig_signal.c',
               'src/gig.c',
               'src/gig_arg_map.c',
               'src/gig_callback.c',
               'src/gig_function.c',
               'src/gig_constant.c',
               'src/gig_flag.c',
               'src/gig_repository.c',
               'src/gig_document.c',
               'src/gig_type.c',
               'src/gig_type_private.c',
               'src/gig_util.c',
               'src/gig_logging.c',
               dependencies: [glib_dep, gobject_dep, gobject_introspection_dep,
                              guile_dep, libffi_dep],
               install: true,
               install_dir: extensiondir)
