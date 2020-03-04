project('guile-gi', 'c',
        license: 'gpl3+',
        version: '0.2.2',
        default_options: ['debug=true',
                          'optimization=2',
                          'warning_level=2'])

top_source = meson.current_source_dir()
top_build = meson.current_build_dir()
gnome = import('gnome')

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
  sitedir = guile_dep.get_pkgconfig_variable('sitedir')
else
  extensiondir = get_option('prefix') / get_option('libdir') / 'guile' / guile_version / 'extensions'
  sitedir = get_option('prefix') / get_option('datadir') / 'guile' / 'site' / guile_version
endif
gitestdir = gobject_introspection_dep.get_pkgconfig_variable('gidatadir') / 'tests'

conf_data = configuration_data({'abs_top_builddir': top_build,
                                'abs_top_srcdir': top_source,
                                'GI_GIR_DIR': gobject_introspection_dep.get_pkgconfig_variable('girdir'),
                                'GUILE': guile_dep.get_pkgconfig_variable('guile'),
                                'SHELL': '/bin/sh',
                               })

configure_file(input: 'module' / 'gi' / 'config.scm.in',
               output: 'config.scm',
               configuration: conf_data)
foreach tool: ['gdb-guile', 'gdb-test', 'lcov.sh', 'run-guile',
               'run-test', 'uninstalled-env', 'uninstalled-test-env',
               'valgrind.sh']
  configure_file(input: 'tools' / tool + '.in',
                 output: tool,
                 configuration: conf_data)
endforeach
configure_file(output: 'config.h',
               configuration: {'VERSION': meson.project_version()})
configure_file(input: gitestdir / 'gitestmacros.h',
               output: 'gitestmacros.h',
               copy: true)

meson.add_postconf_script('meson_postconf.sh')

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

gi_modules = ['module/gi/config.scm',
              'module/gi/core-generics.scm',
              'module/gi/documentation.scm',
              'module/gi/logging.scm',
              'module/gi/oop.scm',
              'module/gi/repository.scm',
              'module/gi/types.scm',
              'module/gi/util.scm']
script_modules = ['module/scripts/gi-gtkdoc.scm']
top_modules = ['module/gi.scm']

install_data(gi_modules, install_dir: sitedir / 'gi')
install_data(script_modules, install_dir: sitedir / 'scripts')
install_data(top_modules, install_dir: sitedir)

test_deps = [glib_dep, gobject_dep]
test_cargs = ['-Wno-unused-parameter', '-Wno-suggest-attribute=malloc']
marshall_sed = [find_program('sed'),
                '-e', 's/gimarshallingtests/marshall/',
                '-e', 's/gi_marshalling_tests/marshall/g',
                '-e', 's/GI_MARSHALLING_TESTS/MARSHALL/g',
                '-e', 's/GIMarshallingTests/Marshall/g',
                '@INPUT@']

marshall_sources = [custom_target('marshall.c',
                                  input: gitestdir / 'gimarshallingtests.c',
                                  output: 'marshall.c',
                                  capture: true,
                                  command: marshall_sed),
                    custom_target('marshall.h',
                                  input: gitestdir / 'gimarshallingtests.h',
                                  output: 'marshall.h',
                                  capture: true,
                                  command: marshall_sed)]
everything_sources = [gitestdir / 'everything.c', gitestdir / 'everything.h']
extra_sources = ['test' / 'extra.c', 'test' / 'extra.h']

libmarshall = shared_library('marshall', marshall_sources,
                             dependencies: test_deps, c_args: test_cargs)
libeverything = shared_library('everything', everything_sources,
                               dependencies: test_deps, c_args: test_cargs)
libextra = shared_library('extra', extra_sources,
                          dependencies: test_deps, c_args: test_cargs)

guile_bin = find_program(guile_dep.get_pkgconfig_variable('guile'))

test_args = ['--no-auto-compile', top_source / 'tools' / 'test-runner.scm']

test_env = environment()
test_env.prepend('LD_LIBRARY_PATH', top_build)
test_env.prepend('GI_TYPELIB_PATH', top_build)
test_env.prepend('GUILE_LOAD_PATH', top_source / 'module')

tests = ['typelib', 'constants', 'mem', 'io', 'file', 'string',
         'bash-the-hash', 'datetime', 'byte-array', 'main-loop',
         'application', 'gtk', 'grilo', 'oop', 'value', 'flags']
foreach t: tests
  test(t, guile_bin,
       args: test_args + [top_source / 'test' / t + '.scm'],
       env: test_env,
       workdir: top_build / 'test')
endforeach

foreach t: [['marshall', 'Marshall', libmarshall, marshall_sources],
            ['everything', 'Everything', libeverything, everything_sources],
            ['extra', 'Extra', libextra, extra_sources]]
  test(t[0], guile_bin,
      args: test_args +  [top_source / 'test' / t[0] + '.scm'],
      env: test_env,
      depends: gnome.generate_gir(t[2],
                                  sources: t[3],
                                  namespace: t[1],
                                  nsversion: '1.0',
                                  identifier_prefix: t[1],
                                  symbol_prefix: t[0],
                                  includes: ['GObject-2.0'],
                                  dependencies: [gobject_dep]),
      workdir: top_build / 'test')
endforeach
