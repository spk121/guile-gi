resourcesdir = $(datadir)/doc/guile-gi/resources
dist_resources_DATA = \
 main.scm \
 builder.ui \
 test.gresource.xml

nodist_resources_DATA = \
 test.gresource

test.gresource: test.gresource.xml
	$(GLIB_COMPILE_RESOURCES) --target=$@ --sourcedir=$(srcdir) $<

CLEANFILES := test.gresource
