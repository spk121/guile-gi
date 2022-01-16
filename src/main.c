#include <libguile.h>
#include "y.h"

int
main(int argc, char **argv)
{
    scm_init_guile();

    init_repository();
    scm_shell(argc, argv);
    return 0;
}
