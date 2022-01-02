#ifndef GI_LOGGING_H
#define GI_LOGGING_H

#include <stdio.h>

#define assert_not_reached()                    \
    do {                                                                \
        fprintf(stderr, "Reached invalid code at %s:%d\n", __FILE__, __LINE__); \
        exit(1);                                                        \
    } while (0)

#endif
