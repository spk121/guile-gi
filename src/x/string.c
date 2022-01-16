// Copyright (C) 2019, 2020, 2021, 2022 Michael L. Gran

// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.

// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <https://www.gnu.org/licenses/>.

#define _XOPEN_SOURCE 700       /* For strdup, strndup */
#include <assert.h>
#include <ctype.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "x/string.h"

size_t
strvlen(const char **x)
{
    size_t i = 0;
    while (x[i] != NULL)
        i++;
    return i;
}

/**
 * Advances the pointer @name by strlen(@strip_prefix) characters.  If
 * the resulting name does not start with a letter or underscore, the
 * @name pointer will be rewound.  This is to ensure that the
 * resulting name is a valid identifier.  Hence the returned string is
 * a pointer into the string @name.
 *
 * Returns: the stripped constant name.
 */
const char *
constant_strip_prefix(const char *name, const char *strip_prefix)
{
    size_t prefix_len, i;

    prefix_len = strlen(strip_prefix);

    /* Check so name starts with strip_prefix, if it doesn't:
     * return the rest of the part which doesn't match
     */
    for (i = 0; i < prefix_len; i++) {
        if (name[i] != strip_prefix[i] && name[i] != '_') {
            return &name[i];
        }
    }

    /* strip off prefix from value name, while keeping it a valid
     * identifier */
    for (i = prefix_len + 1; i > 0; i--) {
        if (isalpha(name[i - 1]) || name[i - 1] == '_') {
            return &name[i - 1];
        }
    }
    return name;
}

char *
gname_to_scm_name(const char *gname)
{
    assert(gname != NULL);
    assert(strlen(gname) > 0);

    size_t len = strlen(gname);
    char *str = malloc(len * 2 + 1);
    if (str == NULL) {
        fprintf(stderr, "Out of memory\n");
        exit(1);
    }
    int was_lower = 0;
    size_t j = 0;

    for (size_t i = 0; i < len; i++) {
        if (islower(gname[i])) {
            str[j++] = gname[i];
            was_lower = 1;
        }
        else if (gname[i] == '_' || gname[i] == '-') {
            str[j++] = '-';
            was_lower = 0;
        }
        else if (gname[i] == '?' || gname[i] == ':' || gname[i] == '%') {
            str[j++] = gname[i];
            was_lower = 0;
        }
        else if (isdigit(gname[i])) {
            str[j++] = gname[i];
            was_lower = 0;
        }
        else if (isupper(gname[i])) {
            if (was_lower)
                str[j++] = '-';
            str[j++] = tolower(gname[i]);
            was_lower = 0;
        }
    }
    str[j++] = '\0';
    return str;
}
