#ifndef GIR_TYPE_H
#define GIR_TYPE_H

#define SLOT_COUNT 8
#define OB_TYPE_SLOT 0
#define OB_REFCNT_SLOT 1
#define OBJ_SLOT 2
#define DEALLOC_SLOT 3
#define FREE_FUNC_SLOT 4
#define INST_DICT_SLOT 5
#define WEAKREFLIST_SLOT 6
#define FLAGS_SLOT 7

void gir_init_types(void);

#endif
