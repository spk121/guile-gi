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

void gir_type_register(GType gtype);
void gir_type_define(GType gtype, GIBaseInfo *info);
GType gir_type_get_gtype_from_obj(SCM x);
SCM  gir_type_make_object(GType gtype, gpointer obj, GITransfer transfer);
void gir_init_types(void);

#endif
