#ifndef TYPE_TYPE_H
#define TYPE_TYPE_H

#if 0
#include <girepository.h>

SCM gig_define_enum(GIEnumInfo *info, SCM defs);
SCM gig_define_enum_conversions(GIEnumInfo *info, GType type, SCM defs);
int gig_enum_to_int(SCM _enum);
SCM gig_int_to_enum(int value, GType type);
SCM gig_int_to_enum_with_info(int val, GIEnumInfo *info);

unsigned gig_flags_to_uint(SCM _flags);
SCM gig_uint_to_flags(unsigned value, GType type);
SCM gig_uint_to_flags_with_info(unsigned val, GIEnumInfo *info);
void gig_init_flag(void);

//SCM gig_make_signal(size_t n_slots, GigSignalSlot *slots, SCM *slot_values);
SCM gig_make_signal2(SCM name, SCM output_bitvector);
void gig_init_signal(void);

SCM gig_property_define(GType type, GIPropertyInfo *info, const char *_namespace, SCM defs);

int gig_type_check_typed_object(SCM obj, SCM expected_type);
SCM gig_type_define(GType gtype, SCM defs);
// SCM gig_type_define_full(GType gtype, SCM defs, SCM extra_supers);
typedef void *(*GigTypeRefFunction)(void *);
typedef void (*GigTypeUnrefFunction)(void *);
void
gig_type_define_fundamental(GType type, SCM extra_supers,
                            GigTypeRefFunction ref, GigTypeUnrefFunction unref);
SCM gig_type_get_scheme_type(GType gtype);
void *gig_type_peek_object(SCM obj);
void *gig_type_peek_typed_object(SCM obj, SCM expected);
SCM gig_type_transfer_object(GType gtype, void *obj, GITransfer transfer);
SCM scm_from_gtype(GType x);
GType scm_to_gtype(SCM x);
GType scm_to_gtype_full(SCM x, const char *subr, int argpos);
#else

#include "type/gig_closure.h"
#include "type/gig_constant.h"
#include "type/gig_flag.h"
#include "type/gig_object.h"
#include "type/gig_signal.h"
#include "type/gig_type.h"
#include "type/gig_type_private.h"
#include "type/gig_types.h"
#include "type/gig_value.h"
#endif

#endif
