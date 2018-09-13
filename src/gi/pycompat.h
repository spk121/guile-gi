/* Python compatibility functions. */
#pragma once
#include <libguile.h>
#include <glib.h>

G_BEGIN_DECLS

typedef ssize_t Gu_ssize_t;

extern SCM Gu_None;
extern SCM GuExc_TypeError;
extern SCM GuExc_ImportError;

SCM Gu_BuildValue (const char *format, ...);
void Gu_DECREF(SCM x);
void Gu_XDECREF(SCM x);

int GuArg_ParseTuple(SCM args, const char *format, ...);
int GuArg_VaParse(SCM args, const char *format, va_list vargs);

int GuCapsule_CheckExact (SCM x);
void *GuCapsule_GetPointer (SCM x, const char *name);

int GuDict_Next(SCM hash_table, ssize_t *pos, SCM key, SCM value);

void GuErr_SetString(SCM type, const char *message);
SCM GuErr_Format(SCM exception, const char *format, ...);
SCM GuErr_Occurred(void);
void GuErr_Fetch(SCM *ptype, SCM *pvalue, SCM *ptraceback);

SCM GuImport_ImportModule(const char *);

void GuModule_AddIntConstant (SCM module, const char *name, long value);

SCM GuObject_Repr(SCM);
SCM GuObject_GetAttrString (SCM x, const char *name);

SCM GuSequence_GetSlice (SCM obj, Gu_ssize_t i1, Gu_ssize_t i2);

char *GuString_AsString (SCM x);

// scm_length
ssize_t GuTuple_Size (SCM tuple);
#define GuTuple_Size(x) (scm_to_ssize_t(scm_length (x)))
#define GuTuple_Check(x) (scm_is_true (scm_list_p (x)))
SCM GuTuple_GetItem (SCM, ssize_t x);



void init_pycompat (void);

typedef int GuGILState_STATE;

inline GuGILState_STATE GuGILState_Ensure (void) {
  return 1;
}

G_END_DECLS

