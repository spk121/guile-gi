/* Python compatibility functions. */
#pragma once
#include <libguile.h>
#include <glib.h>

G_BEGIN_DECLS

typedef ssize_t Gu_ssize_t;

extern SCM Gu_None;
extern SCM GuExc_TypeError;
extern SCM GuExc_ImportError;
extern SCM GuExc_RuntimeError;
extern SCM GuExc_RuntimeWarning;
extern SCM GuExc_Warning;
extern SCM GuExc_DeprecationWarning;

#define METH_VARARGS 1
#define METH_KEYWORDS 2
#define METH_NOARGS 4
#define METH_O 8

typedef SCM (*GuCFunction)(SCM a, SCM b);

typedef struct _GuMethodDef {
  const char *ml_name;
  GuCFunction ml_meth;
  int ml_flags;
  const char *ml_doc;
} GuMethodDef;


SCM Gu_BuildValue (const char *format, ...);
void Gu_DECREF(SCM x);
void Gu_XDECREF(SCM x);
void Gu_INCREF(SCM x);
void Gu_XINCREF(SCM x);
int Gu_IsInitialized(void);
SCM Gu_TYPE(SCM x);

int GuArg_ParseTuple(SCM args, const char *format, ...);
int GuArg_ParseTupleAndKeywords(SCM args, SCM kw, const char *format, char *keywords[], ...);
#define GuArg_ParseList GuArg_ParseTuple
int GuArg_VaParse(SCM args, const char *format, va_list vargs);

int GuCallable_Check(SCM o);

int GuCapsule_CheckExact (SCM x);
void *GuCapsule_GetPointer (SCM x, const char *name);

int GuDict_Next(SCM hash_table, ssize_t *pos, SCM key, SCM value);
int GuDict_SetItemString(SCM p, const char *key, SCM val);
SCM GuDict_GetItemString(SCM p, const char *key);
int GuDict_Check(SCM p);
int GuDict_DelItemString(SCM p, const char *key);

void GuErr_SetString(SCM type, const char *message);
void GuErr_SetObject(SCM type, SCM value);
SCM GuErr_Format(SCM exception, const char *format, ...);
SCM GuErr_Occurred(void);
void GuErr_Fetch(SCM *ptype, SCM *pvalue, SCM *ptraceback);
#define GuErr_Print() GuErr_PrintEx(1)
void GuErr_PrintEx(int set_sys_last_vars);
void GuErr_Restore(SCM ype, SCM value, SCM traceback);
void GuErr_Clear(void);
int GuErr_WarnEx(SCM category, const char* message, ssize_t stack_level);
#define GuErr_Warn(c,m) (GuErr_WarnEx((c),(m),1))
SCM GuErr_NewException(const char *name, SCM base, SCM dict);

void GuEval_InitThreads(void);
SCM GuImport_ImportModule(const char *);

typedef int GuGILState_STATE;
void GuGILState_Release(GuGILState_STATE x);
void GuModule_AddIntConstant (SCM module, const char *name, long value);
SCM GuModule_GetDict(SCM module);
int GuModule_AddStringConstant(SCM module, const char *name, const char *value);

SCM GuObject_Repr(SCM);
SCM GuObject_GetAttrString (SCM x, const char *name);
SCM GuObject_CallMethod(SCM obj, const char *name, const char *format, ...);
SCM GuSequence_GetSlice (SCM obj, Gu_ssize_t i1, Gu_ssize_t i2);
int GuObject_HasAttrString(SCM obj, const char *attr_name);
int GuObject_SetAttrString(SCM o, const char *attr_name, SCM v);
int GuObject_TypeCheck(SCM o, SCM type);
SCM GuObject_CallObject(SCM callable, SCM args);
SCM GuObject_GenericGetAttr(SCM self, SCM attr);

void (*GuOS_getsig(int i))(int);

char *GuString_AsString (SCM x);
int GuSequence_Check(SCM x);
ssize_t GuSequence_Length (SCM x);
SCM GuSequence_GetItem(SCM o, ssize_t i);

SCM GuSequence_Concat(SCM o1, SCM o2);
// scm_length
ssize_t GuTuple_Size (SCM tuple);
#define GuTuple_Size(x) (scm_to_ssize_t(scm_length (x)))
#define GuTuple_Check(x) (scm_is_true (scm_list_p (x)))
SCM GuTuple_GetItem (SCM, ssize_t x);
ssize_t GuTuple_GET_SIZE(SCM p);
SCM GuTuple_GET_ITEM(SCM p, ssize_t pos);
int GuTuple_SetItem(SCM p, ssize_t pos, SCM o);
SCM GuTuple_New(ssize_t len);

int GuType_Check (SCM o);
int GuType_IsSubtype(SCM a, SCM b);

typedef void (*GuCapsule_Destructor)(SCM);
SCM GuCapsule_New(void *pointer, const char *name, GuCapsule_Destructor destructor);

int GuModule_AddObject(SCM module, const char *name, SCM value);
void init_pycompat (void);

typedef int GuGILState_STATE;

inline GuGILState_STATE GuGILState_Ensure (void) {
  return 1;
}


#define Gu_BEGIN_ALLOW_THREADS
#define Gu_END_ALLOW_THREADS

G_END_DECLS

