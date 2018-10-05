#ifndef GIR_TYPE_H
#define GIR_TYPE_H

#define GITYPEX(NAME)				\
  extern SCM s_ ## NAME ## _type;

GITYPEX(GIArgInfo);
GITYPEX(GICallbackInfo);
GITYPEX(GIConstInfo);
GITYPEX(GIConstantInfo);
GITYPEX(GIEnumInfo);
GITYPEX(GIFieldInfo);
GITYPEX(GIFunctionInfo);
GITYPEX(GIInterfaceInfo);
GITYPEX(GIObjectInfo);
GITYPEX(GIPropertyInfo);
GITYPEX(GISignalInfo);
GITYPEX(GIStructInfo);
GITYPEX(GITypeInfo);
GITYPEX(GIUnionInfo);
GITYPEX(GIVFuncInfo);
GITYPEX(GIValueInfo);

extern SCM s_GIRepository_type;
extern SCM s_GITypelib_type;
void gir_init_types();

#endif
