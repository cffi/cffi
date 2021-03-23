#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <inttypes.h>
#include <stddef.h>

#ifndef offsetof
#define offsetof(type, slot) ((long) ((char *) &(((type *) 0)->slot)))
#endif
#define getslot(type, slot) (((type *) 0)->slot)
#define sizeofslot(type, slot) (sizeof(getslot(type, slot)))
#define countofslot(type, slot) \
  (sizeof(getslot(type, slot)) / sizeof(getslot(type, slot)[0]))
#define stringify(x) #x
#define indirect_stringify(x) stringify(x)

#define TYPE_SIGNED_P(type) (((type)-1)<0LL)
#define TYPE_FLOAT_P(type) (0.4 < ((type)0.5) && ((type)0.5 < 0.6))
#define _64_BIT_VALUE_FITS_SIGNED_P(value) ( (value) <= 0x7FFFFFFFFFFFFFFFLL )
#define SLOT_SIGNED_P(result, type, slot)				\
  do { 									\
    type slot_signed_p_struct; 						\
    slot_signed_p_struct.slot = -1; 					\
    (result) = slot_signed_p_struct.slot < 0;				\
  } while (0)
#define SLOT_FLOAT_P(result, type, slot)                                          \
  do {                                                                            \
    type slot_float_p_struct;                                                     \
    slot_float_p_struct.slot = 0.5;                                               \
    (result) = 0.4 < slot_float_p_struct.slot && slot_float_p_struct.slot < 0.6;  \
  } while (0)

void type_name(FILE *output, int signed_p, int float_p, int size) {
  if (signed_p && !float_p) {
    switch (size) {
    case 1: fprintf(output, ":int8"); break;
    case 2: fprintf(output, ":int16"); break;
    case 4: fprintf(output, ":int32"); break;
    case 8: fprintf(output, ":int64"); break;
    default: goto error;
    }
  } else if (signed_p && float_p) {
    if (size == 4) {
      fprintf(output, ":float");
    } else if (size == 8) {
      fprintf(output, ":double");
    } else if (size > 8) {
      fprintf(output, ":long-double");
    } else {
      goto error;
    }
  } else {
    switch(size) {
    case 1: fprintf(output, ":uint8"); break;
    case 2: fprintf(output, ":uint16"); break;
    case 4: fprintf(output, ":uint32"); break;
    case 8: fprintf(output, ":uint64"); break;
    default: goto error;
    }
  }

  return;

error:
  fprintf(output, "(cl:error \"No type of size ~D.\" %i)\n", size);
}

char* print_double_for_lisp(double n)
{
    static char buf[256];
    memset(buf, 0, 256);
    snprintf(buf, 255, "(let ((*read-default-float-format* 'double-float)) (coerce (read-from-string \"%.20E\") 'double-float))", n);
    return buf;
}
