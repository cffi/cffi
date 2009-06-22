#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>

#ifndef offsetof
#define offsetof(type, slot) ((int) ((char *) &(((type *) 0)->slot)))
#endif
#define sizeofslot(type, slot) (sizeof(((type *) 0)->slot))
#define stringify(x) #x
#define indirect_stringify(x) stringify(x)

#define SIGNEDP(x) (((x)-1)<0)
#define SIGNED_(x) (SIGNEDP(x)?"":"un")
#define SIGNED64P(x) ( x <= 0x7FFFFFFFFFFFFFFFLL )

void type_name(FILE *output, int signed_p, int size) {
  if (signed_p) {
    switch (size) {
    case 1: fprintf(output, ":int8"); break;
    case 2: fprintf(output, ":int16"); break;
    case 4: fprintf(output, ":int32"); break;
    case 8: fprintf(output, ":int64"); break;
    default: goto error;
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
