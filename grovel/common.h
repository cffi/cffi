#include <cstdio>
#include <cstdlib>
#include <cstring>

#define CFFI_BUFSIZE 65536



///////////
//
// Printers
//
///////////

char* cffi_print_value(signed int value)
{
    static char buf[CFFI_BUFSIZE]; memset(buf, 0, CFFI_BUFSIZE);
    snprintf(buf, CFFI_BUFSIZE-1, "%d.", value);
    return buf;
}

char* cffi_print_value(unsigned int value)
{
    static char buf[CFFI_BUFSIZE]; memset(buf, 0, CFFI_BUFSIZE);
    snprintf(buf, CFFI_BUFSIZE-1, "%u.", value);
    return buf;
}

char* cffi_print_value(signed long value)
{
    static char buf[CFFI_BUFSIZE]; memset(buf, 0, CFFI_BUFSIZE);
    snprintf(buf, CFFI_BUFSIZE-1, "%ld.", value);
    return buf;
}

char* cffi_print_value(unsigned long value)
{
    static char buf[CFFI_BUFSIZE]; memset(buf, 0, CFFI_BUFSIZE);
    snprintf(buf, CFFI_BUFSIZE-1, "%lu.", value);
    return buf;
}

char* cffi_print_value(signed long long value)
{
    static char buf[CFFI_BUFSIZE]; memset(buf, 0, CFFI_BUFSIZE);
    snprintf(buf, CFFI_BUFSIZE-1, "%lld.", value);
    return buf;
}

char* cffi_print_value(unsigned long long value)
{
    static char buf[CFFI_BUFSIZE]; memset(buf, 0, CFFI_BUFSIZE);
    snprintf(buf, CFFI_BUFSIZE-1, "%llu.", value);
    return buf;
}

char* cffi_print_value(double value)
{
    static char buf[CFFI_BUFSIZE]; memset(buf, 0, CFFI_BUFSIZE);
    snprintf(buf, CFFI_BUFSIZE-1, "#.(let ((*read-default-float-format* 'double-float)) (read-from-string \"%.20E\"))", value);
    return buf;
}

char* cffi_print_value(void *value)
{
    static char buf[CFFI_BUFSIZE]; memset(buf, 0, CFFI_BUFSIZE);
    snprintf(buf, CFFI_BUFSIZE-1, "%lld.", (signed long long)value);
    return buf;
}

char* cffi_print_value(void (*value)())
{
    static char buf[CFFI_BUFSIZE]; memset(buf, 0, CFFI_BUFSIZE);
    snprintf(buf, CFFI_BUFSIZE-1, "%lld.", (signed long long)value);
    return buf;
}

char* cffi_print_value(void (*value)(int))
{
    static char buf[CFFI_BUFSIZE]; memset(buf, 0, CFFI_BUFSIZE);
    snprintf(buf, CFFI_BUFSIZE-1, "%lld.", (signed long long)value);
    return buf;
}

char* cffi_type_name(int signed_p, int size)
{
    static char buf[CFFI_BUFSIZE]; memset(buf, 0, CFFI_BUFSIZE);
    if (signed_p) {
        switch (size) {
        case 1: snprintf(buf, CFFI_BUFSIZE-1, ":int8"); break;
        case 2: snprintf(buf, CFFI_BUFSIZE-1, ":int16"); break;
        case 4: snprintf(buf, CFFI_BUFSIZE-1, ":int32"); break;
        case 8: snprintf(buf, CFFI_BUFSIZE-1, ":int64"); break;
        default: goto error;
        }
    } else {
        switch(size) {
        case 1: snprintf(buf, CFFI_BUFSIZE-1, ":uint8"); break;
        case 2: snprintf(buf, CFFI_BUFSIZE-1, ":uint16"); break;
        case 4: snprintf(buf, CFFI_BUFSIZE-1, ":uint32"); break;
        case 8: snprintf(buf, CFFI_BUFSIZE-1, ":uint64"); break;
        default: goto error;
        }
    }

    goto ok;

error:
    snprintf(buf, CFFI_BUFSIZE-1, "(cl:error \"No type of size ~D.\" %d)\n", size);
ok:
    return buf;
}



////////////////
//
// Helper macros
//
////////////////

#ifndef offsetof
# define offsetof(type, slot) ((long) ((char *) &(((type *) 0)->slot)))
#endif
#define CFFI_SIZEOFSLOT(type, slot) (sizeof(((type *) 0)->slot))
#define CFFI_SIZEOFSLOTMEMBER(type, slot) (sizeof(((type *) 0)->slot[0]))

#define CFFI_STRINGIFY(x) #x
#define CFFI_TYPE_SIGNED_P(type) (((type)-1)<0LL)



///////////////////////
//
// Definers - functions
//
///////////////////////

void cffi_defconstant(FILE *output, char *name, char *value, char *docstring)
{
    static char buf[CFFI_BUFSIZE]; memset(buf, 0, CFFI_BUFSIZE);
    if (docstring)
        snprintf(buf, CFFI_BUFSIZE-1, "\n  \"%s\"", docstring);
    fprintf(output, "(cl:defconstant %s %s%s)\n", name, value, buf);
}

void cffi_deftypesize(FILE *output, char *type, int type_size)
{
    static char buf[CFFI_BUFSIZE]; memset(buf, 0, CFFI_BUFSIZE);
    snprintf(buf, CFFI_BUFSIZE-1, "size-of-%s", type);
    cffi_defconstant(output, buf, cffi_print_value(type_size), NULL);
}

void cffi_defctype(FILE *output, char *type, char *canonical_type, int type_size)
{
    fprintf(output, "(cffi:defctype %s %s)\n", type, canonical_type);
    cffi_deftypesize(output, type, type_size);
}

void cffi_defcstruct_start(FILE *output, char *lisp_name, int type_size, char *docstring)
{
    static char buf[CFFI_BUFSIZE]; memset(buf, 0, CFFI_BUFSIZE);
    if (docstring)
        snprintf(buf, CFFI_BUFSIZE-1, "\n  \"%s\"", docstring);
    fprintf(output, "(cffi:defcstruct (%s :size %s)%s", lisp_name, 
            cffi_print_value(type_size), buf);
}

void cffi_signal_missing_definition(FILE *output, char *lisp_name)
{
    fprintf(output, "(cl:warn 'cffi-grovel:missing-definition :name '%s)\n", lisp_name);
}

void cffi_defcstruct_slot(FILE *output, char *slot_lname, char *slot_ltype, int count,
                          unsigned slot_offset)
{
    fprintf(output, "\n  (%s %s :count %s :offset %s)",
            slot_lname, slot_ltype, 
            cffi_print_value(count),
            cffi_print_value(slot_offset));
}

void cffi_defcunion_slot(FILE *output, char *slot_lname, char *slot_ltype, int count)
{
    fprintf(output, "\n  (%s %s :count %s)",
            slot_lname, slot_ltype, 
            cffi_print_value(count));
}

void cffi_defcunion_start(FILE *output, char *lisp_name, int type_size, char *docstring)
{
    static char buf[CFFI_BUFSIZE]; memset(buf, 0, CFFI_BUFSIZE);
    if (docstring)
        snprintf(buf, CFFI_BUFSIZE-1, "\n  \"%s\"", docstring);
    fprintf(output, "(cffi:defcunion (%s :size %s)%s", lisp_name, 
            cffi_print_value(type_size), buf);
}

void cffi_defcenum_start(FILE *output, char *lisp_name, char *base_type, char *docstring)
{
    static char buf[CFFI_BUFSIZE]; memset(buf, 0, CFFI_BUFSIZE);
    if (docstring)
        snprintf(buf, CFFI_BUFSIZE-1, "\n  \"%s\"", docstring);
    fprintf(output, "(cffi:defcenum (%s %s)%s", lisp_name, base_type, buf);
}

void cffi_defcenum_member(FILE *output, char *lisp_name, char *value, char *docstring)
{
    fprintf(output, "\n  (%s %s)", lisp_name, value);
}



////////////////////
//
// Definers - macros
//
////////////////////

#define CFFI_DEFCONSTANT(name, value, docstring)                        \
    cffi_defconstant(output, name, cffi_print_value(value), docstring)

#define CFFI_DEFTYPESIZE(lisp_name, ctype)              \
    cffi_deftypesize(output, lisp_name, sizeof(ctype))

#define CFFI_DEFCTYPE(lisp_name, ctype)                                 \
    cffi_defctype(output, lisp_name, cffi_type_name(CFFI_TYPE_SIGNED_P(ctype), sizeof(ctype)), sizeof(ctype))

#define CFFI_DEFCSTRUCT_START(lisp_name, ctype, docstring)              \
    cffi_defcstruct_start(output, lisp_name, sizeof(ctype), docstring)

#define CFFI_DEFCSTRUCT_END                     \
    fprintf(output, ")\n")

#define CFFI_DEFCSTRUCT_SLOT(struct_name, slot_cname, slot_lname, slot_ltype, count) \
    cffi_defcstruct_slot(output, slot_lname, slot_ltype, count,         \
                         offsetof(struct_name, slot_cname))

#define CFFI_DEFCSTRUCT_SLOT_AUTO(struct_name, slot_cname, slot_lname, slot_ltype) \
    cffi_defcstruct_slot(output, slot_lname, slot_ltype,                \
                         ((CFFI_SIZEOFSLOT(struct_name, slot_cname)) /  \
                          (CFFI_SIZEOFSLOTMEMBER(struct_name, slot_cname))), \
                         offsetof(struct_name, slot_cname))

#define CFFI_DEFCUNION_START(lisp_name, ctype, docstring)               \
    cffi_defcunion_start(output, lisp_name, sizeof(ctype), docstring)

#define CFFI_DEFCUNION_END                      \
    fprintf(output, ")\n")

#define CFFI_DEFCUNION_SLOT(union_name, slot_cname, slot_lname, slot_ltype, count) \
    cffi_defcunion_slot(output, slot_lname, slot_ltype, count)

#define CFFI_DEFCUNION_SLOT_AUTO(union_name, slot_cname, slot_lname, slot_ltype) \
    cffi_defcunion_slot(output, slot_lname, slot_ltype,                 \
                        ((CFFI_SIZEOFSLOT(union_name, slot_cname)) /    \
                         (CFFI_SIZEOFSLOTMEMBER(union_name, slot_cname))))

#define CFFI_DEFCENUM_START(lisp_name, base_type, docstring)            \
    cffi_defcenum_start(output, lisp_name, base_type, docstring)

#define CFFI_DEFCENUM_END                       \
    fprintf(output, ")\n")

#define CFFI_DEFCENUM_MEMBER(lisp_name, cname, docstring)       \
    cffi_defcenum_member(output, lisp_name, cffi_print_value(cname), docstring)
