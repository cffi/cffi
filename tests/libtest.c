/* -*- Mode: C; tab-width: 4; indent-tabs-mode: nil -*-
 *
 * libtest.c --- auxiliary C lib for testing purposes
 *
 * Copyright (C) 2005, Luis Oliveira  <loliveira(@)common-lisp.net>
 *
 * Permission is hereby granted, free of charge, to any person
 * obtaining a copy of this software and associated documentation
 * files (the "Software"), to deal in the Software without
 * restriction, including without limitation the rights to use, copy,
 * modify, merge, publish, distribute, sublicense, and/or sell copies
 * of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be
 * included in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT.  IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
 * HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
 * WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 * DEALINGS IN THE SOFTWARE.
 */

#ifdef WIN32
#define DLLEXPORT __declspec(dllexport)
#else
#define DLLEXPORT
#endif

#include <limits.h>
#include <string.h>
#include <stdlib.h>
#include <math.h>

/*
 * Some functions that aren't avaiable on WIN32
 */

DLLEXPORT
float my_sqrtf(float n)
{
    return (float) sqrt((double) n);
}

DLLEXPORT
char *my_strdup(const char *str)
{
    char *p = malloc(strlen(str) + 1);
    strcpy(p, str);
    return p;
}

/*
 * Foreign Globals
 */

/* TODO: look into signed char vs. unsigned char issue */
DLLEXPORT char           var_char           = -127;
DLLEXPORT unsigned char  var_unsigned_char  = 255;
DLLEXPORT short          var_short          = -32767;
DLLEXPORT unsigned short var_unsigned_short = 65535;
DLLEXPORT int            var_int            = -32767;
DLLEXPORT unsigned int   var_unsigned_int   = 65535;
DLLEXPORT long           var_long           = -2147483647L;
DLLEXPORT unsigned long  var_unsigned_long  = 4294967295UL;
DLLEXPORT float          var_float          = 42.0f;
DLLEXPORT double         var_double         = 42.0;
DLLEXPORT void *         var_pointer        = NULL;
DLLEXPORT char *         var_string         = "Hello, foreign world!";

DLLEXPORT long long          var_long_long          = -9223372036854775807LL;
DLLEXPORT unsigned long long var_unsigned_long_long = 18446744073709551615ULL;

/*
 * Callbacks
 */

DLLEXPORT
int expect_char_sum(char (*f)(char, char))
{
    return f('a', 3) == 'd';
}

DLLEXPORT
int expect_unsigned_char_sum(unsigned char (*f)(unsigned char, unsigned char))
{
    return f(UCHAR_MAX-1, 1) == UCHAR_MAX;
}

DLLEXPORT
int expect_short_sum(short (*f)(short a, short b))
{
    return f(SHRT_MIN+1, -1) == SHRT_MIN;
}

DLLEXPORT
int expect_unsigned_short_sum(unsigned short (*f)(unsigned short,
                                                  unsigned short))
{
    return f(USHRT_MAX-1, 1) == USHRT_MAX;
}

DLLEXPORT
int expect_int_sum(int (*f)(int, int))
{
    return f(INT_MIN+1, -1) == INT_MIN;
}

DLLEXPORT
int expect_unsigned_int_sum(unsigned int (*f)(unsigned int, unsigned int))
{
    return f(UINT_MAX-1, 1) == UINT_MAX;
}

DLLEXPORT
int expect_long_sum(long (*f)(long, long))
{
    return f(LONG_MIN+1, -1) == LONG_MIN;
}

DLLEXPORT
int expect_unsigned_long_sum(unsigned long (*f)(unsigned long, unsigned long))
{
    return f(ULONG_MAX-1, 1) == ULONG_MAX;
}

DLLEXPORT
int expect_long_long_sum(long long (*f)(long long, long long))
{
    return f(LLONG_MIN+1, -1) == LLONG_MIN;
}

DLLEXPORT
int expect_unsigned_long_long_sum (unsigned long long
                                   (*f)(unsigned long long, unsigned long long))
{
    return f(ULLONG_MAX-1, 1) == ULLONG_MAX;
}

DLLEXPORT
int expect_float_sum(float (*f)(float, float))
{
    /*printf("\n>>> FLOAT: %f <<<\n", f(20.0f, 22.0f));*/
    return f(20.0f, 22.0f) == 42.0f;
}

DLLEXPORT
int expect_double_sum(double (*f)(double, double))
{
    /*printf("\n>>> DOUBLE: %f<<<\n", f(-20.0, -22.0));*/
    return f(-20.0, -22.0) == -42.0;
}

DLLEXPORT
int expect_pointer_sum(void* (*f)(void*, int))
{
    return f(NULL, 0xDEAD) == (void *) 0xDEAD;
}

DLLEXPORT
int expect_strcat(char* (*f)(char*, char*))
{
    char *ret = f("Hello, ", "C world!");
    int res = strcmp(ret, "Hello, C world!") == 0;
    /* commented out as a quick fix on platforms that don't
       foreign allocate in C malloc space. */
    /*free(ret);*/ /* is this allowed? */
    return res;
}

DLLEXPORT
void pass_int_ref(void (*f)(int*))
{
    int x = 1984;
    f(&x);
}

/*
 * Enums
 */

typedef enum {
    ONE = 1,
    TWO,
    THREE,
    FOUR,
    FORTY_ONE = 41,
    FORTY_TWO
} numeros;

DLLEXPORT
int check_enums(numeros one, numeros two, numeros three, numeros four,
                numeros forty_one, numeros forty_two)
{
    if (one == ONE && two == TWO && three == THREE && four == FOUR &&
        forty_one == FORTY_ONE && forty_two == FORTY_TWO)
        return 1;

    return 0;
}

typedef enum { FALSE, TRUE } another_boolean;

DLLEXPORT
another_boolean return_enum(int x)
{
    if (x == 0)
        return FALSE;
    else
        return TRUE;
}

/*
 * Booleans
 */

DLLEXPORT
int equalequal(int a, unsigned int b)
{
    return ((unsigned int) a) == b;
}

DLLEXPORT
char bool_and(unsigned char a, char b)
{
    return a && b;
}

DLLEXPORT
unsigned long bool_xor(long a, unsigned long b)
{
    return (a && !b) || (!a && b);
}

/*
 * Test struct alignment issues. These comments assume the x86 gABI.
 * Hopefully these tests will spot alignment issues in others archs
 * too.
 */

/*
 * STRUCT.ALIGNMENT.1
 */

struct s_ch {
    char a_char;
};

/* This struct's size should be 2 bytes */
struct s_s_ch {
    char another_char;
    struct s_ch a_s_ch; 
};

DLLEXPORT
struct s_s_ch the_s_s_ch = { 2, { 1 } };

/*
 * STRUCT.ALIGNMENT.2
 */

/* This one should be alignment should be the same as short's alignment. */
struct s_short {
    char a_char;
    char another_char;
    short a_short;
};

struct s_s_short {
    char yet_another_char;
    struct s_short a_s_short; /* so this should be 2-byte aligned */
};  /* size: 6 bytes */

DLLEXPORT
struct s_s_short the_s_s_short = { 4, { 1, 2, 3 } };

/*
 * STRUCT.ALIGNMENT.3
 */

/* This test will, among other things, check for the existence tail padding. */

struct s_double {
    char a_char;       /* 1 byte */
                       /* padding: 3 bytes */
    double a_double;   /* 8 bytes */
    char another_char; /* 1 byte */
                       /* padding: 3 bytes */
};                     /* total size: 16 bytes */

struct s_s_double {
    char yet_another_char;      /* 1 byte */
                                /* 3 bytes padding */
    struct s_double a_s_double; /* 16 bytes */
    short a_short;              /* 2 byte */
                                /* 2 bytes padding */
};                              /* total size: 24 bytes */

DLLEXPORT
struct s_s_double the_s_s_double = { 4, { 1, 2.0, 3 }, 5 };

/* STRUCT.ALIGNMENT.4 */
struct s_s_s_double {
    short another_short;            /* 2 bytes */
                                    /* 2 bytes padding */
    struct s_s_double a_s_s_double; /* 24 bytes */
    char last_char;                 /* 1 byte */
                                    /* 3 bytes padding */
};                                  /* total size: 32 */

DLLEXPORT
struct s_s_s_double the_s_s_s_double = { 6, { 4, { 1, 2.0, 3 }, 5 }, 7 };

/* STRUCT.ALIGNMENT.x */

/* commented this test out because this is not standard C
   and MSVC++ (or some versions of it at least) won't compile it. */

/*
struct empty_struct {};

struct with_empty_struct {
    struct empty_struct foo;
    int an_int;
};

DLLEXPORT
struct with_empty_struct the_with_empty_struct = { {}, 42 };
*/

/*
 * DEFCFUN.NOARGS and DEFCFUN.NOOP
 */

DLLEXPORT
int noargs()
{
    return 42;
}

DLLEXPORT
void noop()
{
    return;
}

/* vim: ts=4 et
*/
