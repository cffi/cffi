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

/*
 * Callbacks
 */

DLLEXPORT
int expect_char_sum(char (*f)(char, char))
{
    return f('a', 3) == 'd' ? 1 : 0;
}

DLLEXPORT
int expect_unsigned_char_sum(unsigned char (*f)(unsigned char, unsigned char))
{
    return f(UCHAR_MAX-1, 1) == UCHAR_MAX ? 1 : 0;
}

DLLEXPORT
int expect_short_sum(short (*f)(short a, short b))
{
    return f(SHRT_MIN+1, -1) == SHRT_MIN ? 1 : 0;
}

DLLEXPORT
int expect_unsigned_short_sum(unsigned short (*f)(unsigned short,
                                                  unsigned short))
{
    return f(USHRT_MAX-1, 1) == USHRT_MAX ? 1 : 0;
}

DLLEXPORT
int expect_int_sum(int (*f)(int, int))
{
    return f(INT_MIN+1, -1) == INT_MIN ? 1 : 0;
}

DLLEXPORT
int expect_unsigned_int_sum(unsigned int (*f)(unsigned int, unsigned int))
{
    return f(UINT_MAX-1, 1) == UINT_MAX ? 1 : 0;
}

DLLEXPORT
int expect_long_sum(long (*f)(long, long))
{
    return f(LONG_MIN+1, -1) == LONG_MIN ? 1 : 0;
}

DLLEXPORT
int expect_unsigned_long_sum(unsigned long (*f)(unsigned long, unsigned long))
{
    return f(ULONG_MAX-1, 1) == ULONG_MAX ? 1 : 0;
}

DLLEXPORT
int expect_float_sum(float (*f)(float, float))
{
    /*printf("\n>>> FLOAT: %f <<<\n", f(20.0f, 22.0f));*/
    return f(20.0f, 22.0f) == 42.0f ? 1 : 0;
}

DLLEXPORT
int expect_double_sum(double (*f)(double, double))
{
    /*printf("\n>>> DOUBLE: %f<<<\n", f(-20.0, -22.0));*/
    return f(-20.0, -22.0) == -42.0 ? 1 : 0;
}

DLLEXPORT
int expect_pointer_sum(void* (*f)(void*, int))
{
    return f(NULL, 0xDEAD) == (void *) 0xDEAD ? 1 : 0;
}

DLLEXPORT
int expect_strcat(char* (*f)(char*, char*))
{
    char *ret = f("Hello, ", "C world!");
    int res = strcmp(ret, "Hello, C world!") == 0 ? 1 : 0;
    free(ret); /* is this allowed? */
    return res;
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
    return a == b;
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

/* vim: ts=4 et
*/
