/* -*- Mode: C; tab-width: 4; indent-tabs-mode: nil -*-
 *
 * libfsbv.c --- auxiliary C lib for testing foreign structure by value calls
 *
 * Copyright (C) 2011 Liam M. Healy
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

#include <stdio.h>
#include <limits.h>
#include <string.h>
#include <stdlib.h>
#include <math.h>
#include <float.h>

/* MSVC doesn't have stdint.h and uses a different syntax for stdcall */
#ifndef _MSC_VER
#include <stdint.h>
#endif

#ifdef WIN32
#ifdef _MSC_VER
#define STDCALL __stdcall
#else
#define STDCALL __attribute__((stdcall))
#endif
#else
#define STDCALL
#endif

struct struct_pair {
    int a;
    int b;
};

int sumpair (struct struct_pair sp);
struct struct_pair doublepair (struct struct_pair dp);

DLLEXPORT
int sumpair (struct struct_pair sp)
{
  return sp.a + sp.b;
}

DLLEXPORT
struct struct_pair doublepair (struct struct_pair dp)
{
  struct struct_pair ret;
  ret.a = 2*dp.a;
  ret.b = 2*dp.b;
  return ret;
}
