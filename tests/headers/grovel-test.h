/*
 * Factitious C header file for testing CFFI-GROVELER
 */

#ifndef _GROVEL_TEST_H
#define _GROVEL_TEST_H

#define TAGGED_ARRAY_MAX_LENGTH 64

struct tagged_array {
  void *arr[TAGGED_ARRAY_MAX_LENGTH];
  unsigned int len;
};

#endif // _GROVEL_TEST_H
