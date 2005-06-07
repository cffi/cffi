#include "clisp.h"

extern object module__cffi_clisp__object_tab[];

subr_t module__cffi_clisp__subr_tab[1];
uintC module__cffi_clisp__subr_tab_size = 0;
subr_initdata_t module__cffi_clisp__subr_tab_initdata[1];

object module__cffi_clisp__object_tab[1];
object_initdata_t module__cffi_clisp__object_tab_initdata[1];
uintC module__cffi_clisp__object_tab_size = 0;


void module__cffi_clisp__init_function_1 (module_t* module)
{ }

void module__cffi_clisp__init_function_2 (module_t* module)
{
  register_foreign_function((void*)&sqrtf,"sqrtf",1024);
}
