rem
rem script for compiling the test lib with the free MSVC++ toolkit.
rem

cl /ML /LD -D_MT /DWIN32=1 libtest.c
del libtest.obj libtest.exp

cl /ML /LD -D_MT /DWIN32=1 libtest2.c
del libtest2.obj libtest2.exp
