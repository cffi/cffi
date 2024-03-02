[![Build Status](https://travis-ci.org/cffi/cffi.svg?branch=master)](https://travis-ci.org/cffi/cffi)

# What

CFFI, the Common Foreign Function Interface, purports
to be a portable FFI for Common Lisp. It abstracts away the
differences between the API of the native FFI's of the various Common
Lisp implementations.

# How

The CFFI library is composed of a Lisp-implementation-specific backend
in the CFFI-SYS package, and a portable frontend in the CFFI package.

The CFFI-SYS backend package defines a low-level interface to the
native FFI support in the Lisp implementation. It offers operators for
allocating and dereferencing foreign memory, calling foreign
functions, and loading shared libraries.

The CFFI frontend provides a more comfortable, declarative interface
for defining foreign functions, structures, typedefs, enumerated
types, etc. It is implemented in portable ANSI CL making use of the
low-level operators exported by CFFI-SYS.

The CFFI-LIBFFI subsystem loads support for passing structs by
value. It requires [libffi](https://sourceware.org/libffi/) for that.

Please consult [the
manual](http://common-lisp.net/project/cffi/manual/html_node/) for
further details, including installation instructions.

# Where

Please visit [Github](https://github.com/cffi/cffi/issues) for bug
reports, feature suggestions, the latest version, and to send your
contributions. CFFI also has a [mailing
list](https://mailman.common-lisp.net/listinfo/cffi-devel), and a
project page at [cffi.common-lisp.dev](https://cffi.common-lisp.dev/).

# Notes

### CFFI/C2FFI

CFFI/C2FFI is an ASDF-integrated mechanism to automatically generate a
complete CFFI binding from C header files.

Its input is one `.h` file (with possible `#include`s of course), and
its final output is a lisp file with the relevant CFFI binding forms.

It requires a CLI tool called [c2ffi](https://github.com/rpav/c2ffi),
but only for the developers of the C binding libraries, not their
users. `c2ffi` is written in C++, and it uses Clang as a library to
parse the C code, and emit the result as JSON. To skip this step,
these host-specific JSON files can be checked into the repos of the
binding libraries. This breaks the dependence on a working c2ffi
binary and the C header files, which can be a hurdle.

These JSON files are then used to automatically generate a CL file
with the corresponding CFFI forms. The generated bindings mirror the C
namespace into an empty CL package as closely as possible. This means
that the upper/lower case of the C names are retained. It helps with
reading the original docs and with rewriting C examples into
lisp. `#define`s are also mirrored as CL `defconstant`s.

Binding library developers are advised to introduce another package on
top of this raw layer to add more lispy constructs where appropriate
(e.g. `with-` macros that manage resources, etc).

Until CFFI/C2FFI is properly documented, you may check out these
projects as examples:
[hu.dwim.zlib](https://github.com/hu-dwim/hu.dwim.zlib),
[hu.dwim.sdl](https://github.com/hu-dwim/hu.dwim.sdl),
[hu.dwim.bluez](https://github.com/hu-dwim/hu.dwim.bluez), and
[hu.dwim.mosquitto](https://github.com/attila-lendvai/hu.dwim.mosquitto).

### Related projects

- [cl-autowrap](https://github.com/rpav/cl-autowrap) is another
  project that uses `c2ffi` to generate CFFI bindings.
