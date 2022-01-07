[![Build Status](https://travis-ci.org/cffi/cffi.svg?branch=master)](https://travis-ci.org/cffi/cffi)

CFFI, the Common Foreign Function Interface, purports to be a portable
foreign function interface for Common Lisp. The CFFI library is
composed of a Lisp-implementation-specific backend in the CFFI-SYS
package, and a portable frontend in the CFFI package.

The CFFI-SYS backend package defines a low-level interface to the
native FFI support in the Lisp implementation. It offers operators for
allocating and dereferencing foreign memory, calling foreign
functions, and loading shared libraries.

The CFFI frontend provides a declarative interface for defining
foreign functions, structures, typedefs, enumerated types, etc. It is
implemented in portable ANSI CL making use of the low-level operators
exported by CFFI-SYS.

CFFI/C2FFI is an ASDF-integrated mechanism to automatically generate a
complete CFFI binding for a C project using the JSON output of
[c2ffi](https://github.com/rpav/c2ffi). c2ffi generates its output by
using LLVM/Clang as a library to parse any C project. Until CFFI/C2FFI
is properly documented, please see these projects as examples:
[hu.dwim.bluez](https://github.com/hu-dwim/hu.dwim.bluez),
[hu.dwim.zlib](https://github.com/hu-dwim/hu.dwim.zlib),
[hu.dwim.sdl](https://github.com/hu-dwim/hu.dwim.sdl), and
[hu.dwim.mosquitto](https://github.com/attila-lendvai/hu.dwim.mosquitto).

Please consult [the
manual](http://common-lisp.net/project/cffi/manual/html_node/) for
further details, including installation instructions.

Please visit [Github](https://github.com/cffi/cffi/issues) for bug
reports and feature suggestions.
