# TildeBackend (TildeBE / TB)

TB is compiler backend in the form of a reasonable C library.

## Getting Started

## Progress

Currently there's still a lot of work to be done before the backend reached it's first stable release. But the current roadmap goes as follows.

- [x] IR Builder API
- [x] ELF64 object file output (EARLY)
- [x] COFF object file output
- [x] Debug x64 support
- [x] x64 SSE vector support
- [ ] Simple Optimizer (EARLY)
- [ ] Aarch64 support
- [ ] Aarch64 NEON support
- [ ] Aarch64 SVE support
- [ ] x64 AVX vector support
- [ ] Optimized Aarch64 support
- [ ] Optimized x64 support

## Compiling

Clone the repo with `git clone https://github.com/RealNeGate/tilde-backend.git` <br>
Then just compile & run the compile.c with the C compiler of your choice <br>
You should now have either a tildebackend.lib or tildebackend.a depending on the platform <br>
Copy this and the `tb.h` file into your project and use it as you please <br>
