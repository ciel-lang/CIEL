## Notes: CIEL on Apple Silicon

We need to adapt requirements to our environment, and since Apple Silicon is
different from other Unix-like OSes, also in terms of where libraries
and their header files are located, i.e. installed via `homebrew`, we
need to perform a few additional steps to have CIEL working as it should.

First and foremost, make sure that you are running up to date versions of
Common Lisp implementation (only tested with SBCL), and `asdf`. Also
make sure to have added Ultralisp to Quicklisp dists.

### SBCL (homebrew)

```example
$ brew install sbcl
$ curl -o /tmp/quicklisp.lisp https://beta.quicklisp.org/quicklisp.lisp
$ sbcl --load /tmp/quicklisp.lisp
```

```commonlisp
(quicklisp-quickstart:install)
(sb-ext:exit)
```

```example
$ rm /tmp/quicklisp.lisp
```

### SBCL via Roswell (homebrew)

```example
$ brew update && brew install roswell
$ ros # installs sbcl by default
$ ros run # verify by starting REPL
```

### ASDF
```example
$ mkdir -p ~/common-lisp && cd ~/common-lisp
$ git clone https://gitlab.common-lisp.net/asdf/asdf.git
```

### Update Quicklisp

Needed if SBCL is installed via Roswell:

```commonlisp
(ql:update-dist "quicklisp")
```

### Ultralisp

```commonlisp
(ql-dist:install-dist "http://dist.ultralisp.org/"
                          :prompt nil)
(sb-ext:exit)
```

### Packages and Dependencies

#### Install dependencies via homebrew
```example
$ brew update && brew install \
       zstd \
       gcc \
       sdl2 \
       libmagic \
       jpeg-turbo
```

#### Set environment variables

Then set up some environment variables before endavouring to build
everything (change llvm version according to what you have installed,
but do note that `c2ffi` requires matching tags, see c2ffi's
[README](https://github.com/rpav/c2ffi)).

```example
export CPPFLAGS=-I/opt/homebrew/opt/llvm@18/include
export LDFLAGS=-L/opt/homebrew/opt/llvm@18/lib
export LLVM_DIR=/opt/homebrew/opt/llvm@18/lib/cmake/llvm
export Clang_DIR=/opt/homebrew/opt/llvm@18
export CC=/opt/homebrew/opt/llvm@18/bin/clang
export CXX=/opt/homebrew/opt/llvm@18/bin/clang++
export CPATH=/opt/homebrew/include
```

#### c2ffi

We start by compiling `c2ffi`, which is not available as a homebrew formula:

```example
$ git clone https://github.com/rpav/c2ffi.git
$ cd c2ffi && mkdir build && cd build
```
Since  resulting binary is generated as `./bin/c2ffi`, we need to
make sure is available on `PATH`, so we could do, e.g.:

```example
$ cmake -DCMAKE_INSTALL_PREFIX=$HOME/.local ..
$ make
$ make install
[100%] Built target c2ffi
Install the project...
-- Install configuration: "Release"
-- Installing: /Users/<USERNAME>/.local/bin/c2ffi
```

#### SDL2

Specs are missing for SDL2 on MacOS, thus we need up-to-date packages, and change the
`cl-sdl2` included `autowrap.lisp` source file. Furthermore, we can download an already
generated spec, as shown below:

```example
$ cd ~/quicklisp/local-projects
$ git clone https://github.com/cffi/cffi.git
$ git clone https://github.com/rpav/cl-autowrap.git
$ git clone https://github.com/lispgames/cl-sdl2.git
$ cd cl-sdl2/src
$ curl -O https://raw.githubusercontent.com/ellisvelo/cl-sdl2/main/src/autowrap.lisp
$ cd spec
$ curl -O https://raw.githubusercontent.com/ellisvelo/cl-sdl2/main/src/spec/SDL2.aarch64-apple-darwin9.spec
```
Ref: <https://github.com/lispgames/cl-sdl2/issues/154#issuecomment-1280030566>


#### CIEL

```commonlisp
(ql:quickload :ciel)
```

**NB!** Sometimes there are issues with exhausted heap stack. Try starting
`sbcl` with `sbcl --dynamic-space-size 2024`. Or rerun the loading of CIEL
in REPL. 

#### Building CIEL binary and core image (NOT WORKING YET)

**NB!** The build process generates an error based on not being able to
find foreign library on MacOS Sonoma, specifically:

- `/System/Library/Frameworks/GLUT.framework/GLUT`

which, supposedly, is now found here:

- `/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/System/Library/Frameworks/GLUT.framework`

Note, however, that there is no `GLUT` folder in there. 

```example
$ git clone https://github.com/ciel-lang/CIEL ~/quicklisp/local-projects/CIEL
$ cd ~/quicklisp/local-projects/CIEL
$ make build
````

Error snip:

```example
Unhandled FILE-DOES-NOT-EXIST in thread #<SB-THREAD:THREAD tid=259 "main thread" RUNNING
                                               {7004F805E3}>:
    The file #P"/System/Library/Frameworks/GLUT.framework/GLUT" does not exist:
      No such file or directory
    
Backtrace for: #<SB-THREAD:THREAD tid=259 "main thread" RUNNING {7004F805E3}>
0: (SB-DEBUG::DEBUGGER-DISABLED-HOOK #<FILE-DOES-NOT-EXIST {7006625A83}> #<unused argument> :QUIT T)
1: (SB-DEBUG::RUN-HOOK *INVOKE-DEBUGGER-HOOK* #<FILE-DOES-NOT-EXIST {7006625A83}>)
2: (INVOKE-DEBUGGER #<FILE-DOES-NOT-EXIST {7006625A83}>)
3: (ERROR FILE-DOES-NOT-EXIST :PATHNAME #P"/System/Library/Frameworks/GLUT.framework/GLUT" :MESSAGE "No such file or directory")
4: (SB-IMPL::FILE-PERROR #P"/System/Library/Frameworks/GLUT.framework/GLUT" 2 FILE-DOES-NOT-EXIST)
5: (SB-IMPL::%OPEN-ERROR #P"/System/Library/Frameworks/GLUT.framework/GLUT" 2 :IGNORE-THIS-ARG :ERROR)
6: (OPEN #P"/System/Library/Frameworks/GLUT.framework/GLUT" :DIRECTION :INPUT :ELEMENT-TYPE (UNSIGNED-BYTE 8) :IF-EXISTS NIL :IF-DOES-NOT-EXIST :ERROR :EXTERNAL-FORMAT :DEFAULT :CLASS SB-SYS:FD-STREAM)
7: (UIOP/STREAM:CONCATENATE-FILES (#P"/System/Library/Frameworks/GLUT.framework/GLUT") #P"/Users/username/quicklisp/local-projects/CIEL/bin/GLUT")
8: ((FLET DEPLOY::FOREIGN-LIBRARIES :IN "/Users/username/quicklisp/dists/ultralisp/software/Shinmera-deploy-20240730212609/deploy.lisp") :DIRECTORY #P"/Users/username/quicklisp/local-projects/CIEL/bin/")
9: (DEPLOY:RUN-HOOKS :DEPLOY :DIRECTORY #P"/Users/username/quicklisp/local-projects/CIEL/bin/" :SYSTEM #<ASDF/SYSTEM:SYSTEM "ciel/repl"> :OP #<DEPLOY:DEPLOY-OP >)
```
