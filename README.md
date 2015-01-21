# compose-z3-tutorial
Z3 tutorial for compose conference. 


Use F# and the .NET API of Z3. 


Get Z3 
======

You can use the Z3 dlls in /platform/{windows,osx-mono}. Or get/build from the source:


Using Windows
~~~~~~~~~~~~~~~~~~~~~
Download the Z3 binary from http://z3.codeplex.com/releases. 
Choose the 32-bit dll?

If you want to, build it yourself using VS2013. 
SI: Z3 link? 


Using Mono (for Linux and MacOS)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

build the .NET bindings of z3 using mono.
For this, get z3 sources for z3/unstable (4.3.2 is known to work) from
   http://z3.codeplex.com/

export Z3DIR=/path/to/z3/,  and follow these steps:

(0) Install software needed for the build process:
     * g++
     * python
     * mono for .NET 4.0
     * xbuild
     * fsharp

    On a Debian (>> squeezy) or Ubuntu (>= 14.04 LTS) system, this suffices:
      $ sudo apt-get install build-essential python mono-complete mono-xbuild fsharp

    On OS X, install the Mono MDK for Mac OS from
       http://www.mono-project.com/download/
    and install the XCode development tools (e.g., by executing "gcc" in
    a Terminal -- if it's not there yet, OS X will offer to install XCode).

(1) Build z3.
    On Linux, this suffices:
      $ pushd "$Z3DIR"
      $ ./configure
      $ pushd "$Z3DIR/build"
      $ make
      $ popd && popd

    On OS X, you need to enforce a 32bit build (for compatibility with Mono):
      $ pushd "$Z3DIR"
      $ ./configure

bash-3.2$ ./configure 
bash: ./configure: /bin/sh^M: bad interpreter: No such file or directory
bash-3.2$ 
SI: seems to have ^M control chars in it.
$ tr -d ‘\r’ < configure > configure.fixed
$ chmod u+x configure.fixed; mv configure.fixed configure

      $ pushd "$Z3DIR/build"
      $ perl -i -pe 's/-D_AMD64_/-arch i386/; s/LINK_EXTRA_FLAGS=/$&-arch i386 /' config.mk
      $ make
SI: this might take a few mins. 

      $ popd && popd

(2) Build the .NET bindings for z3:
      $ pushd "$Z3DIR/src/api/dotnet/"
      $ echo -e '<configuration>\n <dllmap dll="libz3.dll" target="libz3.dylib" os="osx"/>\n <dllmap dll="libz3.so" target="libz3.dylib" os="linux"/>\n</configuration>\n' > Microsoft.Z3.config
      $ xbuild Microsoft.Z3.csproj
SI: 1 Warning(s)
      $ popd

(3) Copy the newly built z3 and its .NET bindings to where we’re going to do our work:
      $ cp "$Z3DIR/src/api/dotnet/obj/Debug/Microsoft.Z3.*" scripts
      $ cp "$Z3DIR/build/libz3.*" scripts

SI: Do this copy to where we want to run from, like scripts. Seems that once all dlls+wrapper dlls are in the same place, fsi should be able to load. 

$ fsharpi -I:../platform/osx-mono -r:Microsoft.Z3
> open Microsoft.Z3 ;;
> let ctx = new Context ();;

val ctx : Context

> 
