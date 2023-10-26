# cl-shake

This is my hobby project for learning Common Lisp.  It is a first person
shooter game in vein of original Doom and Quake.  Most of the engine I'm
building as a part of this project is based on my insights from Doom and Quake
engines.  The idea is not to copy or reimplement that code, but come to my own
solution by studying approaches of those engines.  Oh, and I always wanted to
make a BSP based rendering engine :)

# Installation

The code has only been tested with SBCL so I recommend installing that.  The
common preparation steps for various OSes is:

  * Install SBCL (or other Common Lisp compiler)
  * Install quicklisp (from https://www.quicklisp.org)
    - Add cl-shake as local project (e.g. by linking
      $HOME/quicklisp/local-projects/cl-shake to repo root)

The required C libraries

  * For cl-shake game itself:
    - OpenGL 3.3+ implementation (this comes with your GPU drivers)
    - SDL2
  * For cl-shake-ed map editor:
    - SSL implementation (OpenSSL or LibreSSL, for drakma used by qtools)
    - Qt 4.8 (optional, qtools will download precompiled libraries)

The remaining Common Lisp packages should be handled through quicklisp.

On your first run, you need to setup some initial dependencies, so start SBCL
and type.

    (pushnew :shiva-double-float *features*)
    ;; This will also download initially needed packages for shake.
    (ql:quickload "shake-bspc")
    ;; This will compile a level to be used by the game.
    (sbsp:compile-map-file "shake-ed/e1m1.map" "test.bsp")
    (quit)

To run the game, execute `run-shake`.

To run the map editor, execute `run-shake-ed`.

## Linux

Simple calls to your favorite package manager should be enough.

    # On ArchLinux via pacman
    sudo pacman -S sdl2 openssl qt4

    # On systems with APT
    sudo apt-get install libsdl2-dev libssl-dev libqt4-dev

## Windows

As usual, setting up libraries on Windows is harder than on GNU/Linux.  If you
installed a 64bit Common Lisp compiler, libraries need to be compiled for
64bit.

The SDL2 can be downloaded from https://www.libsdl.org/download-2.0.php.

The unofficial OpenSSL binary can be downloaded from
http://slproweb.com/products/Win32OpenSSL.html.

With Qt 4.8 things are a bit tricky.  The qtools package will download the
custom, prebuilt libraries and everything should work.  If you want to use the
tools provided by Qt (e.g. qmake, rcc), you will need to download Qt.  Qt 4.8
can be downloaded from https://download.qt.io/official_releases/qt/4.8/.
Unfortunately, the download link does not have the 64bit prebuilt binary.  You
will need to download the source and build it yourself for 64bit Windows.
Build instructions are here: http://doc.qt.io/qt-4.8/install-win.html.
Perhaps the simplest solution when using 64bit compiler and libraries, is to
download 32bit Qt for the tools.  You should then ensure that 32bit Qt
libraries are not seen when running shake-ed.

All of the libraries and `/bin/` directory of Qt (if you installed it) need to
be on `PATH` environment variable.  Be careful with putting 32 bit Qt on `PATH`
while using 64bit compiler and 64bit Qt libraries downloaded by qtools.  In
such a case, you should only expose Qt executables you want to use.

# Licensing

This project uses the GPL-2.0 license, for details see the `LICENSE`
file.  Copyright (C) 2016-2017 Teon Banek
