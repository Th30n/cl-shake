# cl-shake

This is my hobby project for learning Common Lisp. It is a first person game
in vein of original Doom and Quake. Most of the engine I'm building as a part
of this project is based on my insights from Doom and Quake engines. The idea
is not to copy or reimplement that code, but come to my own solution by
studying approaches of those engines. Oh, and I always wanted to make a BSP
ased rendering engine :)

# Installation

The code has only been tested with SBCL so I recommend installing that. The
common preparation steps for various OSes is:

  * Install SBCL (or other Common Lisp compiler)
  * Install quicklisp (from https://www.quicklisp.org)

The required C libraries:

  * OpenGL 3.3+ implementation (this comes with your GPU drivers)
  * SSL implementation (OpenSSL or LibreSSL)
  * SDL2 (for cl-shake game itself)
  * Qt 4.8 (for cl-shake-ed map editor)
  
The remaining Common Lisp packages should be handled through quicklisp.

To run the game, start SBCL and type:

    (ql:quickload "shake")
    (shake:main)
    
To run the map editor, start SBCL and type:

    (ql:quickload "shake-ed")
    (shake-ed:main)

## Linux

Simple calls to your favorite package manager should be enough.

    # On ArchLinux via pacman
    sudo pacman -S openssl sdl2 qt4
    
    # On systems with APT
    sudo apt-get install libssl-dev libsdl2-dev libqt4-dev

## Windows

As usual, setting up libraries on Windows is harder than on GNU/Linux. For
64bit Windows, libraries need to be compiled as such.

The unofficial OpenSSL binary can be downloaded from
http://slproweb.com/products/Win32OpenSSL.html.

The SDL2 can be downloaded from https://www.libsdl.org/download-2.0.php.

The Qt 4.8 can be downloaded from
https://download.qt.io/archive/qt/4.8/. Unfortunately, the 64bit version does
not have a prebuilt binary. You will need to download the source and build it
yourself for 64bit Windows. Build instructions are here:
http://doc.qt.io/qt-4.8/install-win.html.

All of the libraries and `/bin/` directory of Qt need to be on `PATH`
environment variable.
