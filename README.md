[![CI](https://github.com/yarukha/wcec-dvk/actions/workflows/main.yml/badge.svg)](https://github.com/yarukha/wcec-dvk/actions/workflows/main.yml)

To install the `ocaml` dependencies, first install `opam` and  
```
opam install dune menhir fix apron progress lp-glpk camlzip
```
You need a few dependencies to install the former `ocaml` libraries. You can get them on ubuntu with 
```
sudo apt-get install libgmp-dev pkg-config libglpk-dev libmpfr-dev
```
You will also need the sdk-platform tools https://developer.android.com/studio/releases/platform-tools. Make sure that `dexdump` is in your PATH


Run ``make`` to build

The executable is located in ``_build/default/src/bin/main.exe``

Alternatively for a simple execution you can run 
```
dune exec -- wcec-dvk -a <file.apk> [-p <powermodel.txt>]
```