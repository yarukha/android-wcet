To install dependencies 
```
opam install dune menhir fix apron progress lp-glpk camlzip
```
You need sdk-platform tools https://developer.android.com/studio/releases/platform-tools
As well as glpk, for instance on arch 
```
sudo pacman -S glpk
```
Run ``make`` to build

The executable is located in ``_build/default/src/bin/main.exe``

Alternatively for a simple execution you can run 
```
dune exec -- wcec-dvk -a <file.apk> [-p <powermodel.txt>]
```