The parser needs to be updated, amongst other issues, 
the catches, positions and locals of each code are ignored
interfaces might be a list rather than just one element


need to test and expand using other dalvik examples

need to handle throws and double check each instruction potential branching in lib/analysis/hash2cfg.ml


dangerous instructions: move-exception (not sure about this one), switches 
switches are ignored for now


The lexer cannot deal with odd number of " in strings, need to MANUALLY remove all of them for now

CFG construction done; obviously needs to be revisited and double checked
entry point needs to be defined