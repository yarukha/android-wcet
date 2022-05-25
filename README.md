The parser needs to be updated, amongst other issues, 
the catches, positions and locals of each code are ignored
interfaces might be a list rather than just one element


need to test and expand using other dalvik examples

need to handle throws and double check each instruction potential branching in lib/analysis/hash2cfg.ml

FOR CFG:
dangerous instructions: move-exception (not sure about this one), switches 
switches are ignored for now
need to revisit how node names are defined, namely take care of virtual methods 
for now we ignore some invocation types and all others multi operands operators

for now args are non empty except for invokes where the ony arg is the name of the method invoked

entry point needs to be defined