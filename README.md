The parser needs to be updated, amongst other issues, 
the catches, positions and locals of each code are ignored
interfaces might be a list rather than just one element


need to test and expand using other dalvik examples


the block splitting for invokes is broken: invokes are the end of a block and not a block themselves

for now args are non empty except for invokes where the ony arg is the name of the method invoked

entry point needs to be defined