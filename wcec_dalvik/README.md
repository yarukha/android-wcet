The parser needs to be updated, amongst other issues, 
the catches, positions and locals of each code are ignored
interfaces might be a list rather than just one element


need to test and expand using other dalvik examples

ATM need to know successor list of each instructions (see lib/analysis/next_instruction.ml)
need to keep improving lexer in order to handle multi line string declarations

Parser DOESNT WORK for big files, need to rethink dvk structure to be lighter