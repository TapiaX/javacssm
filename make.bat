@echo off
rem -ag >> recompile attribute grammar(s)
rem -ag -o >> recompile grammar(s), haskell modules and makes an executable
rem by default makes an executable without recompiling grammar(s) only modules
if '%1'=='-ag' goto ag
:out
ghc --make main.hs -o test\javacssm
del *.hi
del *.o
goto :eof
:ag
for %%f in (*.ag) do uuagc -dcfswH "%%f"
if '%2'=='-o' goto out
