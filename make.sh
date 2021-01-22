
# experimental version in bash of make.bat 
# -ag >> recompile attribute grammar(s)
# -ag -o >> recompile grammar(s), haskell modules and makes an executable
# by default makes an executable without recompiling grammar(s)
make ()
{
	ghc --make main.hs -o test/javacssm
	rm *.hi
	rm *.o
}

if ["$1"=="-ag"]
then
	for F in ./*.ag; do
		uuagc -dcfswH $F
	done

    if ["$2"=="-o"] 
    then
    	make
    fi
else
	make
fi

