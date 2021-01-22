##javacssm

This is a compiler for Java to SSM.
Based in Java 13.
Uses uuagc 0.9.52.2 and ssm 1.3.3 of Atze Dijkstra at Utrecht University, thanks alot.

The present project tries to demostrate some concepts of programming language and compiler such as error detection, memory allocation and execution path. As part of the Compiler Design course this project approaches to machine code for JVM returning a Simple Stack Machine language as result. The source code processed is a reduced and slightly different version of Java. At the moment it is focused in estructured programming. 

A list of goals has written in todo.txt file. Major functionalities: Global/local scoped variables, function declaration and return statement.
Examples of processed code are in test/ folder.
There is an automation of compile steps in make.bat script. It is for use from command line as follows.
```bat
make -ag      & :: recompile attribute grammar(s)
make -ag -o   & :: recompile grammar(s), haskell modules and makes an executable
make          & :: by default makes an executable without recompiling grammar(s) only modules
```
