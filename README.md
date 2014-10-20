llvm-MemoryDependenceAnalysis
=============================

Story:
I want to use basicaa and memdep analysis for my own pass. 
I have tried "opt -basicaa -memdep -load myPass.so", but memory dependence analysis 
always generates its results by using no-aa. no-aa assumes all pointers are mayalias.

Content:
I extract codes for no-aa, basicaa and memdep from LLVM. 
Codes in runOnModule function from MemDepPrinter.cpp illustrate how 
to initialize and use these three passes. 



