for /L %%i in (1,1,5) do scala -classpath ..\bin Main -testlexer ..\testcases\%%i.txt > ..\lexersol\%%i.txt