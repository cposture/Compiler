objects=main.o Unit1.o
CXXFLAGS=-std=c++11 -g
compiler:$(objects)
	g++ -std=c++11 -g $(objects) -o compiler
main.o:main.cpp Unit1.h
	g++ -c -g -std=c++11 main.cpp -o main.o
Unit1.o:Unit1.cpp Unit1.h
	g++ -c -g -std=c++11 Unit1.cpp -o Unit1.o
.PHONY:clean
clean:
	-rm compiler main.o Unit1.o
