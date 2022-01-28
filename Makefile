.PHONY: default clean distclean 

CXX=c++
LLVMCONFIG=llvm-config-10
CXXFLAGS=-Wall -std=c++11 `$(LLVMCONFIG) --cxxflags` -frtti -O3
LDFLAGS=`$(LLVMCONFIG) --ldflags --system-libs --libs all`
LIBLLAMAFLAG=-DLIBLLAMA="./libllama/lib.a"
LLVMXTRACPPFLAGS=$(CXXFLAGS) $(LIBGCFLAG)
OPTIONCPPFLAGS=$(CXXFLAGS) $(LIBLLAMAFLAG) $(LIBGCFLAG)

default: all

lexer.cpp: lexer.l
	flex -s -o lexer.cpp lexer.l

parser.hpp parser.cpp: parser.y
	bison -dv --debug -Wall -o parser.cpp parser.y


lexer.o: lexer.cpp lexer.hpp parser.hpp ast.hpp
parser.o: parser.cpp lexer.hpp ast.hpp parser.hpp options.hpp
ast.o: ast.cpp lexer.hpp ast.hpp
symbols.o: symbols.cpp symbols.hpp
types.o: types.cpp types.hpp ast.hpp
type_inference.o: type_inference.cpp type_inference.hpp types.hpp
llvmExtra.o: llvmExtra.cpp ast.hpp type_inference.hpp parser.hpp
	$(CXX) $(LLVMXTRACPPFLAGS) -c -o llvmExtra.o llvmExtra.cpp $(LDFLAGS)
options.o: options.cpp ast.hpp type_inference.hpp symbols.hpp
	$(CXX) $(OPTIONCPPFLAGS) -c -o options.o options.cpp $(LDFLAGS)

all: lib compiler

compiler: lexer.o parser.o symbols.o types.o ast.o type_inference.o llvmExtra.o options.o
	$(CXX) $(CXXFLAGS) -o llama $^ $(LDFLAGS)

lib:
	cd libllama && ./do.sh &&	cd ..

clean:
	$(RM) lexer.cpp parser.cpp parser.hpp parser.output *.o *.ll *.s *.asm *.out llama && \
	cd ../examples && \
	$(RM) *.asm *.imm
	cd ../final

cleanliba:
	cd libllama && \
	make -s clean -C auxil  && \
	rm auxil/auxil.a && \
	make -s clean -C math && \
	rm math/math.a && \
	make -s clean -C stdio && \
	rm stdio/stdio.a && \
	make -s clean -C stdlib && \
	rm stdlib/stdlib.a && \
	make -s clean -C string && \
	rm string/string.a && \
	make -s clean -C new && \
	rm new/reps.a && \
	rm lib.a && \
	cd ..

distclean: clean cleanliba
	$(RM) llama
