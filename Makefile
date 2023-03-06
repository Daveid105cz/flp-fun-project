COMPILER = ghc
SRCS = src/Main.hs src/ParseInput.hs src/CurveMath.hs src/Types.hs src/Ecdsa.hs

OUTPUTE = flp22-fun
GHC_FLAGS = -Wall

all: $(OUTPUTE)

$(OUTPUTE): $(SRCS)
	$(COMPILER) $(GHC_FLAGS) -o $(OUTPUTE) $(SRCS)

clean:
	rm -f $(OUTPUTE) src/*.o src/*.hi

run: $(OUTPUTE)
	./$(OUTPUTE) -i test/test01.in
