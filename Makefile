COMPILER = ghc
SRCS = src/Main.hs src/ParseInput.hs src/Types.hs

OUTPUTE = flp22-fun
GHC_FLAGS = -Wall

all: $(OUTPUTE)

$(OUTPUTE): $(SRCS)
	$(COMPILER) $(GHC_FLAGS) -o $(OUTPUTE) $(SRCS)

clean:
	rm -f $(OUTPUTE) src/*.o src/*.hi

run: $(OUTPUTE)
	./$(OUTPUTE) -i test/test01.in
