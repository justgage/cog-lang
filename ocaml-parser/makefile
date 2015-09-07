main:
	corebuild -quiet -pkg core src/tokenizer.byte
	corebuild -quiet -pkg core src/colors.byte
	corebuild -quiet -pkg core src/prattParser.byte
	corebuild -quiet -pkg core src/eval.native
	corebuild -quiet -pkg core src/cog.native
	corebuild -quiet -pkg core src/main.native
	rm *.byte
	$(info Compiled sucsessfully)

tests:
	$(info -- Building tests...\n)
	corebuild -quiet -pkg core src/testPrattParser.native
	corebuild -quiet -pkg core src/testCog.native

	$(info -- Running tests...\n)

	$(info Pratt tests \n)
	./testPrattParser.native

	$(info Cog tests \n)
	./testCog.native

run:
	./main.native -f first.cog
	./main.native -f recursive.cog
