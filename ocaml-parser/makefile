main:
	corebuild -quiet -pkg core src/tokenizer.byte
	corebuild -quiet -pkg core src/colors.byte 
	# corebuild -quiet -pkg core src/parser.byte
	corebuild -quiet -pkg core src/prattParser.byte
	corebuild -quiet -pkg core src/cog.native
	corebuild -quiet -pkg core src/main.native
	rm *.byte
	echo "Compiled sucsessfully :)\n"

tests:
	corebuild -pkg core -I src src/testPrattParser.byte

run:
	./main.native -f first.cog
	./main.native -f recursive.cog
