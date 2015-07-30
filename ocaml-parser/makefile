main:
	corebuild -quiet -pkg core src/tokenizer.byte
	corebuild -quiet -pkg core src/colors.byte 
	corebuild -quiet -pkg core src/parser.byte
	corebuild -quiet -pkg core src/cog.native
	corebuild -quiet -pkg core src/main.native
	echo "Compiled sucsessfully :)\n"

test:
	./main.native -f first.cog
