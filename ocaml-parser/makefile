main:
	corebuild -pkg core src/tokenizer.byte
	corebuild -pkg core src/colors.byte
	corebuild -pkg core src/parser.byte
	corebuild -pkg core src/main.native
test:
	./main.native -f first.cog
