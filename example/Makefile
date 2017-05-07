AS65 := ca65
LD65 := ld65
CFLAGS65 := --cpu 6502X

map.txt example.nes: nrom128.cfg example.o
	$(LD65) -o example.nes -m map.txt -C $^

example.o: example.s north.inc example.inc
	$(AS65) $(CFLAGS65) $< -o $@

example.inc: example.north
	../dist/build/north/north $^ > /tmp/compiled_north
	mv /tmp/compiled_north $@
