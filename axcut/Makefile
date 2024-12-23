build:
	@idris2 --build axcut.ipkg
	@idris2 source/Main.idr -o axcutc

test: build
	@build/exec/axcutc

clean:
	@rm -rf build
	@rm -f examples/*.{output,asm}
