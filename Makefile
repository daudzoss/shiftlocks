all : shiftlocks_p500.prg shiftlocks_c64.prg shiftlocks_c16.prg

shiftlocks_p500.prg : macrodef.inc gamerule.asm playeras.asm main.asm p500/header.inc
	rm -f header.inc; ln -s p500/header.inc; 64tass -a header.inc main.asm --verbose-list -L shiftlocks_p500.lst -o shiftlocks_p500.prg

shiftlocks_c64.prg : macrodef.inc gamerule.asm playeras.asm main.asm c64/header.inc
	rm -f header.inc; ln -s c64/header.inc; 64tass -a header.inc main.asm --verbose-list -L shiftlocks_c64.lst -o shiftlocks_c64.prg

shiftlocks_c16.prg : macrodef.inc gamerule.asm playeras.asm main.asm c16/header.inc
	rm -f header.inc; ln -s c16/header.inc; 64tass -a header.inc main.asm --verbose-list -L shiftlocks_c16.lst -o shiftlocks_c16.prg

clean :
	rm -f *.prg *.lst
