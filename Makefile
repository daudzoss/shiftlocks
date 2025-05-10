all : shiftlocks_p500.prg shiftlocks_c64.prg shiftlocks_c16.prg

shiftlocks_p500.prg : macrodef.inc gamerule.asm playeras.asm main.asm p500/header.inc
	64tass -a p500/header.inc main.asm --verbose-list -L shiftlocks_p500.lst -o shiftlocks_p500.prg

shiftlocks_c64.prg : macrodef.inc gamerule.asm playeras.asm main.asm c64/header.inc
	64tass -a c64/header.inc main.asm --verbose-list -L shiftlocks_c64.lst -o shiftlocks_c64.prg

shiftlocks_c16.prg : macrodef.inc gamerule.asm playeras.asm main.asm c16/header.inc
	64tass -a c16/header.inc main.asm --verbose-list -L shiftlocks_c16.lst -o shiftlocks_c16.prg

clean :
	rm -f *.prg *.lst
