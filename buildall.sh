#!/bin/sh
make clean
for gamerule in valley problem ; do
    for playeras in clrhomes wdodgson ; do
	case $gamerule in
	    valley) g=vf ;;
	    problem) g=fp ;;
	esac
	case $playeras in
	    clrhomes) p=sh ;;
	    wdodgson) p=wd ;;
	esac

	combo=${p}_${g}
	if [ -d "$combo" ] ; then
	    cd $combo
	else
	    mkdir $combo
	    cd $combo
	fi
	for x in Makefile c16 c64 ${gamerule}.asm macrodef.inc main.asm ${playeras}.asm ; do
	    if [ ! -e "$x" ] ; then
		ln -s ../$x .
	    fi
	done
	if [ ! -f "gamerule.asm" ] ; then
	    ln -s ${gamerule}.asm gamerule.asm
	fi
	if [ ! -f "playeras.asm" ] ; then
	    ln -s ${playeras}.asm playeras.asm
	fi
	echo "Building ${combo} for all platforms..."
	make clean
	make all || exit 1
	echo -n "...built ${combo} successfully at link"
	cd ..

	for target in c16 c64 ; do
	    case $target in
		c16) t=16 ;;
		c64) t=64 ;;
	    esac
	    prg=${combo}_${t}.prg

	    if [ ! -f "$prg" ] ; then
		ln -s ${combo}/shiftlocks_${target}.prg ${prg}
	    fi
	    echo -n " $prg"
	done
	echo
    done
done
exit 0
