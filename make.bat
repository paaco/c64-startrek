@echo off
mkdir .cache
acme -v -f cbm -o .cache/startrek.prg startrek.asm
acme -v -f cbm -o .cache/loader.prg loader.asm
