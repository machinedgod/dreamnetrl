FONT='xft:Square:style=Regular:size=9' 
EXECUTABLE='.stack-work/dist/x86_64-linux-tinfo6/Cabal-2.4.0.1/build/dreamnetrl/dreamnetrl'

#-------------------------------------------------------------------------------

all:
	@make -C res
	@stack install

clean:
	@make -C res clean
	@stack clean

graphmod:
	@graphmod -p src > modules.dot && dot -Tps modules.dot -o modules.ps 


run:
#	@urxvt --font ${FONT} -e ${EXECUTABLE}
	@urxvt --font ${FONT}

#------------------------------------------------------------------------------- 
