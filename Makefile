all:
	make -C res
	stack install --flag ncurses:force-narrow-library

repl:
	make -C res
	stack repl --flag ncurses:force-narrow-library

clean:
	stack clean

modgraph:
	graphmod -p src > modules.dot && dot -Tps modules.dot -o modules.ps 
