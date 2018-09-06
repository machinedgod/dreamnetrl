all:
	@make -C res
	@stack install

clean:
	@make -C res clean

modgraph:
	@graphmod -p src > modules.dot && dot -Tps modules.dot -o modules.ps 
