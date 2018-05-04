all:
	stack install --profile

clean:
	stack clean

modgraph:
	graphmod -p src > modules.dot && dot -Tps modules.dot -o modules.ps 
