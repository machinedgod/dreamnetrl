all:
	stack install --profile

clean:
	stack clean

modgraph:
	graphmod > modules.dot && dot -Tps modules.dot -o modules.ps 
