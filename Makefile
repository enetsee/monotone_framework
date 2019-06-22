test: 
	dune runtest

format:
	dune build @fmt --auto-promote

clean:
	dune clean

re: clean all