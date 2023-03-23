solution := 

guile := guile -L .

help :
	echo 'Run make (old | new)'

check-all : new

new :
	$(guile) test.scm $(solution)

old :
	$(guile) oldtest.scm $(solution)
.PHONY : help check-all guile
