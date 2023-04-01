solution := 

guile := guile -L .

help :
	echo 'Run make ( new | tok )'

check-all : new tok

tok :
	$(guile) testtok.scm $(solution)

new :
	$(guile) test.scm $(solution)

.PHONY : help check-all guile
