build:
	./db/db_validate.sh ./db
	stack build
	
run:
	stack exec haskell-plants-exe
