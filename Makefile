build:
	./db/db_validate.sh ./db
	cabal build

run:
	cabal run
