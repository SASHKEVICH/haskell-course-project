build:
	./db/db_validate.sh ./db
	ghc -o main.bin main.hs
