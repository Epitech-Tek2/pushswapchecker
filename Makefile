##
## EPITECH PROJECT, 2021
## B-CPP-300-STG-3-1-CPPD10-clement.muth
## File description:
## Makefile
##

CC	=	ghc

SRC	=	pushswap_checker.hs

EXEC	=	pushswap_checker

all:    $(EXEC)

$(EXEC):	$(OBJ)
	$(CC) -o $(EXEC) $(SRC)

tests_run:
	ghc -o unit_tests tests/Tests.hs
	- ./unit_tests

clean:
	rm -rf *.hi *.o

fclean: clean
	rm -rf $(EXEC)

re:	fclean all

.PHONY: clean fclean re all