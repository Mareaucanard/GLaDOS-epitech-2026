##
## EPITECH PROJECT, 2023
## MAKEFILE
## File description:
## Wow, such make, much file!
##

NAME	=	glados

CC		=	ghc

BINPATH = $(shell stack path --local-install-root)/bin/

BINNAME = glados-exe

all:
			stack setup --allow-different-user
			stack build --copy-bins --allow-different-user
			mv $(BINNAME) $(NAME)

default:	all

clean:
			stack clean

fclean:		clean
			rm -f $(NAME)

tests:
			stack test

re:			clean all
