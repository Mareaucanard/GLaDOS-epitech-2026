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
			stack build
			cp $(BINPATH)$(BINNAME) ./$(NAME)

default:	all

clean:
			stack clean

fclean:		clean
			rm -f $(NAME)

re:			clean all
