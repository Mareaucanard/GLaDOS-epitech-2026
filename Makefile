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
			docker pull someone2love/glados_build_env:latest
			docker run --rm -v $(shell pwd):/glados -w /glados someone2love/glados_build_env:latest make build

build:
			stack setup --allow-different-user
			stack build --copy-bins --allow-different-user
			mv $(BINNAME) $(NAME)

default:	all

clean:
			stack clean

fclean:		clean
			rm -f $(NAME)
			docker rmi -f someone2love/glados_build_env:latest

tests:
			stack test --coverage

re:			clean all
