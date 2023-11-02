##
## EPITECH PROJECT, 2023
## MAKEFILE
## File description:
## Wow, such make, much file!
##

include glados.env
export

NAME	=	glados

CC		=	ghc

BINPATH = $(shell stack path --local-install-root)/bin/

BINNAME = glados-exe

all:
			docker pull someone2love/glados_build_env:latest
			docker run --env-file glados.env --rm -v $(shell pwd):/glados -w /glados someone2love/glados_build_env:latest make build

build:
			stack --allow-different-user setup
			stack --allow-different-user build --copy-bins
			mv $(BINNAME) $(NAME)

default:	all

clean:
			stack --allow-different-user clean

fclean:		clean
			rm -f $(NAME)
			rm -drf ./doc
			docker rmi -f someone2love/glados_build_env:latest

tests: | unit_test functional_test

functional_test:
			python3 test/Functional.py

unit_test:
			@stack test --coverage
			@ln -sf "$(stack path --local-install-root)/hpc/index.html" report.html
			@echo -e "\e[0;32mhtml report created at report.html\e[0m"



re:			clean all

doc:		clean
			haddock ./src/* --html -o ./doc
