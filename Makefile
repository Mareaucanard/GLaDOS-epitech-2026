##
## EPITECH PROJECT, 2023
## MAKEFILE
## File description:
## Wow, such make, much file!
##

include glados.env

NAME	=	glados

CC		=	ghc

BINPATH = $(shell stack path --local-install-root)/bin/

BINNAME = glados-exe

all:
			docker pull someone2love/glados_build_env:latest
			docker run --env-file glados.env --rm -v $(shell pwd):/glados -w /glados someone2love/glados_build_env:latest make build

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

tests: | unit_test functional_test

functional_test:
			python3 test/Functional.py

unit_test:
			@stack test --coverage
			@ln -sf /home/mareau/Programs/semestre_5/glados/.stack-work/install/x86_64-linux/5659563df3a50a00a90044575e65fadb5a715e0b372ebe388e5567a46d60cb70/9.4.7/hpc/index.html report.html
			@echo -e "\e[0;32mhtml report created at report.html\e[0m"


re:			clean all
