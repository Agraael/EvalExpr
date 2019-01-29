##
## EPITECH PROJECT, 2018
## Functionelle
## File description:
## Makefile To compile Stack
##

STACK_BUILD	=	stack build

NAME		=	funEvalExpr

all: $(NAME)

$(NAME):
	$(STACK_BUILD)
	stack install --local-bin-path .
	mv evalexprProject-exe ../$(NAME)
clean:
	rm ../$(NAME)

fclean:
	rm ../$(NAME)

re: fclean all

.PHONY: all clean fclean re
