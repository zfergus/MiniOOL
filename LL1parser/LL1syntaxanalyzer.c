#include <stdlib.h>
#include <stdio.h>

int stop (char *M) {
  printf("\nSyntax error: %s\n", M);
  exit(EXIT_FAILURE);
}

char CC;

void readChar (char *C){
  while ((*C = getchar()) == ' ');
}

void T (void){
  if ((CC == 'a') || (CC == 'b'))
    readChar(&CC);
  else stop("'a' or 'b' expected.");
}

void S (void) {
  if ((CC == 'a') || (CC == 'b'))
      { T() ; S() ; }
  else if (CC == '.') ;
  else stop("'a', 'b' or '.' expected.");
}

void P (void) {
  S();
  if (CC == '.') ;
  else stop("'.' expected.");
}

int main (void) {
  readChar(&CC);
  P();
  printf("\nCorrect sentence.\n");
  exit(EXIT_SUCCESS); }
