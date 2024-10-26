#include "HsFFI.h"

void initialize_haskell()
{
    int number = 0;
    char *name[] = {"Haskell Runtime"};
    char **name_ptr = name;
    hs_init(&number, &name_ptr);
}

void exit_haskell()
{
    hs_exit();
}

extern int *createShape(int n);
extern int isSquare(int *shape);
extern int getSide(int *shape);
extern int getWidth(int *shape);
extern int getLength(int *shape);
