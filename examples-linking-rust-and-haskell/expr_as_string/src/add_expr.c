#include "HsFFI.h"

void initialize_haskell()
{
    int number = 0;
    char *name[] = {"Haskell Runtime"};
    char **name_ptr = name;
    hs_init(&number, &name_ptr);
}

extern char *createExpr(int, int);

char *call_haskell_function(int a, int b)
{
    initialize_haskell();

    char *result = createExpr(a, b);

    hs_exit();

    return result;
}