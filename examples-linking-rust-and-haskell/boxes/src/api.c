#include "HsFFI.h"

int *generateList();

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
