#include "HsFFI.h"

int triple_input(int a);

void initialize_haskell()
{
    int number = 0;
    char *name[] = {"Haskell Runtime"};
    char **name_ptr = name;
    hs_init(&number, &name_ptr);
}

int use_triple(int a)
{
    initialize_haskell();

    int result = triple_input(a);

    hs_exit();

    return result;
}
