#include "HsFFI.h"

extern char *reverseString(const char *);

void initialize_haskell()
{
    int number = 0;
    char *name[] = {"Haskell Runtime"};
    char **name_ptr = name;
    hs_init(&number, &name_ptr);
}

char *call_haskell_function(const char *input)
{
    initialize_haskell();

    char *result = reverseString(input);

    hs_exit();

    return result;
}