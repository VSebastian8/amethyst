#include "HsFFI.h"

void initialize_haskell()
{
    int number = 0;
    char *name[] = {"Haskell Parser Runtime"};
    char **name_ptr = name;
    hs_init(&number, &name_ptr);
}

void exit_haskell()
{
    hs_exit();
}

extern int *parse(const char *str);
extern int *result_type(const int *result);
extern char *return_error(const int *error);
