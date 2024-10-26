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

extern int *createExpr(int a, int b);
extern int *createComplexExpr();
extern int evaluateExpr(int *expr);
extern int expressionType(int *expr);
extern int getNumber(int *expr);
extern int *getFirstAdd(int *expr);
extern int *getSecondAdd(int *expr);
