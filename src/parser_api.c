#include "HsFFI.h"

// Haskell environment function
void initialize_haskell()
{
    int number = 0;
    char *name[] = {"Haskell Parser Runtime"};
    char **name_ptr = name;
    hs_init(&number, &name_ptr);
}
extern void clean_up();
void exit_haskell()
{
    clean_up();
    hs_exit();
}
// Move function
extern int move_type(const int *move);
// Transition functions
extern char transition_read_symbol(const int *transition);
extern char transition_write_symbol(const int *transition);
extern int *transition_move_symbol(const int *transition);
extern char *transition_new_state(const int *transition);
extern void free_transition(const int *transition);
extern int *test_transition(int n);
// Result functions
extern int *result_type(const int *result);
extern char *return_error(const int *error);
// String -> Syntax
extern int *parse(const char *str);
