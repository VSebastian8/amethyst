#include "HsFFI.h"
#include <stdbool.h>
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
// State functions
extern int state_type(const int *state);
extern char *state_name(const int *state);
extern bool state_is_initial(const int *state);
extern int state_tr_len(const int *state);
extern int *state_transitions(const int *state);
extern int *state_transition_i(const int *transitions, int i);
extern void free_state(const int *state);
// Test functions
extern int *test_transition(int n);
extern int *test_state(int n);
// Result functions
extern int *result_type(const int *result);
extern char *return_error(const int *error);
// String -> Syntax
extern int *parse(const char *str);
