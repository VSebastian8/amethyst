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
extern int *state_transition_i(const int *transitions, const int i);
extern void free_state(const int *state);
// Macro functions
extern int macro_type(const int *macro);
extern char *macro_string(const int *macro);
extern int macro_number(const int *macro);
extern int *macro_move(const int *macro);
extern char macro_symbol(const int *macro);
extern int macro_list_len(const int *macro);
extern int *macro_list(const int *macro);
extern char *macro_list_i(const int *macro, const int i);
// Machine functions
extern int machine_components_len(const int *machine);
extern int *machine_components(const int *machine);
extern char *machine_components_i_first(const int *components, const int i);
extern char *machine_components_i_second(const int *components, const int i);
extern int machine_states_len(const int *machine);
extern int *machine_states(const int *machine);
extern int *machine_states_i(const int *states, const int i);
// Automata functions
extern int automaton_type(const int *automaton);
extern char *automaton_name(const int *automaton);
extern int *automaton_machine(const int *automaton);
extern int *automaton_macro(const int *automaton);
extern void free_automaton(const int *automaton);
// Program | Error functions
extern int result_type(const int *result);
extern int result_program_len(const int *program);
extern int *program_automata(const int *program);
extern int *program_automata_i(const int *automata, const int i);
extern char *error_string(const int *error);
extern int error_line(const int *error);
extern int error_column(const int *error);
extern void free_result(const int *result);
// Test functions
extern int *test_transition(int n);
extern int *test_state(int n);
extern int *test_macro(int n);
extern int *test_machine(int n);
extern int *test_program(int n);
// Input String -> Syntax Tree
extern int *amethyst_parser(const char *code);
