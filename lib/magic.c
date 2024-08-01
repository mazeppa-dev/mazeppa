#include "caml/mlvalues.h"

// Taken from <https://stackoverflow.com/a/8974919/13166656>.
value address_of_value(value v) {
    return Val_long((unsigned long)v / sizeof(long));
}
