#include <stdio.h>
#include "./ocaml_flint_utils.h"
#include "zarith.h"

void flint_stubs_utils_z_of_fmpz(fmpz *f, value* z)
{
    __mpz_struct *g;
    g = _fmpz_promote_val(f);
    *z = ml_z_from_mpz(g);
    _fmpz_demote_val(f);
}

void flint_stubs_utils_fmpz_of_z(fmpz *f, value z)
{
   mpz_t g;
   ml_z_mpz_init_set_z(g,z);
   fmpz_set_mpz(f,g);
   mpz_clear(g);
}

