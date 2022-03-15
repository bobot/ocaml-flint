#include "flint/fmpz.h"
#include "ctypes_cstubs_internals.h"
#include "gmp.h"
#include "zarith.h"
#include <stdio.h>

value flint_stubs_utils_z_of_fmpz(value vfmpz)
{
    __mpz_struct *g;
    fmpz *f = CTYPES_ADDR_OF_FATPTR(vfmpz);
    g = _fmpz_promote_val(f);
    value z = ml_z_from_mpz(g);
    _fmpz_demote_val(f);
   return z;
}

value flint_stubs_utils_fmpz_of_z(value vfmpz, value z)
{
   fmpz *f = CTYPES_ADDR_OF_FATPTR(vfmpz);
   mpz_t g;
   ml_z_mpz_init_set_z(g,z);
   fmpz_set_mpz(f,g);
   mpz_clear(g);
   return Val_unit;
}

value flint_stubs_utils_to_string(value vfmpz)
{
  char* str = 0;
  size_t size;
  FILE* file = open_memstream(&str, &size);
  fmpz *f = CTYPES_ADDR_OF_FATPTR(vfmpz);
  fmpz_fprint(file,f);
  fclose(file);
  value r = caml_copy_string(str);
  free(str);
  return r;
}
