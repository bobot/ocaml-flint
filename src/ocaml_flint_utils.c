// gmp must be included before flint for some functions, such as
// _fmpz_promote_val, to be available; see e.g.
// https://flintlib.org/doc/fmpz.html#c._fmpz_promote
#include "gmp.h"
#include "flint/fmpz.h"
#include "flint/fmpz_poly.h"
#include "flint/acb.h"
#include "flint/ca.h"
#include "ctypes_cstubs_internals.h"
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

value flint_stubs_utils_fmpz_to_string(value vfmpz)
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

value flint_stubs_utils_fmpz_poly_to_string(value vfmpz)
{
  char* str = 0;
  size_t size;
  FILE* file = open_memstream(&str, &size);
  fmpz_poly_struct *f = CTYPES_ADDR_OF_FATPTR(vfmpz);
  fmpz_poly_fprint_pretty(file,f,"x");
  fclose(file);
  value r = caml_copy_string(str);
  free(str);
  return r;
}


value acb_stubs_utils_to_string(value vacb)
{
  char* str = 0;
  size_t size;
  FILE* file = open_memstream(&str, &size);
  acb_ptr f = CTYPES_ADDR_OF_FATPTR(vacb);
  acb_fprint(file,f);
  fclose(file);
  value r = caml_copy_string(str);
  free(str);
  return r;
}


value arf_stubs_utils_to_string(value varf)
{
  char* str = 0;
  size_t size;
  FILE* file = open_memstream(&str, &size);
  arf_ptr f = CTYPES_ADDR_OF_FATPTR(varf);
  arf_fprint(file,f);
  fclose(file);
  value r = caml_copy_string(str);
  free(str);
  return r;
}

value mag_stubs_utils_to_string(value vmag)
{
  char* str = 0;
  size_t size;
  FILE* file = open_memstream(&str, &size);
  mag_ptr f = CTYPES_ADDR_OF_FATPTR(vmag);
  mag_fprint(file,f);
  fclose(file);
  value r = caml_copy_string(str);
  free(str);
  return r;
}

value arb_stubs_utils_to_string(value varb)
{
  char* str = 0;
  size_t size;
  FILE* file = open_memstream(&str, &size);
  arb_ptr f = CTYPES_ADDR_OF_FATPTR(varb);
  arb_fprint(file,f);
  fclose(file);
  value r = caml_copy_string(str);
  free(str);
  return r;
}


value calcium_stubs_utils_to_string(value vca, value vctx)
{
  char* str = 0;
  size_t size;
  FILE* file = open_memstream(&str, &size);
  ca_struct *f = CTYPES_ADDR_OF_FATPTR(vca);
  ca_ctx_struct *ctx = CTYPES_ADDR_OF_FATPTR(vctx);
  ca_fprint(file,f,ctx);
  fclose(file);
  value r = caml_copy_string(str);
  free(str);
  return r;
}
