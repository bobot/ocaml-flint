#include "acb.h"
#include "ctypes_cstubs_internals.h"
#include <stdio.h>

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
