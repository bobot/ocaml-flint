#include "lib/ca.h"
#include "ctypes_cstubs_internals.h"
#include <stdio.h>

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
