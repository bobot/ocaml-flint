// gmp must be included before flint for some functions, such as
// _fmpz_promote_val, to be available; see e.g.
// https://flintlib.org/doc/fmpz.html#c._fmpz_promote
#include <gmp.h>
#include <flint/fmpz.h>
#include <flint/fmpz_poly.h>
#include <flint/fmpz_mat.h>
#include <flint/fmpz_poly_factor.h>
#include <flint/arb_fmpz_poly.h>
#include <flint/acb.h>
#include <flint/ca.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>

void flint_stubs_utils_z_of_fmpz(fmpz *f, value* z);
void flint_stubs_utils_fmpz_of_z(fmpz *f, value z);

static inline void fmpq_to_q(fmpq *f,value* num,value* den){
    flint_stubs_utils_z_of_fmpz(&(f->num),num);
    flint_stubs_utils_z_of_fmpz(&(f->den),den);
}

static inline void create_fmpz_poly_from_fmpz(fmpz_poly_struct *poly,fmpz** coef, size_t len){
    fmpz_poly_realloc(poly,(mp_limb_signed_t)len);
    for(size_t i=0; i<len;i++){
        fmpz_set(&(poly->coeffs[i]),coef[i]);
    };
    poly->length=(mp_limb_signed_t)len;
    _fmpz_poly_normalise(poly);
}

static inline void create_fmpz_poly_from_z(fmpz_poly_struct *poly,value arr){
    CAMLparam1(arr);
    size_t len = caml_array_length(arr);
    fmpz_poly_realloc(poly,(mp_limb_signed_t)len);
    for(size_t i=0; i<len;i++){
        flint_stubs_utils_fmpz_of_z(&(poly->coeffs[i]),Field(arr,i));
    };
    poly->length=(mp_limb_signed_t)len;
    _fmpz_poly_normalise(poly);
    CAMLreturn0;
}

static inline void get_coef_fmpz_poly_from_z(value * z, fmpz_poly_struct *poly, slong coef){
    if (coef < 0 || coef >= poly->length) {
        /** todo direct */
        fmpz_t zero;
        fmpz_init(zero);
        flint_stubs_utils_z_of_fmpz(zero, z);
        fmpz_clear(zero);
    } else {
        flint_stubs_utils_z_of_fmpz(&(poly->coeffs[coef]), z);
    }
}

static inline void qqbar_from_fmpz_poly(qqbar_t qqbar, fmpz_poly_t poly, acb_t enclosure, int*status){
    *status = _qqbar_validate_existence_uniqueness(&(qqbar->enclosure),poly,enclosure,QQBAR_DEFAULT_PREC);
    fmpz_poly_set(&(qqbar->poly),poly);
}

static inline void fmpz_mat_get_entry_wrapper(fmpz *res, fmpz_mat_struct *m, slong i, slong j) {
    fmpz_set(res, fmpz_mat_entry(m, i, j));
}

static inline void fmpz_mat_set_entry_wrapper(fmpz_mat_struct *m, slong i, slong j, fmpz *v) {
    fmpz_set(fmpz_mat_entry(m, i, j), v);
}

static inline fmpz_poly_struct* fmpz_poly_factor_get_factor(fmpz_poly_factor_struct *fac, slong i) {
    return &(fac->p[i]);
}

static inline int fmpz_poly_factor_get_exp(fmpz_poly_factor_struct *fac, slong i) {
    return fac->exp[i];
}

static inline void fmpz_poly_factor_content(fmpz *c, fmpz_poly_factor_struct *fac) {
    fmpz_set(c, &(fac->c));
}

static inline int fmpz_poly_factor_length(fmpz_poly_factor_struct *fac) {
    return fac->num;
}

/**
 * Extracts the matrix pointer from an OCaml value.
 *
 * If `v` is a custom block (Tag_custom), it directly returns the value.
 * If `v` is a pair (Tag_tuple / Is_block), it extracts the first element (Field(v, 0))
 *
 * The result is alive if v stays alive
 */
static inline value extract_mat_in_value(value v) {
    if (Tag_val(v) == Custom_tag) {
        // Direct matrix (custom block)
        return v;
    } else {
        // Window matrix (Pair: Field(v, 0) is the window custom block)
        return Field(v, 0);
    }
}
