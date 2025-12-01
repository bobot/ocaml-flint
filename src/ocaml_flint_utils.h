// gmp must be included before flint for some functions, such as
// _fmpz_promote_val, to be available; see e.g.
// https://flintlib.org/doc/fmpz.html#c._fmpz_promote
#include "gmp.h"
#include "flint/fmpz.h"
#include "flint/fmpz_poly.h"
#include "flint/acb.h"
#include "flint/ca.h"
#include "caml/mlvalues.h"
#include "caml/memory.h"

void flint_stubs_utils_z_of_fmpz(fmpz *f, value* z);
void flint_stubs_utils_fmpz_of_z(fmpz *f, value z);

static inline void fmpq_to_q(fmpq *f,value* num,value* den){
    flint_stubs_utils_z_of_fmpz(&(f->num),num);
    flint_stubs_utils_z_of_fmpz(&(f->den),den);
}

static inline void create_fmpz_poly_from_fmpz(fmpz_poly_struct *poly,fmpz** coef, size_t len){
    fmpz_poly_realloc(poly,(mp_limb_signed_t)len);
    for(size_t i=0; i<len;i++){
        poly->coeffs[i] = *coef[i];
    };
    poly->length=(mp_limb_signed_t)len;
}

static inline void create_fmpz_poly_from_z(fmpz_poly_struct *poly,value arr){
    CAMLparam1(arr);
    size_t len = caml_array_length(arr);
    fmpz_poly_realloc(poly,(mp_limb_signed_t)len);
    for(size_t i=0; i<len;i++){
        flint_stubs_utils_fmpz_of_z(&(poly->coeffs[i]),Field(arr,i));
    };
    poly->length=(mp_limb_signed_t)len;
    CAMLreturn0;
}

static inline void get_coef_fmpz_poly_from_z(value * z, fmpz_poly_struct *poly, slong coef){
    flint_stubs_utils_z_of_fmpz(&(poly->coeffs[coef]),z);
}

static inline void qqbar_from_fmpz_poly(qqbar_t qqbar, fmpz_poly_t poly, acb_t enclosure, int*status){
    *status = _qqbar_validate_existence_uniqueness(&(qqbar->enclosure),poly,enclosure,QQBAR_DEFAULT_PREC);
    fmpz_poly_set(&(qqbar->poly),poly);
}

typedef struct { long int count; ca_ctx_struct ctx; } ca_ctx_ref_count;

static inline void ca_ctx_ref_count_init(ca_ctx_ref_count * ctx){
    ctx->count=1;
    ca_ctx_init(&ctx->ctx);
}

static inline void ca_ctx_ref_count_free(ca_ctx_ref_count * ctx){
    ctx->count-=1;
    if(ctx->count==0) ca_ctx_clear(&ctx->ctx);
}

static inline void ca_ctx_ref_count_incr(ca_ctx_ref_count * ctx){
    ctx->count+=1;
}

typedef struct { ca_ctx_ref_count *ctx; ca_struct *ca; } ca_with_ctx;

static inline void ca_with_ctx_get(ca_struct ** ca, ca_with_ctx *ca_with_ctx ){
    *ca = (ca_with_ctx->ca);
}

static inline void ca_with_ctx_init(ca_struct ** ca,ca_ctx_ref_count *ctx){
    *ca=malloc(sizeof(ca_struct));
    ca_init(*ca,&(ctx->ctx));
}

static inline void ca_with_ctx_set(ca_with_ctx *ca_with_ctx, ca_struct ** ca, ca_ctx_ref_count *ctx ){
    (ca_with_ctx->ca)=*ca;
    (ca_with_ctx->ctx)=ctx;
    ca_ctx_ref_count_incr(ctx);
}

static inline void ca_with_ctx_free(ca_with_ctx *ca_with_ctx){
    ca_clear(ca_with_ctx->ca,&(ca_with_ctx->ctx->ctx));
    ca_ctx_ref_count_free(ca_with_ctx->ctx);
}

static inline void qqbar_alloc_set(qqbar_struct **dst, qqbar_struct* src){
    *dst=malloc(sizeof(qqbar_struct));
    qqbar_init(*dst);
    qqbar_set(*dst,src);
}
