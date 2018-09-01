#include "sha2.h"
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>

CAMLprim value caml_sha256(value message)
{
  CAMLparam1(message);
  value digest = alloc_string(32);
  sha256((const unsigned char *) (String_val(message)),
         caml_string_length(message),
         ((unsigned char *) String_val(digest)));
  CAMLreturn(digest);
}

CAMLprim value caml_sha384(value message)
{
  CAMLparam1(message);
  value digest = alloc_string(48);
  sha384((const unsigned char *) (String_val(message)),
         caml_string_length(message),
         ((unsigned char *) String_val(digest)));
  CAMLreturn(digest);
}

CAMLprim value caml_sha512(value message)
{
  CAMLparam1(message);
  value digest = alloc_string(64);
  sha512((const unsigned char *) (String_val(message)),
         caml_string_length(message),
         ((unsigned char *) String_val(digest)));
  CAMLreturn(digest);
}
