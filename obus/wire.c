/*
 * wire.c
 * ------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 */

#include <sys/types.h>
#include <string.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>

#define write8(p, x) *((u_int8_t *)(String_val(buffer) + p)) = (u_int8_t) x
#define write16(p, x) *((u_int16_t *)(String_val(buffer) + p)) = (u_int16_t) x
#define write32(p, x) *((u_int32_t *)(String_val(buffer) + p)) = (u_int32_t) x
#define write64(p, x) *((u_int64_t *)(String_val(buffer)+ p)) = (u_int64_t) x

#define read8 (*((u_int8_t *)(String_val(buffer) + Int_val(i))))
#define read16 (*((u_int16_t *)(String_val(buffer) + Int_val(i))))
#define read32 (*((u_int32_t *)(String_val(buffer) + Int_val(i))))
#define read64 (*((u_int64_t *)(String_val(buffer)+ Int_val(i))))

#define swap16(val)                                                     \
  ((u_int16_t)                                                          \
   ((u_int16_t) ((u_int16_t) (val) >> 8) |                              \
    (u_int16_t) ((u_int16_t) (val) << 8)))

#define swap32(val)                                                     \
  ((u_int32_t)                                                          \
   ((((u_int32_t) (val) & (u_int32_t) 0x000000ffU) << 24) |             \
    (((u_int32_t) (val) & (u_int32_t) 0x0000ff00U) <<  8) |             \
    (((u_int32_t) (val) & (u_int32_t) 0x00ff0000U) >>  8) |             \
    (((u_int32_t) (val) & (u_int32_t) 0xff000000U) >> 24)))

#define swap64(val)                                                     \
  ((u_int64_t)                                                          \
   (((u_int64_t) (val) &                                                \
     (u_int64_t) 0x00000000000000ffULL) << 56) |                        \
   (((u_int64_t) (val) &                                                \
     (u_int64_t) 0x000000000000ff00ULL) << 40) |                        \
   (((u_int64_t) (val) &                                                \
     (u_int64_t) 0x0000000000ff0000ULL) << 24) |                        \
   (((u_int64_t) (val) &                                                \
     (u_int64_t) 0x00000000ff000000ULL) <<  8) |                        \
   (((u_int64_t) (val) &                                                \
     (u_int64_t) 0x000000ff00000000ULL) >>  8) |                        \
   (((u_int64_t) (val) &                                                \
     (u_int64_t) 0x0000ff0000000000ULL) >> 24) |                        \
   (((u_int64_t) (val) &                                                \
     (u_int64_t) 0x00ff000000000000ULL) >> 40) |                        \
   (((u_int64_t) (val) &                                                \
     (u_int64_t) 0xff00000000000000ULL) >> 56))

#ifdef ARCH_BIG_ENDIAN
#  define le16(x) swap16(x)
#  define le32(x) swap32(x)
#  define le64(x) swap64(x)
#  define be16(x) (x)
#  define be32(x) (x)
#  define be64(x) (x)
#else
#  define le16(x) (x)
#  define le32(x) (x)
#  define le64(x) (x)
#  define be16(x) swap16(x)
#  define be32(x) swap32(x)
#  define be64(x) swap64(x)
#endif

#define _writer(bo, bit, dbust, camlt, macro)                      \
  value caml_writer_##bo##_##camlt##_##dbust(value buffer, value i, value x) \
  {                                                                     \
    write##bit(Int_val(i), bo##bit(macro##_val(x)));                    \
    return Val_unit;                                                    \
  }

#define writer(bit, dbust, camlt, macro)                      \
  _writer(le, bit, dbust, camlt, macro)                       \
  _writer(be, bit, dbust, camlt, macro)                       \
  _writer(le, bit, u##dbust, camlt, macro)                    \
  _writer(be, bit, u##dbust, camlt, macro)

#define _reader_unboxed(bo, bit, dbust, camlt, macro)                   \
  value caml_reader_##bo##_##camlt##_##dbust(value buffer, value i)     \
  {                                                                     \
    return Val_##camlt(bo##bit(read##bit));                             \
  }

#define _reader_boxed(bo, bit, dbust, camlt, macro)                     \
  value caml_reader_##bo##_##camlt##_##dbust(value buffer, value i)     \
  {                                                                     \
    CAMLparam2(buffer, i);                                              \
    CAMLreturn(caml_copy_##macro(bo##bit(read##bit)));                  \
  }

#define reader(boxed, bit, dbust, camlt)                      \
  _reader_##boxed(le, bit, dbust, camlt, camlt)               \
  _reader_##boxed(be, bit, dbust, camlt, camlt)               \
  _reader_##boxed(le, bit, u##dbust, camlt, camlt)            \
  _reader_##boxed(be, bit, u##dbust, camlt, camlt)

#define boxed(bit)                                      \
  writer(bit, int##bit, int##bit, Int##bit)             \
  reader(boxed, bit, int##bit, int##bit)

#define unboxed(bit)                            \
  writer(bit, int##bit, int, Int)               \
  reader(unboxed, bit, int##bit, int)

unboxed(16)
unboxed(32)
unboxed(64)
boxed(32)
boxed(64)
_writer(le, 64, double, float, Double)
_writer(be, 64, double, float, Double)
_reader_boxed(le, 64, double, float, double)
_reader_boxed(be, 64, double, float, double)

value caml_native_byte_order(value unit) {
#ifdef ARCH_BIG_ENDIAN
  return Val_int(1);
#else
  return Val_int(0);
#endif
}

value caml_pad2(value buffer, value i) {
  register int p = Int_val(i);
  if ((p & 1) == 0) {
    return Val_int(p);
  } else {
    write8(p, 0);
    return Val_int(p + 1);
  }
}

value caml_pad4(value buffer, value i) {
  register int p = Int_val(i);
  switch (p & 3) {
  case 0:
    return Val_int(p);
  case 1:
    write8(p, 0);
    write16(p+1, 0);
    return Val_int(p + 3);
  case 2:
    write16(p, 0);
    return Val_int(p + 2);
  default:
    write8(p, 0);
    return Val_int(p + 1);
  }
}

value caml_pad8(value buffer, value i) {
  register int p = Int_val(i);
  switch (p & 7) {
  case 0:
    return Val_int(p);
  case 1:
    write8(p, 0);
    write16(p+1, 0);
    write32(p+3, 0);
    return Val_int(p + 7);
  case 2:
    write16(p, 0);
    write32(p+2, 0);
    return Val_int(p + 6);
  case 3:
    write8(p, 0);
    write32(p+1, 0);
    return Val_int(p + 5);
  case 4:
    write32(p, 0);
    return Val_int(p + 4);
  case 5:
    write8(p, 0);
    write16(p+1, 0);
    return Val_int(p + 3);
  case 6:
    write16(p, 0);
    return Val_int(p + 2);
  default:
    write8(p, 0);
    return Val_int(p + 1);
  }
}

value caml_zero7(value buffer, value i) {
  register int p = Int_val(i);
  write8(p, 0);
  write16(p+1, 0);
  write32(p+3, 0);
  return Val_unit;
}

value caml_zero6(value buffer, value i) {
  register int p = Int_val(i);
  write16(p, 0);
  write32(p+2, 0);
  return Val_unit;
}

value caml_zero5(value buffer, value i) {
  register int p = Int_val(i);
  write8(p, 0);
  write32(p+1, 0);
  return Val_unit;
}

value caml_zero4(value buffer, value i) {
  write32(Int_val(i), 0);
  return Val_unit;
}

value caml_zero3(value buffer, value i) {
  register int p = Int_val(i);
  write8(p, 0);
  write16(p+1, 0);
  return Val_unit;
}

value caml_zero2(value buffer, value i) {
  write16(Int_val(i), 0);
  return Val_unit;
}

value caml_string_match(value buffer, value i, value str, value len) {
  return Val_int(memcmp(String_val(buffer) + Int_val(i), String_val(str), Int_val(len)) == 0 ? Val_true : Val_false);
}
