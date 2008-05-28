/*
 * wire_stubs.c
 * ------------
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

value caml_native_byte_order(value unit) {
#ifdef ARCH_BIG_ENDIAN
  return Val_int(1);
#else
  return Val_int(0);
#endif
}

value caml_string_match(value buffer, value i, value str, value len) {
  return Val_int(memcmp(String_val(buffer) + Int_val(i), String_val(str), Int_val(len)) == 0 ? Val_true : Val_false);
}

value
caml_writer_le_int_int16 (value buffer, value i, value x)
{
  write16 (Int_val (i), le16 (Int_val (x)));
  return Val_unit;
}

value
caml_writer_be_int_int16 (value buffer, value i, value x)
{
  write16 (Int_val (i), be16 (Int_val (x)));
  return Val_unit;
}

value
caml_writer_le_int_uint16 (value buffer, value i, value x)
{
  write16 (Int_val (i), le16 (Int_val (x)));
  return Val_unit;
}

value
caml_writer_be_int_uint16 (value buffer, value i, value x)
{
  write16 (Int_val (i), be16 (Int_val (x)));
  return Val_unit;
}

value
caml_reader_le_int_int16 (value buffer, value i)
{
  return Val_int (le16 (read16));
}

value
caml_reader_be_int_int16 (value buffer, value i)
{
  return Val_int (be16 (read16));
}

value
caml_reader_le_int_uint16 (value buffer, value i)
{
  return Val_int (le16 (read16));
}

value
caml_reader_be_int_uint16 (value buffer, value i)
{
  return Val_int (be16 (read16));
}

value
caml_writer_le_int_int32 (value buffer, value i, value x)
{
  write32 (Int_val (i), le32 (Int_val (x)));
  return Val_unit;
}

value
caml_writer_be_int_int32 (value buffer, value i, value x)
{
  write32 (Int_val (i), be32 (Int_val (x)));
  return Val_unit;
}

value
caml_writer_le_int_uint32 (value buffer, value i, value x)
{
  write32 (Int_val (i), le32 (Int_val (x)));
  return Val_unit;
}

value
caml_writer_be_int_uint32 (value buffer, value i, value x)
{
  write32 (Int_val (i), be32 (Int_val (x)));
  return Val_unit;
}

value
caml_reader_le_int_int32 (value buffer, value i)
{
  return Val_int (le32 (read32));
}

value
caml_reader_be_int_int32 (value buffer, value i)
{
  return Val_int (be32 (read32));
}

value
caml_reader_le_int_uint32 (value buffer, value i)
{
  return Val_int (le32 (read32));
}

value
caml_reader_be_int_uint32 (value buffer, value i)
{
  return Val_int (be32 (read32));
}

value
caml_writer_le_int_int64 (value buffer, value i, value x)
{
  write64 (Int_val (i), le64 (Int_val (x)));
  return Val_unit;
}

value
caml_writer_be_int_int64 (value buffer, value i, value x)
{
  write64 (Int_val (i), be64 (Int_val (x)));
  return Val_unit;
}

value
caml_writer_le_int_uint64 (value buffer, value i, value x)
{
  write64 (Int_val (i), le64 (Int_val (x)));
  return Val_unit;
}

value
caml_writer_be_int_uint64 (value buffer, value i, value x)
{
  write64 (Int_val (i), be64 (Int_val (x)));
  return Val_unit;
}

value
caml_reader_le_int_int64 (value buffer, value i)
{
  return Val_int (le64 (read64));
}

value
caml_reader_be_int_int64 (value buffer, value i)
{
  return Val_int (be64 (read64));
}

value
caml_reader_le_int_uint64 (value buffer, value i)
{
  return Val_int (le64 (read64));
}

value
caml_reader_be_int_uint64 (value buffer, value i)
{
  return Val_int (be64 (read64));
}

value
caml_writer_le_int32_int32 (value buffer, value i, value x)
{
  write32 (Int_val (i), le32 (Int32_val (x)));
  return Val_unit;
}

value
caml_writer_be_int32_int32 (value buffer, value i, value x)
{
  write32 (Int_val (i), be32 (Int32_val (x)));
  return Val_unit;
}

value
caml_writer_le_int32_uint32 (value buffer, value i, value x)
{
  write32 (Int_val (i), le32 (Int32_val (x)));
  return Val_unit;
}

value
caml_writer_be_int32_uint32 (value buffer, value i, value x)
{
  write32 (Int_val (i), be32 (Int32_val (x)));
  return Val_unit;
}

value
caml_reader_le_int32_int32 (value buffer, value i)
{
  CAMLparam2 (buffer, i);
  CAMLreturn (caml_copy_int32 (le32 (read32)));
}

value
caml_reader_be_int32_int32 (value buffer, value i)
{
  CAMLparam2 (buffer, i);
  CAMLreturn (caml_copy_int32 (be32 (read32)));
}

value
caml_reader_le_int32_uint32 (value buffer, value i)
{
  CAMLparam2 (buffer, i);
  CAMLreturn (caml_copy_int32 (le32 (read32)));
}

value
caml_reader_be_int32_uint32 (value buffer, value i)
{
  CAMLparam2 (buffer, i);
  CAMLreturn (caml_copy_int32 (be32 (read32)));
}

value
caml_writer_le_int64_int64 (value buffer, value i, value x)
{
  write64 (Int_val (i), le64 (Int64_val (x)));
  return Val_unit;
}

value
caml_writer_be_int64_int64 (value buffer, value i, value x)
{
  write64 (Int_val (i), be64 (Int64_val (x)));
  return Val_unit;
}

value
caml_writer_le_int64_uint64 (value buffer, value i, value x)
{
  write64 (Int_val (i), le64 (Int64_val (x)));
  return Val_unit;
}

value
caml_writer_be_int64_uint64 (value buffer, value i, value x)
{
  write64 (Int_val (i), be64 (Int64_val (x)));
  return Val_unit;
}

value
caml_reader_le_int64_int64 (value buffer, value i)
{
  CAMLparam2 (buffer, i);
  CAMLreturn (caml_copy_int64 (le64 (read64)));
}

value
caml_reader_be_int64_int64 (value buffer, value i)
{
  CAMLparam2 (buffer, i);
  CAMLreturn (caml_copy_int64 (be64 (read64)));
}

value
caml_reader_le_int64_uint64 (value buffer, value i)
{
  CAMLparam2 (buffer, i);
  CAMLreturn (caml_copy_int64 (le64 (read64)));
}

value
caml_reader_be_int64_uint64 (value buffer, value i)
{
  CAMLparam2 (buffer, i);
  CAMLreturn (caml_copy_int64 (be64 (read64)));
}

value
caml_writer_le_float_double (value buffer, value i, value x)
{
  write64 (Int_val (i), le64 (Double_val (x)));
  return Val_unit;
}

value
caml_writer_be_float_double (value buffer, value i, value x)
{
  write64 (Int_val (i), be64 (Double_val (x)));
  return Val_unit;
}

value
caml_reader_le_float_double (value buffer, value i)
{
  CAMLparam2 (buffer, i);
  CAMLreturn (caml_copy_double (le64 (read64)));
}

value
caml_reader_be_float_double (value buffer, value i)
{
  CAMLparam2 (buffer, i);
  CAMLreturn (caml_copy_double (be64 (read64)));
}
