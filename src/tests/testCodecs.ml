(******************************************************************************
 * capnp-ocaml
 *
 * Copyright (c) 2013-2014, Paul Pelzl
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 *  1. Redistributions of source code must retain the above copyright notice,
 *     this list of conditions and the following disclaimer.
 *
 *  2. Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimer in the
 *     documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 ******************************************************************************)

open OUnit2
module Quickcheck = Base_quickcheck


let expect_packs_to unpacked packed =
  assert_equal packed (Capnp.Runtime.Packing.pack_string unpacked);
  assert_equal unpacked (Capnp.Runtime.Packing.unpack_string packed)


let packing_suite =
  let t0 _ctx = expect_packs_to "" "" in
  let t1 _ctx = expect_packs_to
      "\x00\x00\x00\x00\x00\x00\x00\x00"
      "\x00\x00"
  in
  let t2 _ctx = expect_packs_to
      "\x00\x00\x0c\x00\x00\x22\x00\x00"
      "\x24\x0c\x22"
  in
  let t3 _ctx = expect_packs_to
      "\x01\x03\x02\x04\x05\x07\x06\x08"
      "\xff\x01\x03\x02\x04\x05\x07\x06\x08\x00"
  in
  let t4 _ctx = expect_packs_to
      "\x00\x00\x00\x00\x00\x00\x00\x00\x01\x03\x02\x04\x05\x07\x06\x08"
      "\x00\x00\xff\x01\x03\x02\x04\x05\x07\x06\x08\x00"
  in
  let t5 _ctx = expect_packs_to
      "\x00\x00\x0c\x00\x00\x22\x00\x00\x01\x03\x02\x04\x05\x07\x06\x08"
      "\x24\x0c\x22\xff\x01\x03\x02\x04\x05\x07\x06\x08\x00"
  in
  let t6 _ctx = expect_packs_to
      "\x01\x03\x02\x04\x05\x07\x06\x08\x08\x06\x07\x04\x05\x02\x03\x01"
      "\xff\x01\x03\x02\x04\x05\x07\x06\x08\x01\x08\x06\x07\x04\x05\x02\x03\x01"
  in
  let t7 _ctx = expect_packs_to
      "\x01\x02\x03\x04\x05\x06\x07\x08\
       \x01\x02\x03\x04\x05\x06\x07\x08\
       \x01\x02\x03\x04\x05\x06\x07\x08\
       \x01\x02\x03\x04\x05\x06\x07\x08\
       \x00\x02\x04\x00\x09\x00\x05\x01"
      "\xff\x01\x02\x03\x04\x05\x06\x07\x08\x03\
       \x01\x02\x03\x04\x05\x06\x07\x08\
       \x01\x02\x03\x04\x05\x06\x07\x08\
       \x01\x02\x03\x04\x05\x06\x07\x08\
       \xd6\x02\x04\x09\x05\x01"
  in
  let t8 _ctx = expect_packs_to
      "\x01\x02\x03\x04\x05\x06\x07\x08\
       \x01\x02\x03\x04\x05\x06\x07\x08\
       \x06\x02\x04\x03\x09\x00\x05\x01\
       \x01\x02\x03\x04\x05\x06\x07\x08\
       \x00\x02\x04\x00\x09\x00\x05\x01"
      "\xff\x01\x02\x03\x04\x05\x06\x07\x08\x03\
       \x01\x02\x03\x04\x05\x06\x07\x08\
       \x06\x02\x04\x03\x09\x00\x05\x01\
       \x01\x02\x03\x04\x05\x06\x07\x08\
       \xd6\x02\x04\x09\x05\x01"
  in
  let t9 _ctx = expect_packs_to
      "\x08\x00\x64\x06\x00\x01\x01\x02\
       \x00\x00\x00\x00\x00\x00\x00\x00\
       \x00\x00\x00\x00\x00\x00\x00\x00\
       \x00\x00\x00\x00\x00\x00\x00\x00\
       \x00\x00\x01\x00\x02\x00\x03\x01"
      "\xed\x08\x64\x06\x01\x01\x02\x00\x02\xd4\x01\x02\x03\x01"
  in
  "serialize_packed" >::: [
    "empty" >:: t0;
    "zero" >:: t1;
    "sparse" >:: t2;
    "literal1" >:: t3;
    "concat1" >:: t4;
    "concat2" >:: t5;
    "literal2" >:: t6;
    "literals with compressible tail" >:: t7;
    "literals with compressible tail" >:: t8;
    "sparse" >:: t9;
  ]


(* Generator for characters with a distribution that resembles real
   cap'n proto data, with lots of zeros. *)
let random_char_generator =
  let open Quickcheck.Generator.Monad_infix in
  Quickcheck.Generator.weighted_union [
    1.0, Base_quickcheck.Generator.int_uniform_inclusive 0 255 >>| char_of_int;
    5.0, Quickcheck.Generator.return '\x00';
  ]

let capnp_string_gen =
  let open Quickcheck.Generator.Monad_infix in
  Quickcheck.Generator.string_of random_char_generator >>| fun s ->
  (* input string must be word-aligned *)
  Capnp.Runtime.Util.str_slice ~stop:((String.length s) land (lnot 0x7)) s


let laws_exn name trials gen fn =
  let config = Quickcheck.Test.{default_config with Config.test_count = trials } in
  Quickcheck.Test.with_sample_exn ~config gen ~f:(fun s ->
      Base.Sequence.iter s ~f:(fun x -> if not (fn x) then failwith name)
    )

let test_random_pack_unpack _ctx =
  laws_exn "unpack(pack(x)) = x" 2000 capnp_string_gen (fun s ->
    let packed = Capnp.Runtime.Packing.pack_string s in
    let unpacked = Capnp.Runtime.Packing.unpack_string packed in
    unpacked = s)


let fragment (s : string) (add_fragment : string -> unit) =
  let open Capnp.Runtime in
  let rec loop ofs =
    if ofs = String.length s then
      ()
    else
      let next_ofs = min (String.length s) (ofs + (Random.int 100)) in
      let () = add_fragment (Util.str_slice ~start:ofs ~stop:next_ofs s) in
      loop next_ofs
  in
  loop 0


let test_random_pack_unpack_fragmented _ctx =
  laws_exn "unpack(fragment(pack(x))) = x" 2000 capnp_string_gen (fun s ->
    let open Capnp.Runtime in
    let packed = Packing.pack_string s in
    let packed_fragments = FragmentBuffer.empty () in
    let () = fragment packed (FragmentBuffer.add_fragment packed_fragments) in
    let unpacked_fragments = FragmentBuffer.empty () in
    let () = Packing.unpack ~packed:packed_fragments
        ~unpacked:unpacked_fragments
    in
    let () = assert (FragmentBuffer.byte_count packed_fragments = 0) in
    let bytes_avail = FragmentBuffer.byte_count unpacked_fragments in
    let unpacked =
      match FragmentBuffer.remove_at_least unpacked_fragments bytes_avail with
      | Some v -> v
      | None -> assert false
    in
    let () = assert (FragmentBuffer.byte_count unpacked_fragments = 0) in
    unpacked = s)


let random_packing_suite =
  "random_packing" >::: [
    "pack_string_unpack_string" >:: test_random_pack_unpack;
    "fragmented_pack_string_unpack_string" >:: test_random_pack_unpack_fragmented
  ]


let message_gen =
  let open Quickcheck.Generator.Monad_infix in
  let segment_gen = capnp_string_gen >>| Bytes.unsafe_of_string in
  Quickcheck.Generator.int_uniform_inclusive 1 25 >>= fun length ->
  Quickcheck.Generator.list_with_length ~length segment_gen
  >>| Capnp.BytesMessage.Message.of_storage

let test_random_serialize_deserialize _ctx =
  laws_exn "deserialize(fragment(serialize(x))) = x"
      2000 (Quickcheck.Generator.both message_gen capnp_string_gen) (fun (m, trailing_data) ->
    let open Capnp.Runtime in
    let serialized = Codecs.serialize ~compression:`None m in
    let ser_fragments = Codecs.FramedStream.empty `None in
    let () = fragment serialized (Codecs.FramedStream.add_fragment ser_fragments) in
    let () = Codecs.FramedStream.add_fragment ser_fragments trailing_data in
    match Codecs.FramedStream.get_next_frame ser_fragments with
    | Result.Ok decoded_message ->
        let () = assert (Codecs.FramedStream.bytes_available ser_fragments =
          (String.length trailing_data)) in
        (Capnp.Message.BytesMessage.Message.to_storage m) =
          (Capnp.Message.BytesMessage.Message.to_storage decoded_message)
    | Result.Error _ ->
        assert false)

let test_random_serialize_deserialize_fold _ctx =
  laws_exn "deserialize(fragment(serialize(x))) = x"
      2000 (Quickcheck.Generator.both message_gen capnp_string_gen) (fun (m, trailing_data) ->
    let open Capnp.Runtime in
    let ser_fragments = Codecs.FramedStream.empty `None in
    Codecs.serialize_fold m ~compression:`None ~init:() 
        ~f:(fun () fragment -> 
            Codecs.FramedStream.add_fragment ser_fragments fragment
        );
    let () = Codecs.FramedStream.add_fragment ser_fragments trailing_data in
    match Codecs.FramedStream.get_next_frame ser_fragments with
    | Result.Ok decoded_message ->
        let () = assert (Codecs.FramedStream.bytes_available ser_fragments =
          (String.length trailing_data)) in
        (Capnp.Message.BytesMessage.Message.to_storage m) =
          (Capnp.Message.BytesMessage.Message.to_storage decoded_message)
    | Result.Error _ ->
        assert false)

let test_random_serialize_deserialize_fold_copyless _ctx =
  laws_exn "deserialize(fragment(serialize(x))) = x"
      2000 (Quickcheck.Generator.both message_gen capnp_string_gen) (fun (m, trailing_data) ->
    let open Capnp.Runtime in
    let ser_fragments = Codecs.FramedStream.empty `None in
    Codecs.serialize_fold_copyless m ~compression:`None ~init:() 
        ~f:(fun () fragment len -> 
            let fragment = String.sub fragment 0 len in
            Codecs.FramedStream.add_fragment ser_fragments fragment
        );
    let () = Codecs.FramedStream.add_fragment ser_fragments trailing_data in
    match Codecs.FramedStream.get_next_frame ser_fragments with
    | Result.Ok decoded_message ->
        let () = assert (Codecs.FramedStream.bytes_available ser_fragments =
          (String.length trailing_data)) in
        (Capnp.Message.BytesMessage.Message.to_storage m) =
          (Capnp.Message.BytesMessage.Message.to_storage decoded_message)
    | Result.Error _ ->
        assert false)

let test_random_serialize_deserialize_packed _ctx =
  laws_exn "deserialize_unpack(fragment(serialize_pack(x))) = x"
      2000 message_gen (fun m ->
    let open Capnp.Runtime in
    let packed = Codecs.serialize ~compression:`Packing m in
    let pack_fragments = Codecs.FramedStream.empty `Packing in
    let () = fragment packed (Codecs.FramedStream.add_fragment pack_fragments) in
    match Codecs.FramedStream.get_next_frame pack_fragments with
    | Result.Ok decoded_message ->
        (Capnp.Message.BytesMessage.Message.to_storage m) =
          (Capnp.Message.BytesMessage.Message.to_storage decoded_message)
    | Result.Error _ ->
        assert false)


let random_serialize_suite =
  "random_serialization_deserialization" >::: [
    "serialize_deserialize_message" >:: test_random_serialize_deserialize;
    "serialize_deserialize_fold_message" >:: test_random_serialize_deserialize_fold;
    "serialize_deserialize_fold_copyless_message" >:: test_random_serialize_deserialize_fold_copyless;
    "serialize_deserialize_packed_message" >:: test_random_serialize_deserialize_packed;
  ]

