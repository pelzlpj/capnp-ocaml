module BM = Capnp.BytesMessage
module BS = Capnp.BytesStorage
module Foo = Foo.Make(BM)

let serialize n =
  (* Let's open up the Foo namespace to reduce verbosity *)
  let open Foo in
  (* Constructing the message. First we instantiate and initialize the Foo builder giving us a handle on a read/write object. *)
  let rw = Builder.Foo.init_root () in
  (* Using the readwrite (rw) handle we set [num] *)
  Builder.Foo.num_set rw n;
  (* Then build it into a message and serialize it into a string buffer and return it *)
  let message = Builder.Foo.to_message rw in
  let s = Capnp.Codecs.serialize ~compression:`None message in
  Printf.printf "Bytes: '%s'\n" s;
  s

let deserialize b =
  let open Foo in
  let message = BM.Message.of_storage [ Bytes.of_string b ] in
  let reader = Reader.Foo.of_message message in
  Printf.printf "Read: %ld\n" (Reader.Foo.num_get reader);
  ()

let _ = 
  (* TODO this doesn't actually work... *)
  deserialize (serialize 3l);
