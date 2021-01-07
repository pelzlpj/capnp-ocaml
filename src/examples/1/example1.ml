module BM = Capnp.BytesMessage
module BS = Capnp.BytesStorage
(* Instantiate a module for the Foo type *)
module Foo = Foo.Make(BM)


(* This is a very simple example where we create a message - encode it then decode it *)

let encode n =
  let open Foo in
  (* Constructing the message. First we instantiate and initialize the Foo builder giving us a handle on a read/write object. *)
  let rw = Builder.Foo.init_root () in
  (* Using the readwrite handle we set [num] to the provided value *)
  Builder.Foo.num_set rw n;
  (* Then encode it into a message constructing a string with the contents *)
  let message = Builder.Foo.to_message rw in
  Capnp.Codecs.serialize ~compression:`None message

let decode_exn s =
  let open Foo in
  (* Get a stream of the bytes to deserialize *)
  let stream = Capnp.Codecs.FramedStream.of_string ~compression:`None s in
  (* Attempt to get the next frame from the stream in the form of a Message *)
  let res = Capnp.Codecs.FramedStream.get_next_frame stream in 
  match res with
  | (Result.Ok message) -> Reader.Foo.of_message message
  | (Result.Error _) -> failwith "Could not extract frame"

let _ = 
  let open Foo in
  let f = encode 3l in
  Printf.printf "Read: %ld\n" (Reader.Foo.num_get (decode_exn f))
