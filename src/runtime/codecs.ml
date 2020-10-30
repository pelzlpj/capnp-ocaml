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


type compression_t = [ `None | `Packing ]

module FramingError = struct
  type t =
    | Incomplete    (** less than a full frame is available *)
    | Unsupported   (** frame header describes a segment count or segment size that
                        is too large for the implementation *)
end


module UncompStream = struct

  type incomplete_frame_t = {
    frame_header      : string;
    complete_segments : string Res.Array.t;
  }

  type decoder_state_t =
    | IncompleteHeader
    | IncompleteFrame of incomplete_frame_t

  type t = {
    (** Primary storage for incoming stream segments. *)
    fragment_buffer : FragmentBuffer.t;

    (** Partially-decoded frame information *)
    mutable decoder_state : decoder_state_t;
  }

  let empty () = {
    fragment_buffer = FragmentBuffer.empty ();
    decoder_state = IncompleteHeader;
  }

  let add_fragment stream fragment =
    FragmentBuffer.add_fragment stream.fragment_buffer fragment

  let bytes_available stream =
    match stream.decoder_state with
    | IncompleteHeader ->
        FragmentBuffer.byte_count stream.fragment_buffer
    | IncompleteFrame partial_frame ->
        (String.length partial_frame.frame_header) +
        (Res.Array.fold_left (fun acc x -> acc + (String.length x))
           0
           partial_frame.complete_segments) +
        (FragmentBuffer.byte_count stream.fragment_buffer)

  let is_empty stream =
    match stream.decoder_state with
    | IncompleteHeader ->
        FragmentBuffer.byte_count stream.fragment_buffer = 0
    | _ ->
        false

  let rec get_next_frame stream =
    match stream.decoder_state with
    | IncompleteHeader -> unpack_header stream
    | IncompleteFrame incomplete_frame ->
        unpack_frame stream incomplete_frame

  and unpack_header stream =
    (* First four bytes of the header contain a segment count, which tells
       you how long the full header is *)
    match FragmentBuffer.peek_exact stream.fragment_buffer 4 with
    | Some partial_header ->
        begin try
          let segment_count =
            let bytes_header = Bytes.unsafe_of_string partial_header in
            Util.int_of_uint32_exn (BytesStorage.get_uint32 bytes_header 0)
          in
          let () =
            if segment_count > (max_int / 4) - 2 then
              Util.out_of_int_range "Uint32.to_int"
          in
          let segment_count = segment_count + 1 in
          let frame_header_size =
            let word_size = 8 in
            (Util.ceil_ratio (4 * (segment_count + 1)) word_size) * word_size
          in
          (* Now we know the full header size, so try to get the whole thing *)
          begin match FragmentBuffer.remove_exact stream.fragment_buffer
                        frame_header_size with
          | Some frame_header ->
              let () = stream.decoder_state <- IncompleteFrame {
                  frame_header;
                  complete_segments = Res.Array.empty ();
                }
              in
              get_next_frame stream
          | None ->
              Result.Error FramingError.Incomplete
          end
        with Util.Out_of_int_range _ ->
          Result.Error FramingError.Unsupported
        end
    | None ->
        Result.Error FramingError.Incomplete

  and unpack_frame stream incomplete_frame =
    let frame_header_bytes =
      Bytes.unsafe_of_string incomplete_frame.frame_header
    in
    let segment_count_u32 = BytesStorage.get_uint32 frame_header_bytes 0 in
    let segment_count = 1 + (Util.int_of_uint32_exn segment_count_u32) in
    let segments_decoded = Res.Array.length incomplete_frame.complete_segments in
    if segments_decoded = segment_count then
      let () = stream.decoder_state <- IncompleteHeader in
      let string_segments = Res.Array.to_list incomplete_frame.complete_segments in
      let bytes_segments = ListLabels.map string_segments ~f:Bytes.unsafe_of_string in
      Result.Ok (Message.BytesMessage.Message.of_storage bytes_segments)
    else
      let () = assert (segments_decoded < segment_count) in
      let segment_size_words_u32 = BytesStorage.get_uint32
          frame_header_bytes (4 + (4 * segments_decoded))
      in
      begin try
        let segment_size = 8 * (Util.int_of_uint32_exn segment_size_words_u32) in
        begin match FragmentBuffer.remove_exact stream.fragment_buffer
                      segment_size with
        | Some segment ->
            let () = Res.Array.add_one incomplete_frame.complete_segments segment in
            unpack_frame stream incomplete_frame
        | None ->
            Result.Error FramingError.Incomplete
        end
      with Invalid_argument _ ->
        Result.Error FramingError.Unsupported
      end

end


module PackedStream = struct

  type t = {
    (** Packed fragments waiting to be unpacked *)
    packed : FragmentBuffer.t;

    (** Unpacked fragments waiting to be decoded as messages *)
    unpacked : UncompStream.t;
  }

  let empty () = {
    packed = FragmentBuffer.empty ();
    unpacked = UncompStream.empty ();
  }

  let add_fragment stream fragment =
    FragmentBuffer.add_fragment stream.packed fragment

  let bytes_available stream =
    (* This isn't a very meaningful number, except maybe for the
       purpose of bounding the amount of memory in use... *)
    (FragmentBuffer.byte_count stream.packed) +
    (UncompStream.bytes_available stream.unpacked)

  let is_empty stream =
    (FragmentBuffer.byte_count stream.packed = 0) &&
    (UncompStream.is_empty stream.unpacked)

  let get_next_frame stream =
    let () = Packing.unpack ~packed:stream.packed
        ~unpacked:stream.unpacked.UncompStream.fragment_buffer
    in
    UncompStream.get_next_frame stream.unpacked

end


module FramedStream = struct
  (* Using runtime dispatch here... makes the API much easier to use
     relative to exposing different types for compressed and
     uncompressed streams. *)
  type t =
    | NoPack of UncompStream.t
    | Pack of PackedStream.t

  let empty compression =
    match compression with
    | `None    -> NoPack (UncompStream.empty ())
    | `Packing -> Pack   (PackedStream.empty ())

  let of_string ~compression s =
    match compression with
    | `None ->
        let stream = UncompStream.empty () in
        let () = UncompStream.add_fragment stream s in
        NoPack stream
    | `Packing ->
        let stream = PackedStream.empty () in
        let () = PackedStream.add_fragment stream s in
        Pack stream

  let add_fragment stream fragment =
    match stream with
    | NoPack stream' -> UncompStream.add_fragment stream' fragment
    | Pack stream'   -> PackedStream.add_fragment stream' fragment

  let bytes_available stream =
    match stream with
    | NoPack stream' -> UncompStream.bytes_available stream'
    | Pack stream'   -> PackedStream.bytes_available stream'

  let is_empty stream =
    match stream with
    | NoPack stream' -> UncompStream.is_empty stream'
    | Pack stream'   -> PackedStream.is_empty stream'

  let get_next_frame stream =
    match stream with
    | NoPack stream' -> UncompStream.get_next_frame stream'
    | Pack stream'   -> PackedStream.get_next_frame stream'

end


let make_header segment_descrs : string =
  let buf = Buffer.create 8 in
  let () = ListLabels.iter segment_descrs ~f:(fun descr ->
      let size_buf = Bytes.create 4 in
      let seg_len = descr.Message.BytesMessage.Message.bytes_consumed in
      let () = assert ((seg_len mod 8) = 0) in
      let seg_word_count = seg_len / 8 in
      let () = BytesStorage.set_uint32 size_buf 0
          (Util.uint32_of_int_exn seg_word_count)
      in
      Buffer.add_string buf (Bytes.unsafe_to_string size_buf))
  in
  let segment_sizes = Buffer.contents buf in
  let segment_count = (String.length segment_sizes) / 4 in
  if segment_count = 0 then
    invalid_arg "make_header requires nonempty message"
  else
    let count_buf = Bytes.create 4 in
    let () = BytesStorage.set_uint32 count_buf 0
        (Util.uint32_of_int_exn (segment_count - 1))
    in
    (* pad out to a word boundary *)
    let count_buf = Bytes.unsafe_to_string count_buf in
    if segment_count mod 2 = 0 then
      count_buf ^ segment_sizes ^ (String.make 4 '\x00')
    else
      count_buf ^ segment_sizes


let rec serialize_fold message ~compression ~init ~f =
  let segment_descrs = Message.BytesMessage.Message.to_storage message in
  match compression with
  | `None ->
      let header = make_header segment_descrs in
      ListLabels.fold_left segment_descrs ~init:(f init header) ~f:(fun acc descr ->
        let open Message.BytesMessage in
        let seg =
          if descr.Message.bytes_consumed = Bytes.length descr.Message.segment then
            descr.Message.segment
          else
            Bytes.sub descr.Message.segment 0 descr.Message.bytes_consumed
        in
        f acc (Bytes.unsafe_to_string seg))
  | `Packing ->
      serialize_fold message ~compression:`None ~init
        ~f:(fun acc unpacked_fragment ->
          f acc (Packing.pack_string unpacked_fragment))


let serialize_iter message ~compression ~f =
  serialize_fold message ~compression ~init:() ~f:(fun () s -> f s)


let serialize_fold_copyless message ~compression ~init ~f = 
  let segment_descrs = Message.BytesMessage.Message.to_storage message in
  match compression with
  | `None ->
      let header = make_header segment_descrs in
      ListLabels.fold_left segment_descrs ~init:(f init header (String.length header)) ~f:(fun acc descr ->
        let open Message.BytesMessage in
        f acc (Bytes.unsafe_to_string descr.Message.segment) descr.Message.bytes_consumed )
  | `Packing ->
      serialize_fold message ~compression:`None ~init
        ~f:(fun acc unpacked_fragment ->
            let packed_string = Packing.pack_string unpacked_fragment in
          f acc packed_string (String.length packed_string))


let serialize_iter_copyless message ~compression ~f =
  serialize_fold_copyless message ~compression ~init:() ~f:(fun () s -> f s)


let rec serialize ~compression message =
  match compression with
  | `None ->
      let segment_descrs = Message.BytesMessage.Message.to_storage message in
      let header = make_header segment_descrs in
      let header_size = String.length header in
      let segments_size = Message.BytesMessage.Message.total_size message in
      let total_size = header_size + segments_size in
      let buf = Bytes.create total_size in
      Bytes.blit
        (Bytes.unsafe_of_string header) 0
        buf 0
        header_size;
      let (_ : int) = ListLabels.fold_left segment_descrs ~init:header_size
          ~f:(fun pos descr ->
            let open Message.BytesMessage in
            Bytes.blit
              descr.Message.segment 0
              buf pos
              descr.Message.bytes_consumed;
            pos + descr.Message.bytes_consumed)
      in
      Bytes.unsafe_to_string buf
  | `Packing ->
      Packing.pack_string (serialize ~compression:`None message)


