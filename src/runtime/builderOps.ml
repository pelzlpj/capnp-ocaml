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

(* Builder operations.  This provides most of the support code required for
   the builder interface. *)

type ro = Message.ro
type rw = Message.rw

let invalid_msg = Message.invalid_msg

let sizeof_uint64 = Common.sizeof_uint64

module StructSizes = struct
  type t = {
    data_words    : int;
    pointer_words : int;
  }
end

(* ROM == "Read-Only Message"
   RWM == "Read/Write Message"

   Most of the builder operations are tied to the DM types.  The exceptional
   cases are functions that make a copy from a source to a destination. *)
module Make (ROM : Message.S) (RWM : Message.S) = struct
  module ROC = Common.Make(ROM)
  module RWC = Common.Make(RWM)
  module RReader = Reader.Make(RWM)


  (* Given storage for a struct, get the pointer bytes for the given
     struct-relative pointer index. *)
  let get_struct_pointer
      (struct_storage : 'cap RWC.StructStorage.t)
      (pointer_word : int)
    : 'cap RWM.Slice.t =
    let pointers = struct_storage.RWC.StructStorage.pointers in
    let num_pointers = pointers.RWM.Slice.len / sizeof_uint64 in
    (* By design, this function should always be invoked after the struct
       has been upgraded to at least the expected data and pointer
       slice sizes. *)
    let () = assert (pointer_word < num_pointers) in {
      pointers with
      RWM.Slice.start = pointers.RWM.Slice.start + (pointer_word * sizeof_uint64);
      RWM.Slice.len   = sizeof_uint64;
    }


  (* Allocate storage for a struct within the specified message. *)
  let alloc_struct_storage
      (message : rw RWM.Message.t)
      ~(data_words : int)
      ~(pointer_words : int)
    : rw RWC.StructStorage.t =
    let storage = RWM.Slice.alloc message
      ((data_words + pointer_words) * sizeof_uint64)
    in
    let data = {storage with RWM.Slice.len = data_words * sizeof_uint64} in
    let pointers = {
      storage with
      RWM.Slice.start = data.RWM.Slice.start + data.RWM.Slice.len;
      RWM.Slice.len   = pointer_words * sizeof_uint64;
    } in {
      RWC.StructStorage.data = data;
      RWC.StructStorage.pointers = pointers;
    }


  (* Allocate storage for a list within the specified message. *)
  let alloc_list_storage
      (message : rw RWM.Message.t)
      (storage_type : Common.ListStorageType.t)
      (num_elements : int)
    : rw RWC.ListStorage.t =
    let storage =
      let open Common in
      match storage_type with
      | ListStorageType.Empty ->
          RWM.Slice.alloc message 0
      | ListStorageType.Bit ->
          RWM.Slice.alloc message (Util.ceil_ratio num_elements 8)
      | ListStorageType.Bytes1
      | ListStorageType.Bytes2
      | ListStorageType.Bytes4
      | ListStorageType.Bytes8
      | ListStorageType.Pointer ->
          RWM.Slice.alloc message
            (num_elements * (ListStorageType.get_byte_count storage_type))
      | ListStorageType.Composite (data_words, pointer_words) ->
          (* Composite list looks a little different from the other cases:
             content is prefixed by a tag word which describes the shape of
             the content. *)
          let word_count = 1 + (num_elements * (data_words + pointer_words)) in
          let slice = RWM.Slice.alloc message (word_count * sizeof_uint64) in
          let tag_descr = {
            StructPointer.offset = num_elements;
            StructPointer.data_words = data_words;
            StructPointer.pointer_words = pointer_words;
          } in
          let tag_val = StructPointer.encode tag_descr in
          let () = RWM.Slice.set_int64 slice 0 tag_val in
          slice
    in
    let open RWC.ListStorage in {
      storage;
      storage_type;
      num_elements;
    }


  (* Initialize a far pointer so that it will point to the specified [content],
     which is physically located in the given [content_slice].
     [init_normal_pointer] describes how to construct a normal intra-segment
     pointer which is appropriate for the content type; [init_far_pointer_tag]
     provides a similar method for constructing the tag word found in a
     "double far" landing pad. *)
  let init_far_pointer
      (pointer_bytes : rw RWM.Slice.t)
      ~(content : 'a)
      ~(content_slice : rw RWM.Slice.t)
      ~(init_normal_pointer : rw RWM.Slice.t -> 'a -> unit)
      ~(init_far_pointer_tag : rw RWM.Slice.t -> unit)
    : unit =
    let landing_pad_opt = RWM.Slice.alloc_in_segment
        content_slice.RWM.Slice.msg content_slice.RWM.Slice.segment_id sizeof_uint64
    in
    begin match landing_pad_opt with
    | Some landing_pad_bytes ->
        (* Use a "normal" far pointer. *)
        let () = init_normal_pointer landing_pad_bytes content in
        let far_pointer_desc = {
          FarPointer.landing_pad = FarPointer.NormalPointer;
          FarPointer.offset = landing_pad_bytes.RWM.Slice.start / sizeof_uint64;
          FarPointer.segment_id = landing_pad_bytes.RWM.Slice.segment_id;
        } in
        let far_pointer_val = FarPointer.encode far_pointer_desc in
        RWM.Slice.set_int64 pointer_bytes 0 far_pointer_val
    | None ->
        (* Use the "double far" convention. *)
        let landing_pad_bytes =
          let landing_pad =
            RWM.Slice.alloc pointer_bytes.RWM.Slice.msg (2 * sizeof_uint64)
          in
          let far_pointer_desc = {
            FarPointer.landing_pad = FarPointer.NormalPointer;
            FarPointer.offset = content_slice.RWM.Slice.start / 8;
            FarPointer.segment_id = content_slice.RWM.Slice.segment_id;
          } in
          let () = RWM.Slice.set_int64 landing_pad 0
            (FarPointer.encode far_pointer_desc)
          in
          let tag_slice = {
            landing_pad with
            RWM.Slice.start = landing_pad.RWM.Slice.start + sizeof_uint64;
            RWM.Slice.len   = sizeof_uint64;
          } in
          let () = init_far_pointer_tag tag_slice in
          landing_pad
        in
        let far_pointer_desc = {
          FarPointer.landing_pad = FarPointer.TaggedFarPointer;
          FarPointer.offset = landing_pad_bytes.RWM.Slice.start / sizeof_uint64;
          FarPointer.segment_id = landing_pad_bytes.RWM.Slice.segment_id;
        } in
        let far_pointer_val = FarPointer.encode far_pointer_desc in
        RWM.Slice.set_int64 pointer_bytes 0 far_pointer_val
    end


  let list_pointer_type_of_storage_type tp =
    let open Common in
    match tp with
    | ListStorageType.Empty       -> ListPointer.Void
    | ListStorageType.Bit         -> ListPointer.OneBitValue
    | ListStorageType.Bytes1      -> ListPointer.OneByteValue
    | ListStorageType.Bytes2      -> ListPointer.TwoByteValue
    | ListStorageType.Bytes4      -> ListPointer.FourByteValue
    | ListStorageType.Bytes8      -> ListPointer.EightByteValue
    | ListStorageType.Pointer     -> ListPointer.EightBytePointer
    | ListStorageType.Composite _ -> ListPointer.Composite


  (* Given a pointer location and list storage located within the same
     message segment, modify the pointer so that it points to the list
     storage. *)
  let init_normal_list_pointer
      (pointer_bytes : rw RWM.Slice.t)
      (list_storage : rw RWC.ListStorage.t)
    : unit =
    let storage_slice = list_storage.RWC.ListStorage.storage in
    let () =
      assert (storage_slice.RWM.Slice.segment_id = pointer_bytes.RWM.Slice.segment_id)
    in
    let offset_bytes = storage_slice.RWM.Slice.start - RWM.Slice.get_end pointer_bytes in
    let () = assert (offset_bytes land 7 = 0) in
    let offset_words = offset_bytes / 8 in
    let element_type =
      list_pointer_type_of_storage_type list_storage.RWC.ListStorage.storage_type
    in
    let pointer_element_count =
      match list_storage.RWC.ListStorage.storage_type with
      | Common.ListStorageType.Composite (data_words, pointer_words) ->
          list_storage.RWC.ListStorage.num_elements * (data_words + pointer_words)
      | _ ->
          list_storage.RWC.ListStorage.num_elements
    in
    let pointer_descr = {
      ListPointer.offset = offset_words;
      ListPointer.element_type = element_type;
      ListPointer.num_elements = pointer_element_count;
    } in
    let pointer_val = ListPointer.encode pointer_descr in
    RWM.Slice.set_int64 pointer_bytes 0 pointer_val


  (* Initialize a list pointer so that it points to the specified list storage. *)
  let init_list_pointer
      (pointer_bytes : rw RWM.Slice.t)
      (list_storage : rw RWC.ListStorage.t)
    : unit =
    let storage_slice = list_storage.RWC.ListStorage.storage in
    if storage_slice.RWM.Slice.segment_id = pointer_bytes.RWM.Slice.segment_id then
      (* Use a normal intra-segment list pointer. *)
      init_normal_list_pointer pointer_bytes list_storage
    else
      let init_far_pointer_tag tag_slice =
        let pointer_element_count =
          match list_storage.RWC.ListStorage.storage_type with
          | Common.ListStorageType.Composite (data_words, pointer_words) ->
              list_storage.RWC.ListStorage.num_elements * (data_words + pointer_words)
          | _ ->
              list_storage.RWC.ListStorage.num_elements
        in
        let tag_word_desc = {
          ListPointer.offset = 0;
          ListPointer.element_type = list_pointer_type_of_storage_type
              list_storage.RWC.ListStorage.storage_type;
          ListPointer.num_elements = pointer_element_count;
        } in
        RWM.Slice.set_int64 pointer_bytes 0 (ListPointer.encode tag_word_desc)
      in
      init_far_pointer pointer_bytes
        ~content:list_storage
        ~content_slice:list_storage.RWC.ListStorage.storage
        ~init_normal_pointer:init_normal_list_pointer
        ~init_far_pointer_tag


  (* Given a pointer location and struct storage located within the same
     message segment, modify the pointer so that it points to the struct
     storage. *)
  let init_normal_struct_pointer
      (pointer_bytes : rw RWM.Slice.t)
      (struct_storage : 'cap RWC.StructStorage.t)
    : unit =
    let () = assert (struct_storage.RWC.StructStorage.data.RWM.Slice.segment_id =
      pointer_bytes.RWM.Slice.segment_id)
    in
    let pointer_descr = {
      StructPointer.offset = struct_storage.RWC.StructStorage.data.RWM.Slice.start -
          RWM.Slice.get_end pointer_bytes;
      StructPointer.data_words =
        struct_storage.RWC.StructStorage.data.RWM.Slice.len / 8;
      StructPointer.pointer_words =
        struct_storage.RWC.StructStorage.pointers.RWM.Slice.len / 8;
    } in
    let pointer_val = StructPointer.encode pointer_descr in
    RWM.Slice.set_int64 pointer_bytes 0 pointer_val


  (* Initialize a struct pointer so that it points to the specified
     struct storage. *)
  let init_struct_pointer
      (pointer_bytes : rw RWM.Slice.t)
      (struct_storage : 'cap RWC.StructStorage.t)
    : unit =
    if struct_storage.RWC.StructStorage.data.RWM.Slice.segment_id =
        pointer_bytes.RWM.Slice.segment_id then
      (* Use a normal intra-segment struct pointer. *)
      init_normal_struct_pointer pointer_bytes struct_storage
    else
      let init_far_pointer_tag tag_slice =
        let tag_word_desc = {
          StructPointer.offset = 0;
          StructPointer.data_words =
            struct_storage.RWC.StructStorage.data.RWM.Slice.len / 8;
          StructPointer.pointer_words =
            struct_storage.RWC.StructStorage.pointers.RWM.Slice.len / 8;
        } in
        RWM.Slice.set_int64 pointer_bytes 0 (StructPointer.encode tag_word_desc)
      in
      let content_slice = {
        struct_storage.RWC.StructStorage.data with
        RWM.Slice.len = struct_storage.RWC.StructStorage.data.RWM.Slice.len +
            struct_storage.RWC.StructStorage.pointers.RWM.Slice.len
      } in
      init_far_pointer pointer_bytes
        ~content:struct_storage
        ~content_slice
        ~init_normal_pointer:init_normal_struct_pointer
        ~init_far_pointer_tag


  (* Copy a pointer from the source slice to the destination slice.  This
     copies the pointer only, not the pointed-to data.  If the source
     and destination are in different segments, this may result in
     allocating additional message space to instantiate a far pointer. *)
  let shallow_copy_pointer
      ~(src : 'cap RWM.Slice.t)
      ~(dest : rw RWM.Slice.t)
    : unit =
    match RWC.deref_pointer src with
    | RWC.Object.None ->
        RWM.Slice.set_int64 dest 0 Int64.zero
    | RWC.Object.List list_storage ->
        init_list_pointer dest list_storage
    | RWC.Object.Struct struct_storage ->
        init_struct_pointer dest struct_storage


  (* Copy a struct from the source slice to the destination slice.  This
     is a shallow copy; the data section is copied in bitwise fashion,
     and the pointers are copied using [shallow_copy_pointer]. *)
  let shallow_copy_struct
      ~(src : 'cap RWC.StructStorage.t)
      ~(dest : rw RWC.StructStorage.t)
    : unit =
    let open RWC.StructStorage in
    let data_copy_size =
      min src.data.RWM.Slice.len dest.data.RWM.Slice.len
    in
    let () = RWM.Slice.blit
        ~src:src.data ~src_ofs:0
        ~dest:dest.data ~dest_ofs:0
        ~len:data_copy_size
    in
    let pointer_copy_size =
      min src.pointers.RWM.Slice.len dest.pointers.RWM.Slice.len
    in
    let pointer_copy_words = pointer_copy_size / sizeof_uint64 in
    for i = 0 to pointer_copy_words - 1 do
      let src_pointer  = get_struct_pointer src i in
      let dest_pointer = get_struct_pointer dest i in
      shallow_copy_pointer ~src:src_pointer ~dest:dest_pointer
    done


  (* Upgrade a List<Struct> so that each of the elements is at least as large
     as the requirements of the current schema version.  In general, this will
     allocate a new list, make a shallow copy of the old data into the new list,
     zero out the old data, and update the list pointer to reflect the change.
     If the schema has not changed, this is a noop.

     Returns the new list storage descriptor. *)
  let upgrade_struct_list
      (pointer_bytes : rw RWM.Slice.t)
      (list_storage : rw RWC.ListStorage.t)
      ~(data_words : int)
      ~(pointer_words : int)
    : rw RWC.ListStorage.t =
    let needs_upgrade =
      let open Common in
      match list_storage.RWC.ListStorage.storage_type with
      | ListStorageType.Bytes1
      | ListStorageType.Bytes2
      | ListStorageType.Bytes4
      | ListStorageType.Bytes8 ->
          let orig_data_size =
            ListStorageType.get_byte_count list_storage.RWC.ListStorage.storage_type
          in
          data_words * sizeof_uint64 > orig_data_size || pointer_words > 0
      | ListStorageType.Pointer ->
          data_words > 0 || pointer_words > 1
      | ListStorageType.Composite (orig_data_words, orig_pointer_words) ->
          data_words > orig_data_words || pointer_words > orig_pointer_words
      | ListStorageType.Empty
      | ListStorageType.Bit ->
          invalid_msg "decoded non-struct list where struct list was expected"
    in
    if needs_upgrade then
      let message = pointer_bytes.RWM.Slice.msg in
      let new_storage = alloc_list_storage message
          (Common.ListStorageType.Composite (data_words, pointer_words))
          list_storage.RWC.ListStorage.num_elements
      in
      let src_struct_of_index  = RWC.make_struct_of_list_index list_storage in
      let dest_struct_of_index = RWC.make_struct_of_list_index new_storage in
      for i = 0 to list_storage.RWC.ListStorage.num_elements - 1 do
        shallow_copy_struct ~src:(src_struct_of_index i)
          ~dest:(dest_struct_of_index i)
      done;
      let content_slice =
        match list_storage.RWC.ListStorage.storage_type with
        | Common.ListStorageType.Composite _ ->
            (* Composite lists prefix the storage region with a tag word,
               which we can zero out as well. *)
            { list_storage.RWC.ListStorage.storage with
              RWM.Slice.start =
                list_storage.RWC.ListStorage.storage.RWM.Slice.start - sizeof_uint64;
              RWM.Slice.len =
                list_storage.RWC.ListStorage.storage.RWM.Slice.len + sizeof_uint64; }
        | _ ->
            list_storage.RWC.ListStorage.storage
      in
      let () = init_list_pointer pointer_bytes new_storage in
      let () = RWM.Slice.zero_out content_slice
          ~ofs:0 ~len:content_slice.RWM.Slice.len
      in
      new_storage
    else
      list_storage


  (* Given a pointer which is expected to be a list pointer, compute the
     corresponding list storage descriptor.  If the pointer is null, storage
     for a default list is immediately allocated using [alloc_default_list]. *)
  let deref_list_pointer
      ?(struct_sizes : StructSizes.t option)
      ~(create_default : rw RWM.Message.t -> rw RWC.ListStorage.t)
      (pointer_bytes : rw RWM.Slice.t)
    : rw RWC.ListStorage.t =
    match RReader.deref_list_pointer pointer_bytes with
    | None ->
        let list_storage = create_default pointer_bytes.RWM.Slice.msg in
        let () = init_list_pointer pointer_bytes list_storage in
        list_storage
    | Some list_storage ->
        begin match struct_sizes with
        | Some { StructSizes.data_words; StructSizes.pointer_words } ->
            upgrade_struct_list pointer_bytes list_storage
              ~data_words ~pointer_words
        | None ->
            list_storage
        end


  (* Set a struct to all-zeros.  Pointers are not followed. *)
  let shallow_zero_out_struct
      (struct_storage : rw RWC.StructStorage.t)
    : unit =
    let open RWC.StructStorage in
    RWM.Slice.zero_out struct_storage.data
      ~ofs:0 ~len:struct_storage.data.RWM.Slice.len;
    RWM.Slice.zero_out struct_storage.pointers
      ~ofs:0 ~len:struct_storage.pointers.RWM.Slice.len


  (* Upgrade a struct so that its data and pointer regions are at least as large
     as the protocol currently specifies.  If the [orig] struct satisfies the
     requirements of the [data_words] and [pointer_words], this is a no-op.
     Otherwise a new struct is allocated, the data is copied over, the [orig]
     is zeroed out, and the pointer to the struct is updated.

     Returns: new struct descriptor (possibly the same as the old one). *)
  let upgrade_struct
      (pointer_bytes : rw RWM.Slice.t)
      (orig : rw RWC.StructStorage.t)
      ~(data_words : int)
      ~(pointer_words : int)
    : rw RWC.StructStorage.t =
    let open RWC.StructStorage in
    if orig.data.RWM.Slice.len < data_words * sizeof_uint64 ||
       orig.pointers.RWM.Slice.len < pointer_words * sizeof_uint64 then
      let new_storage =
        alloc_struct_storage orig.data.RWM.Slice.msg ~data_words ~pointer_words
      in
      let () = shallow_copy_struct ~src:orig ~dest:new_storage in
      let () = init_struct_pointer pointer_bytes new_storage in
      let () = shallow_zero_out_struct orig in
      new_storage
    else
      orig


  (* Given a pointer which is expected to be a struct pointer, compute the
     corresponding struct storage descriptor.  If the pointer is null, storage
     for a default struct is immediately allocated using [alloc_default_struct].
     [data_words] and [pointer_words] indicate the expected structure layout;
     if the struct has a smaller layout (i.e. from an older protocol version),
     then a new struct is allocated and the data is copied over. *)
  let deref_struct_pointer
      ~(create_default : rw RWM.Message.t -> rw RWC.StructStorage.t)
      ~(data_words : int)
      ~(pointer_words : int)
      (pointer_bytes : rw RWM.Slice.t)
    : rw RWC.StructStorage.t =
    match RReader.deref_struct_pointer pointer_bytes with
    | None ->
        let struct_storage = create_default pointer_bytes.RWM.Slice.msg in
        let () = init_struct_pointer pointer_bytes struct_storage in
        struct_storage
    | Some struct_storage ->
        upgrade_struct pointer_bytes struct_storage ~data_words ~pointer_words


  (* Given a [src] pointer to an arbitrary struct or list, first create a
     deep copy of the pointed-to data then store a pointer to the data in
     [dest]. *)
  let rec deep_copy_pointer
      ~(src : 'cap ROM.Slice.t)
      ~(dest : rw RWM.Slice.t)
    : unit =
    match ROC.deref_pointer src with
    | ROC.Object.None ->
        RWM.Slice.set_int64 dest 0 Int64.zero
    | ROC.Object.List src_list_storage ->
        let dest_list_storage =
          deep_copy_list ~src:src_list_storage ~dest_message:dest.RWM.Slice.msg ()
        in
        init_list_pointer dest dest_list_storage
    | ROC.Object.Struct src_struct_storage ->
        let dest_struct_storage =
          let data_words =
            src_struct_storage.ROC.StructStorage.data.ROM.Slice.len / sizeof_uint64
          in
          let pointer_words =
            src_struct_storage.ROC.StructStorage.pointers.ROM.Slice.len / sizeof_uint64
          in
          deep_copy_struct ~src:src_struct_storage ~dest_message:dest.RWM.Slice.msg
            ~data_words ~pointer_words
        in
        init_struct_pointer dest dest_struct_storage

  (* Given a [src] struct storage descriptor, first allocate storage in
     [dest_message] for a copy of the struct and then fill the allocated
     region with a deep copy.  [data_words] and [pointer_words] specify the
     minimum allocation regions for the destination struct, and may exceed the
     corresponding sizes from the [src] (for example, when fields are added
     during a schema upgrade).
  *)
  and deep_copy_struct
      ~(src : 'cap ROC.StructStorage.t)
      ~(dest_message : rw RWM.Message.t)
      ~(data_words : int)
      ~(pointer_words : int)
    : rw RWC.StructStorage.t =
    let src_data_words    = src.ROC.StructStorage.data.ROM.Slice.len / sizeof_uint64 in
    let src_pointer_words = src.ROC.StructStorage.pointers.ROM.Slice.len / sizeof_uint64 in
    let dest_data_words    = max data_words src_data_words in
    let dest_pointer_words = max pointer_words src_pointer_words in
    let dest = alloc_struct_storage dest_message
        ~data_words:dest_data_words ~pointer_words:dest_pointer_words
    in
    let () = deep_copy_struct_to_dest ~src ~dest in
    dest

  (* As [deep_copy_struct], but the destination is already allocated. *)
  and deep_copy_struct_to_dest
      ~(src : 'cap ROC.StructStorage.t)
      ~(dest : rw RWC.StructStorage.t)
    : unit =
    let data_bytes = min
        src.ROC.StructStorage.data.ROM.Slice.len
        dest.RWC.StructStorage.data.RWM.Slice.len
    in
    let () = assert ((data_bytes mod sizeof_uint64) = 0) in
    let data_words = data_bytes / sizeof_uint64 in
    let () =
      let src_data  = src.ROC.StructStorage.data in
      let dest_data = dest.RWC.StructStorage.data in
      for i = 0 to data_words - 1 do
        let byte_ofs = i * sizeof_uint64 in
        let word = ROM.Slice.get_int64 src_data byte_ofs in
        RWM.Slice.set_int64 dest_data byte_ofs word
      done
    in
    let src_pointer_words =
      src.ROC.StructStorage.pointers.ROM.Slice.len / sizeof_uint64
    in
    let dest_pointer_words =
      dest.RWC.StructStorage.pointers.RWM.Slice.len / sizeof_uint64
    in
    let pointer_words = min src_pointer_words dest_pointer_words in
    for i = 0 to pointer_words - 1 do
      let src_pointer =
        let open ROC.StructStorage in {
        src.pointers with
        ROM.Slice.start = src.pointers.ROM.Slice.start + (i * sizeof_uint64);
        ROM.Slice.len   = sizeof_uint64;
      } in
      let dest_pointer =
        let open RWC.StructStorage in {
        dest.pointers with
        RWM.Slice.start = dest.pointers.RWM.Slice.start + (i * sizeof_uint64);
        RWM.Slice.len   = sizeof_uint64;
      } in
      deep_copy_pointer ~src:src_pointer ~dest:dest_pointer
    done

  (* Given a [src] list storage descriptor, first allocate storage in
     [dest_message] for a copy of the list and then fill the allocated
     region with deep copies of the list elements.  If the [struct_sizes]
     are provided, the deep copy will create inlined structs which have
     data and pointer regions at least as large as specified. *)
  and deep_copy_list
      ?(struct_sizes : StructSizes.t option)
      ~(src : 'cap ROC.ListStorage.t)
      ~(dest_message : rw RWM.Message.t)
      ()
    : rw RWC.ListStorage.t =
    match struct_sizes with
    | Some { StructSizes.data_words; StructSizes.pointer_words } ->
        deep_copy_struct_list ~src ~dest_message
          ~data_words ~pointer_words
    | None ->
        let dest =
          alloc_list_storage dest_message src.ROC.ListStorage.storage_type
            src.ROC.ListStorage.num_elements
        in
        let copy_by_value word_count =
          for i = 0 to word_count - 1 do
            let byte_ofs = i * sizeof_uint64 in
            let word = ROM.Slice.get_int64 src.ROC.ListStorage.storage byte_ofs in
            RWM.Slice.set_int64 dest.RWC.ListStorage.storage byte_ofs word
          done
        in
        let () =
          let open Common in
          match src.ROC.ListStorage.storage_type with
          | ListStorageType.Empty ->
              ()
          | ListStorageType.Bit ->
              copy_by_value
                (Util.ceil_ratio (Util.ceil_ratio src.ROC.ListStorage.num_elements 8)
                   sizeof_uint64)
          | ListStorageType.Bytes1
          | ListStorageType.Bytes2
          | ListStorageType.Bytes4
          | ListStorageType.Bytes8 ->
              let byte_count =
                ListStorageType.get_byte_count src.ROC.ListStorage.storage_type
              in
              copy_by_value
                (Util.ceil_ratio (src.ROC.ListStorage.num_elements * byte_count)
                   sizeof_uint64)
          | ListStorageType.Pointer ->
              let open RWC.ListStorage in
              for i = 0 to src.ROC.ListStorage.num_elements - 1 do
                let src_pointer =
                  let open ROC.ListStorage in {
                  src.storage with
                  ROM.Slice.start = src.storage.ROM.Slice.start + (i * sizeof_uint64);
                  ROM.Slice.len   = sizeof_uint64;
                } in
                let dest_pointer =
                  let open RWC.ListStorage in {
                  dest.storage with
                  RWM.Slice.start = dest.storage.RWM.Slice.start + (i * sizeof_uint64);
                  RWM.Slice.len   = sizeof_uint64;
                } in
                deep_copy_pointer ~src:src_pointer ~dest:dest_pointer
              done
          | ListStorageType.Composite (data_words, pointer_words) ->
              let words_per_element = data_words + pointer_words in
              for i = 0 to src.ROC.ListStorage.num_elements - 1 do
                let src_struct =
                  let open ROC.ListStorage in {
                  ROC.StructStorage.data = {
                    src.storage with
                    ROM.Slice.start = src.storage.ROM.Slice.start +
                        (i * words_per_element * sizeof_uint64);
                    ROM.Slice.len = data_words * sizeof_uint64;};
                  ROC.StructStorage.pointers = {
                    src.storage with
                    ROM.Slice.start = src.storage.ROM.Slice.start +
                        ((i * words_per_element) + data_words) * sizeof_uint64;
                    ROM.Slice.len = pointer_words * sizeof_uint64;};
                } in
                let dest_struct =
                  let open RWC.ListStorage in {
                  RWC.StructStorage.data = {
                    dest.storage with
                    RWM.Slice.start = dest.storage.RWM.Slice.start +
                        (i * words_per_element * sizeof_uint64);
                    RWM.Slice.len = data_words * sizeof_uint64;};
                  RWC.StructStorage.pointers = {
                    dest.storage with
                    RWM.Slice.start = dest.storage.RWM.Slice.start +
                        ((i * words_per_element) + data_words) * sizeof_uint64;
                    RWM.Slice.len = pointer_words * sizeof_uint64;};
                } in
                deep_copy_struct_to_dest ~src:src_struct ~dest:dest_struct
              done
        in
        dest

  (* Given a List<Struct>, allocate new (orphaned) list storage and
     deep-copy the list elements into it.  The newly-allocated list
     shall have data and pointers regions sized according to
     [data_words] and [pointer_words], to support schema upgrades;
     if the source has a larger data/pointers region, the additional
     bytes are copied as well.

     Returns: new list storage
  *)
  and deep_copy_struct_list
      ~(src : 'cap ROC.ListStorage.t)
      ~(dest_message : rw RWM.Message.t)
      ~(data_words : int)
      ~(pointer_words : int)
    : rw RWC.ListStorage.t =
    let dest_storage =
      let (dest_data_words, dest_pointer_words) =
        let open Common in
        match src.ROC.ListStorage.storage_type with
        | ListStorageType.Bytes1
        | ListStorageType.Bytes2
        | ListStorageType.Bytes4
        | ListStorageType.Bytes8
        | ListStorageType.Pointer ->
            (data_words, pointer_words)
        | ListStorageType.Composite (src_data_words, src_pointer_words) ->
            (max data_words src_data_words, max pointer_words src_pointer_words)
        | ListStorageType.Empty
        | ListStorageType.Bit ->
          invalid_msg
            "decoded unexpected list type where List<struct> was expected"
      in
      alloc_list_storage dest_message
        (Common.ListStorageType.Composite (dest_data_words, dest_pointer_words))
        src.ROC.ListStorage.num_elements
    in
    let src_struct_of_list_index  = ROC.make_struct_of_list_index src in
    let dest_struct_of_list_index = RWC.make_struct_of_list_index dest_storage in
    for i = 0 to src.ROC.ListStorage.num_elements - 1 do
      let src_struct  = src_struct_of_list_index i in
      let dest_struct = dest_struct_of_list_index i in
      deep_copy_struct_to_dest ~src:src_struct ~dest:dest_struct
    done;
    dest_storage


  (* Recursively zero out all data which this pointer points to.  The pointer
     value is unchanged. *)
  let rec deep_zero_pointer
      (pointer_bytes : rw RWM.Slice.t)
    : unit =
    match RWC.deref_pointer pointer_bytes with
    | RWC.Object.None ->
        ()
    | RWC.Object.List list_storage ->
        deep_zero_list list_storage
    | RWC.Object.Struct struct_storage ->
        deep_zero_struct struct_storage

  and deep_zero_list
      (list_storage : rw RWC.ListStorage.t)
    : unit =
    let open Common in
    match list_storage.RWC.ListStorage.storage_type with
    | ListStorageType.Empty
    | ListStorageType.Bit
    | ListStorageType.Bytes1
    | ListStorageType.Bytes2
    | ListStorageType.Bytes4
    | ListStorageType.Bytes8 ->
        RWM.Slice.zero_out list_storage.RWC.ListStorage.storage
          ~ofs:0 ~len:list_storage.RWC.ListStorage.storage.RWM.Slice.len
    | ListStorageType.Pointer ->
        let open RWC.ListStorage in
        let () =
          for i = 0 to list_storage.num_elements - 1 do
            let pointer_bytes = {
              list_storage.storage with
              RWM.Slice.start =
                list_storage.storage.RWM.Slice.start + (i * sizeof_uint64);
              RWM.Slice.len = sizeof_uint64;
            } in
            deep_zero_pointer pointer_bytes
          done
        in
        RWM.Slice.zero_out list_storage.storage
          ~ofs:0 ~len:list_storage.storage.RWM.Slice.len
    | ListStorageType.Composite (data_words, pointer_words) ->
        let open RWC.ListStorage in
        let () =
          let total_words = data_words + pointer_words in
          for i = 0 to list_storage.num_elements - 1 do
            (* Note: delegating to [deep_zero_struct] is kind of inefficient
               because it means we clear most of the list twice. *)
            let data = {
              list_storage.storage with
              RWM.Slice.start = list_storage.storage.RWM.Slice.start +
                  (i * total_words * sizeof_uint64);
              RWM.Slice.len = data_words * sizeof_uint64;
            } in
            let pointers = {
              list_storage.storage with
              RWM.Slice.start = RWM.Slice.get_end data;
              RWM.Slice.len   = pointer_words * sizeof_uint64;
            } in
            deep_zero_struct { RWC.StructStorage.data; RWC.StructStorage.pointers }
          done
        in
        (* Composite lists prefix the data with a tag word, so clean up
           the tag word along with everything else *)
        let content_slice = {
          list_storage.storage with
          RWM.Slice.start = list_storage.storage.RWM.Slice.start - sizeof_uint64;
          RWM.Slice.len   = list_storage.storage.RWM.Slice.len   + sizeof_uint64;
        } in
        RWM.Slice.zero_out content_slice ~ofs:0 ~len:content_slice.RWM.Slice.len

  and deep_zero_struct
    (struct_storage : rw RWC.StructStorage.t)
    : unit =
    let open RWC.StructStorage in
    let pointer_words =
      struct_storage.pointers.RWM.Slice.len / sizeof_uint64
    in
    for i = 0 to pointer_words - 1 do
      let pointer_bytes = get_struct_pointer struct_storage i in
      deep_zero_pointer pointer_bytes
    done;
    RWM.Slice.zero_out struct_storage.data
      ~ofs:0 ~len:struct_storage.data.RWM.Slice.len;
    RWM.Slice.zero_out struct_storage.pointers
      ~ofs:0 ~len:struct_storage.pointers.RWM.Slice.len

end
