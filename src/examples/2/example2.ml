module BM = Capnp.BytesMessage
module BS = Capnp.BytesStorage
module Shape = Shape.Make(BM)

let rect_str =
  let open Shape in
  let rw = Builder.Shape.init_root() in
  Builder.Shape.colour_set rw Builder.Shape.Colour.Blue;
  let rect = Builder.Shape.rectangle_init rw in 
  Builder.Shape.Rectangle.width_set rect 10.0;
  Builder.Shape.Rectangle.height_set rect 15.0;
  let message = Builder.Shape.to_message rw in 
  Capnp.Codecs.serialize ~compression:`None message

let circle_str =
  let open Shape in
  let rw = Builder.Shape.init_root() in
  Builder.Shape.colour_set rw Builder.Shape.Colour.Blue;
  let circle = Builder.Shape.circle_init rw in 
  Builder.Shape.Circle.radius_set circle 3.0;
  let message = Builder.Shape.to_message rw in 
  Capnp.Codecs.serialize ~compression:`None message


let colour_to_string c =
  let open Shape.Reader.Shape.Colour in
  match c with
  | Red -> "Red"
  | Green -> "Green"
  | Blue -> "Blue"
  | _ -> failwith "Colour not handled"

let decode_exn shape_str = 
  let open Shape in
  let stream = Capnp.Codecs.FramedStream.of_string ~compression:`None shape_str in 
  let res = Capnp.Codecs.FramedStream.get_next_frame stream in 
  let shape = match res with 
    | (Result.Ok message) -> Reader.Shape.of_message message 
    | (Result.Error _) -> failwith "Could not read string"
  in
  match (Reader.Shape.get shape) with 
  | Reader.Shape.Circle c -> 
    Printf.printf "%s circle with a radius of %f\n"
      (colour_to_string (Reader.Shape.colour_get shape))
      (Reader.Shape.Circle.radius_get c)
  | Reader.Shape.Rectangle r -> 
    Printf.printf "%s rectangle with a width of %f and a height of %f\n"
      (colour_to_string (Reader.Shape.colour_get shape))
      (Reader.Shape.Rectangle.width_get r)
      (Reader.Shape.Rectangle.height_get r)
  | _ -> failwith "Could not read type"

let _ =
  decode_exn rect_str;
  decode_exn circle_str;