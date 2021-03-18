module LazyStream = struct
  (**
     A module to manipulate lazy input streams
  *)

  type 'a t =
    | Cons of 'a * 'a t Lazy.t
    | Nil

  exception End_of_stream

  let tail = function
    | Cons (_, s) -> Lazy.force s
    | Nil -> raise End_of_stream

  let of_stream s =
    let rec next s =
      try Cons (Stream.next s, lazy (next s))
      with Stream.Failure -> Nil
    in
    next s

  let of_string str = str |> Stream.of_string |> of_stream

  let of_channel ic = ic |> Stream.of_channel |> of_stream
end

type source =
  | File of string
  | String of string

type location = 
  { offset: int
  ; data: char LazyStream.t
  ; reference: source
  }

let from_file fname =
  { offset = 0
  ; data = LazyStream.of_channel (open_in fname)
  ; reference = File fname
  }

let from_string s =
  { offset = 0
  ; data = LazyStream.of_string s
  ; reference = String s
  }