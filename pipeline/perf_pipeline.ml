
let add_normal a =
  let s = ref 0 in
  for i = 0 to pred (Array.length a) do
    s := !s + a.(i)
  done;
  !s

let [@cold] add_unsafe a =
  let s = ref 0 in
  for i = 0 to pred (Array.length a) do
    s := !s + (Array.unsafe_get a i)
  done;
  !s

let [@cold] add_unsafe_64 a =
  let s = ref 0L in
  for i = 0 to pred (Array.length a) do
    s := Int64.add !s (Int64.of_int (Array.unsafe_get a i))
  done;
  Int64.to_int !s

let add_simple_pipe a =
  let s0 = ref 0 in
  let s1 = ref 0 in
  let s2 = ref 0 in
  let s3 = ref 0 in
  let i = ref 0 in
  while !i < Array.length a do
    s0 := !s0 + a.(!i);
    incr i;
    s1 := !s1 + a.(!i);
    incr i;
    s2 := !s2 + a.(!i);
    incr i;
    s3 := !s3 + a.(!i);
    incr i;
  done;
  !s0 + !s1 + !s2 + !s3

let add_simple_pipe_unsafe a =
  let s0 = ref 0 in
  let s1 = ref 0 in
  let s2 = ref 0 in
  let s3 = ref 0 in
  let i = ref 0 in
  while !i < Array.length a do
    s0 := !s0 + (Array.unsafe_get a !i);
    incr i;
    s1 := !s1 + (Array.unsafe_get a !i);
    incr i;
    s2 := !s2 + (Array.unsafe_get a !i);
    incr i;
    s3 := !s3 + (Array.unsafe_get a !i);
    incr i;
  done;
  !s0 + !s1 + !s2 + !s3

open Core
open Core_bench

let make_array_test name f =
  Bench.Test.create_indexed
    ~name
    (*
    ~args:[ 4; 400; 4_000; 40_000; 400_000; 4_000_000; 40_000_000 ]
       *)
    ~args:[ 40_000_000 ]
    (fun elements ->
        let ar = Array.init elements ~f:(fun _ -> Random.int Int.max_value) in
        Staged.stage (fun () ->
            f ar
          )
      )

let tests =
  [ make_array_test "normal" add_normal
  ; make_array_test "unsafe" add_unsafe
  ; make_array_test "unsafe64" add_unsafe_64
  ; make_array_test "simple_pipe" add_simple_pipe
  ; make_array_test "simple_pipe_unsafe" add_simple_pipe_unsafe
  ]

let command = Bench.make_command tests

let () =
  Command_unix.run command
