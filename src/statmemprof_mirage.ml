(*---------------------------------------------------------------------------
   Copyright (c) 2017 CNRS. All rights reserved. Distributed under the MIT
   license.
  ---------------------------------------------------------------------------*)

open Memprof

(* Data structures for sampled blocks *)

let with_lock _lock f x = f x

let min_buf_size = 1024
let empty_ephe = Ephemeron.K1.create ()
let samples : _ array ref = ref (Array.make min_buf_size empty_ephe)
let n_samples = ref 0
let samples_lock = ()
let counter = ref 0
let disabled = ref false

let no_sampling f x =
  disabled := true ;
  match f x with
  | exception e -> disabled := false; raise e
  | y -> disabled := false ; y

(* Data structure management functions. *)

let clean () =
  incr counter;
  let s = !samples and sz = !n_samples in
  let rec aux i j =
    if i >= sz then j
    else if Ephemeron.K1.check_key s.(i) then (s.(j) <- s.(i); aux (i+1) (j+1))
    else aux (i+1) j
  in
  n_samples := aux 0 0;
  Array.fill s !n_samples (sz - !n_samples) empty_ephe;
  if 8 * !n_samples <= Array.length s && Array.length s > min_buf_size then
    samples := Array.sub s 0 (max min_buf_size (2 * !n_samples))
  else if 2 * !n_samples > Array.length s then begin
    let s_new = Array.make (2 * !n_samples) empty_ephe in
    Array.blit !samples 0 s_new 0 !n_samples;
    samples := s_new
  end

let push e =
  if !n_samples = Array.length !samples then clean ();
  !samples.(!n_samples) <- e;
  incr n_samples

(* Our callback. *)

let callback : (int * sample_info) Memprof.callback = fun info ->
  if !disabled then None
  else
    let ephe = Ephemeron.K1.create () in
    Ephemeron.K1.set_data ephe (!counter, info);
    with_lock samples_lock push ephe;
    Some ephe

(* Control functions *)

let started = ref false
let sample_rate = ref 0.
let start sampling_rate callstack_size _min_samples_print =
  if !started then failwith "Already started";
  started := true;
  Printf.printf "started statmemprof\n%!" ;
  sample_rate := sampling_rate ;
  Memprof.start { sampling_rate; callstack_size; callback }

let reset = no_sampling @@ with_lock samples_lock @@ fun () ->
  samples := Array.make min_buf_size empty_ephe;
  n_samples := 0

let dump = no_sampling @@ with_lock samples_lock @@ fun () ->
  let s, sz = !samples, !n_samples in
  let rec aux acc i =
    if i >= sz then acc
    else match Ephemeron.K1.get_data s.(i) with
      | None -> aux acc (i+1)
      | Some (counter, info) ->
          let bt = Printexc.backtrace_slots info.Memprof.callstack in
          (match bt with
          | None -> Printf.printf "no bt\n%!" ;
          | Some arr -> Printf.printf "slots %d\n%!" (Array.length arr)) ;
          aux ((counter, (info, bt)) :: acc) (i+1)
  in
  aux [] 0

let serve () =
  (* Gc.full_major (); *)
  let data = (!sample_rate, dump ()) in
  let bytes = Marshal.to_bytes data [] in
  let len = Printf.sprintf "%08X" (Bytes.length bytes) in
  Printf.printf "serving %d bytes (%s)\n%!" (Bytes.length bytes) len ;
  Bytes.(cat (unsafe_of_string len) bytes)
