(*---------------------------------------------------------------------------
   Copyright (c) 2017 CNRS. All rights reserved. Distributed under the MIT
   license.
  ---------------------------------------------------------------------------*)

(** [no_sampling f x] executes [f x] by temporarilly disabling
    sampling for the current thread. If an exception occurs, sampling
    is re-enabled. *)
val no_sampling : ('a -> 'b) -> 'a -> 'b

(** [reset ()] empties the current set of tracked blocks. *)
val reset : unit -> unit

(** [dump ()] dumps the current set of tracked blocks. *)
val dump : unit -> (int * (Memprof.sample_info * Printexc.backtrace_slot array option)) list

(** [start sampling_rate callstack_size min_sample_print] starts the
    sampling on the current process.

    [sampling_rate] is the sampling rate of the profiler. Good value: 1e-4.

    [callstack_size] is the size of the fragment of the call stack
    which is captured for each sampled allocation.

    [min_sample_print] is the minimum number of samples under which
    the location of an allocation is not displayed.
 *)
val start : float -> int -> int -> unit

val serve : unit -> bytes
