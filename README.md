## statmemprof-mirage — MirageOS library and emacs client for statistical memory profiler

%%VERSION%%

statmemprof-mirage consists of two parts: the first part is the Statmemprof_mirage module, to be used in
a unikernel (NB: this should be done either in a secure environment, or authentication and encryption should be added):
```OCaml
  let memprof flow =
    (* yes, we could read something.. *)
    let out = Statmemprof_mirage.serve () in
    S.TCPV4.write flow (Cstruct.of_bytes out) >|= (function
    | Error e -> Logs.err (fun m -> m "error while sending memprof %a" S.TCPV4.pp_write_error e) ; ()
    | Ok () -> ()) >>= fun () ->
    S.TCPV4.close flow


  let start _clock data keys stack http =
    Statmemprof_mirage.start 1E-1 30 5 ;
    S.listen_tcpv4 stack ~port:9999 memprof ;
```

The second part is the binary `statmemprof_emacs_proxy` to be run in the host system, which is a Sturgeon/emacs front-end of the statmemprof
statistical memory profiler for OCaml that receives its data via TCP to the unikernel.

statmemprof-mirage is distributed under the MIT license.

Homepage: https://github.com/hannesm/statmemprof-mirage

## Installation

For using statmemprof-mirage with Solo5 unikernels, an instrumented
ocaml-freestanding is required. For compiling statmemprof-mirage, a
4.07.1+statistical-memprof switch is required.

Then, add the custom opam-repository:

`opam repo add statmemprof-mirage https://github.com/hannesm/statmemprof-repo`

and install statmemprof-mirage by executing:

`opam install statmemprof-mirage`

## Usage

In the OCaml program you need to profile, you can start the profiling
by executing the following instruction (see the documentation in
statmenprof_emacs.mli for more details):

`Statmemprof_mirage.start 1E-4 30 5`

Then, in emacs, load the file sturgeon.el (coming with you Sturgeon
installation), and type M-x sturgeon-connect.
