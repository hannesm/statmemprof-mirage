(*---------------------------------------------------------------------------
   Copyright (c) 2017 CNRS and Frédéric Bour. All rights reserved.
   Distributed under the MIT license.
  ---------------------------------------------------------------------------*)

open Printexc
open Inuit

(* Reading and printing the set of samples. *)

type time = { min : int; max : int; sum : int; count : int }

type sampleTree =
    STC of time option * Memprof.sample_info list * int *
             (backtrace_slot, sampleTree) Hashtbl.t

let add_sampleTree (t:sampleTree) ((s_time, (s, bt)):(int * (Memprof.sample_info * Printexc.backtrace_slot array option))) : sampleTree =
  let rec aux idx (STC (time, sl, n, sth)) arr =
    let time =
      match time with
      | None ->
          { min = s_time;
            max = s_time;
            count = 1;
            sum = s_time }
      | Some time ->
          { min = min time.min s_time;
            max = max time.max s_time;
            count = time.count + 1;
            sum = time.sum + s_time }
    in
    if idx >= Printexc.raw_backtrace_length s.callstack then
      STC(Some time, s::sl, n+s.n_samples, sth)
    else
      let li = Array.get arr idx in
      let child =
        try Hashtbl.find sth li
        with Not_found -> STC (None, [], 0, Hashtbl.create 3)
      in
      Hashtbl.replace sth li (aux (idx+1) child arr);
      STC(Some time, sl, n+s.n_samples, sth)
  in
  match bt with
  | None ->
      Printf.printf "no backtrace slots :/\n%!" ;
      let (STC (_, sl, n, sth)) = t in
      STC(None, sl, n+s.n_samples, sth)
  | Some slots ->
      Printf.printf "some backtrace slots %d\n%!" (Array.length slots);
      aux 0 t slots

type sortedSampleTree =
    SSTC of time option * int array * int * (backtrace_slot * sortedSampleTree) list

let acc_si si children =
  let acc = Array.make 3 0 in
  List.iter (fun s ->
    let o = match s.Memprof.kind with
      | Memprof.Minor -> 0
      | Memprof.Major -> 1
      | Memprof.Major_postponed -> 1
      | Memprof.Serialized -> 2
    in
    acc.(o) <- acc.(o) + s.Memprof.n_samples;
  ) si;
  List.iter (fun (_, SSTC (_time, acc',_,_)) ->
    acc.(0) <- acc.(0) + acc'.(0);
    acc.(1) <- acc.(1) + acc'.(1);
    acc.(2) <- acc.(2) + acc'.(2);
  ) children;
  acc

let rec sort_sampleTree (t:sampleTree) : sortedSampleTree =
  let STC (time, sl, n, sth) = t in
  let children =
    List.sort (fun (_, SSTC (_, _, n1, _)) (_, SSTC (_, _, n2, _)) -> n2 - n1)
      (Hashtbl.fold (fun li st lst -> (li, sort_sampleTree st)::lst) sth [])
  in
  SSTC (time, acc_si sl children, n, children)


let dump_SST data =
  let sampling_rate, datas = Marshal.from_bytes data 0 in
  (sampling_rate,
   List.fold_left add_sampleTree (STC (None, [], 0, Hashtbl.create 3)) datas
   |> sort_sampleTree)

let min_samples = ref 0

let of_hex data =
  let r = ref 0 in
  for i = 0 to pred (Bytes.length data) do
    r := !r lsl 4 ;
    match Bytes.get data i with
    | '0'..'9' as n -> r := !r + (int_of_char n - 0x30)
    | 'A'..'F' as n -> r := !r + (int_of_char n - 0x37)
    | 'a'..'f' as n -> r := !r + (int_of_char n - 0x57)
    | _ -> assert false
  done ;
  !r

let read_network () =
  Printf.printf "reading network\n%!" ;
  let c = Unix.(socket PF_INET SOCK_STREAM 0) in
  Unix.(connect c (ADDR_INET (inet_addr_of_string "127.0.0.1", 9999))) ;
  let msg = Bytes.unsafe_of_string "00000004dump" in
  let r = Unix.send c msg 0 (Bytes.length msg) [] in
  Printf.printf "sent request\n%!" ;
  assert (Bytes.length msg = r) ;
  let recv_len = Bytes.create 8 in
  let l = Unix.recv c recv_len 0 8 [] in
  assert (Bytes.length recv_len = l) ;
  let size = of_hex recv_len in
  Printf.printf "reading %d bytes (%s)\n%!" size (Bytes.unsafe_to_string recv_len) ;
  let buf = Bytes.create size in
  let l = Unix.recv c buf 0 size [] in
  Printf.printf "read %d\n%!" l;
  assert (size = l) ;
  Unix.close c ;
  Printf.printf "closed and done\n%!" ;
  buf

let sturgeon_dump k =
  let print_acc k acc =
    let n = acc.(0) + acc.(1) + acc.(2) in
    let percent x = float x /. float n *. 100.0 in
    if n > 0 then begin
      Cursor.printf k " (";
      if acc.(0) > 0 then begin
        Cursor.printf k "%02.2f%% minor" (percent acc.(0));
        if acc.(0) < n then Cursor.printf k ", "
      end;
      if acc.(1) > 0 then begin
        Cursor.printf k "%02.2f%% major" (percent acc.(1));
        if acc.(2) > 0 then Cursor.printf k ", "
      end;
      if acc.(2) > 0 then
        Cursor.printf k "%02.2f%% unmarshal" (percent acc.(2));
      Cursor.printf k ")"
    end
  in
  let rec aux sampling_rate root (slot, SSTC (time, si, n, bt)) =
    if n >= !min_samples then (
      let children =
        if List.exists (fun (_,SSTC(_, _,n',_)) -> n' >= !min_samples) bt then
          Some (fun root' -> List.iter (aux sampling_rate root') bt)
        else None
      in
      let mean =
        match time with
        | None -> "<...>"
        | Some { min; max; count; sum } ->
            Printf.sprintf "%i, %i, %0.1f" min max (float sum /. float count)
      in
      let node = Widget.Tree.add ?children root in
      begin match Printexc.Slot.location slot with
      | Some { filename; line_number; start_char; end_char } ->
        Cursor.printf node "%11.2f MB | %s | %s:%d %d-%d"
          (float n /. sampling_rate *. float Sys.word_size /. 8e6)
          mean
          filename line_number start_char end_char
      | None ->
        Cursor.printf node "%11.2f MB | %s | ?"
          (float n /. sampling_rate *. float Sys.word_size /. 8e6)
          mean
      end;
      print_acc node si
    )
  in
  let data = read_network () in
  let sampling_rate, SSTC (_time, si, n, bt) = dump_SST data in
  let root = Widget.Tree.make k in
  let node = Widget.Tree.add root ~children:(fun root' -> List.iter (aux sampling_rate root') bt) in
  Cursor.printf node "%11.2f MB total "
                (float n /. sampling_rate *. float Sys.word_size /. 8e6);
  print_acc node si

let () =
  (* let start sampling_rate callstack_size min_samples_print = *)
  min_samples := 5 (* min_samples_print *) ;
  let name = Filename.basename Sys.executable_name in
  let server =
    Sturgeon_recipes_server.text_server name
    @@ fun ~args:_ shell ->
    let cursor = Sturgeon_stui.create_cursor shell ~name in
    let menu = Cursor.sub cursor in
    Cursor.text cursor "\n";
    let body = Cursor.sub cursor in
    Cursor.link menu "[Refresh]"
      (fun _ -> Cursor.clear body; sturgeon_dump body);
    sturgeon_dump body
  in
  Sturgeon_recipes_server.main_loop server
