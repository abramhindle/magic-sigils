open Thread;;
open Mutex;;
open Condition;;
open Unix;;
open Queue;;
open Hashtbl;;

let port = 9911 

(* format

handshake:
either:

REQUIRE value1

or

PROVIDE value1

now loop over
value1\n
sizeofdata\n
rawdataofsize\n

for the reader
so
read-string of keyname
read-string of length (convert to int)
read-n-bytes of data
read-string of length 1 "\n"

for the write
write-string keyname
write-string string_of_int size
write-values data
write-string "\n"


if keyname is "#quit" then quit
*)

(*
     Mutex.lock m;
     while (* some predicate P over D is not satisfied *) do
       Condition.wait c m
     done;
     (* Modify D *)
     if (* the predicate P over D is now satified *) then Condition.signal c;
     Mutex.unlock m
*)

type listener = {
  key   : string ;
  queue : (string Queue.t);
  mutex : Mutex.t ; (* ensure exclusivity *)
  cond  : Condition.t ; (* sleep *)
}

let size_of_int  = 4 
let listeners = Hashtbl.create 30
    
let add_to_listeners key listener =
  if (Hashtbl.mem listeners 	key) then
    Hashtbl.replace listeners key ( listener :: (Hashtbl.find listeners key))
  else
    Hashtbl.add listeners key [ listener ] 
      

let make_listener key =
  let listener = { 
    key   = key ;
    queue = Queue.create () ;
    mutex = Mutex.create () ;
    cond  = Condition.create () ;
  } in
  let _ = add_to_listeners key listener in
    listener

(* read to a buffer *)
let rec read sock buf n buffer_size =
  n <= 0 || (let r = input sock buf 0 (min n buffer_size) in
	       (r > 0) && (read sock buf (n - r) (buffer_size)))

let enqueue_listener listener (message:string) =
  let _ = Mutex.lock listener.mutex in
  let _ = Queue.add 
    message
    listener.queue  in
  let _ = Condition.signal listener.cond in
  let _ = Mutex.unlock listener.mutex in
    ()


let enqueue (key:string) (message:string) =
  if (Hashtbl.mem listeners key) then
    let listeners = Hashtbl.find listeners key in
      List.iter (fun listener -> enqueue_listener listener message) listeners
  else
    () (* DO NOTHING *)
      
let read_a_line fd =
  let line = input_line fd in
  let line = let l = String.length line in
    if (l > 0) then
(*      String.sub line 0 ((String.length line) - 1) *)
      (* THIS SHOULD BE A \r check *)
      line
    else
      ""
  in
(*  print_endline ("[READING]["^line^"]"); *)
    line

let status str = print_endline ("[debug] "^str) 


let reader key sock fdi =
  try while true do 
    status "Reading a key";
    let key' = read_a_line fdi in
      if (not(key' = key)) then 
	print_endline "Ignoring input" 
      else
	let _ =  status ("Reading a size for key ["^key ^"]") in
	let ssize = read_a_line fdi in
	let size = int_of_string ssize in (* we could cache this *)
	let b = String.create (size+1) in (* we could cache this *)
	let _ =  status ("Reading a message of size "^ssize) in
	 (* if (read fdi b (size+1) (size+1)) then *)
	  really_input fdi b 0 (size);
	   (* let _ = status b in *)
	      enqueue 
		key 
		b
(*	  else
	    raise (End_of_file) *)
  done
  with (End_of_file) -> ()

let write_line fd s = 
  output_string fd s;
  output_char fd '\n';
    flush fd
     
let write_string fd s = 
  output_string fd s

let write_endline fd =
  output_char fd '\n';
  flush fd

let wstatus str = 
  prerr_endline ("[WRITER] "^str)

let writer listener sock fdi =
  try 
    while true do 
      wstatus "locking";
      Mutex.lock listener.mutex;
      wstatus "Waiting while empty";
      while (Queue.is_empty listener.queue) do
	Condition.wait listener.cond listener.mutex;
	wstatus "Waiting while empty x2";
      done;

      while (not (Queue.is_empty listener.queue)) do
	wstatus "Popping queue";
	let elm  = Queue.pop listener.queue in
	let size = String.length elm in
	    wstatus ("Unlocking , have["^(string_of_int size)^"]");

	    write_line fdi listener.key;
	    write_line fdi (string_of_int (size - 1));
	    output fdi elm 0 size; 
(*	    write_string fdi elm ;*)
	    write_endline fdi; 
      done;
      let _    = Mutex.unlock listener.mutex in
	()
    done
  with (End_of_file) -> ()
    
(* DEQUEUE AFTER FAILURE UNLOCK MUTEX DURING FAILURE *)

let split_on_first c s =
  let index = String.index s c in
  let s1 = String.sub s 0 index in
  let s2 = String.sub s (index+1) ((String.length s) - index - 1) in
    (s1,s2) 

let server (sock,s) =

  let fdi = in_channel_of_descr s in
  let fdo = out_channel_of_descr s in
    set_binary_mode_in fdi true;
    set_binary_mode_out fdo true;

(* we need to figure out the name of the data *)
(* REQUIRE\n string *)
(* PROVIDE\n string *)
  let line = read_a_line fdi in
  let (r,key) = split_on_first ' ' line in
    status (r ^ " for " ^ key);
    try
      if (r = "PROVIDE") then
	reader key s fdi
      else
      let listener = make_listener key in
	writer listener s fdo
	  
    with (End_of_file) -> ();
      Unix.close s;
      Unix.close sock
	
let () =
  let q = Queue.create () in
  let sock = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
    Unix.setsockopt sock Unix.SO_REUSEADDR true;
    Unix.bind sock (Unix.ADDR_INET(Unix.inet_addr_any, port));
    while true do
      Unix.listen sock 1;
      let (s, _) = Unix.accept sock in
      status "TICK";
      let thread = Thread.create server (sock,s) in
	Queue.add thread q
    done
      
	    

