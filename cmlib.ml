open Unix;;

let cm_connect server port = 
  let sock = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  let _ = Unix.connect sock (Unix.ADDR_INET (Unix.inet_addr_of_string server, port)) in
  let fdo = out_channel_of_descr sock in
  let fdi = in_channel_of_descr sock in
    set_binary_mode_in fdi true;
    set_binary_mode_out fdo true;
    (sock,fdi,fdo)
;;
let cm_write_line ?flush:(f=true) fd s = 
  output_string fd s;
  output_char fd '\n';
  if (f) then flush fd else () 
;;
let cm_endline fd =
  output_char fd '\n';
  flush fd
;;
let cm_provide fdo keyname =
  cm_write_line fdo ("PROVIDE "^keyname);
  flush fdo
;;
let cm_require fdo keyname =
  cm_write_line fdo ("REQUIRE "^keyname);
  flush fdo
;;
let cm_send_buffer fdo key buf =
  let str =  Marshal.to_string buf [ Marshal.No_sharing ] in
    cm_write_line ~flush:false fdo key;
    cm_write_line ~flush:false fdo (string_of_int (String.length str));
    output_string fdo str;
    cm_endline fdo
;;
let cm_send_int_array fdo key buf =
  cm_write_line ~flush:false fdo key;
  cm_write_line ~flush:false fdo (string_of_int (size*4));
  for i = 0 to (size - 1) do
    output_binary_int fdo buf.(i)
  done;
  cm_endline fdo;
;;

let cm_read_buffer fdi =
  let key = input_line fdi in
    val_status "key" key;
    let ssize = input_line fdi in
      val_status "ssize" ssize;
      let isize = ( int_of_string ssize )in
      let ints = isize / 4 in
	(*
	for i = 0 to (size - 1) do
	  buffer.(i) <- input_binary_int fdi ;
	done;

      let other =   input_line fdi in  
	  buffer
	*)

      let str = String.create isize in
	really_input fdi str 0 isize;
	let other =   input_line fdi in 
	  val_status "str.length  " (string_of_int (String.length str))  ; 
	  let (arr:int array) = Marshal.from_string str 0 in 
	    arr
;;

let cm_read_int_array fdi buffer =
  let key = input_line fdi in
    val_status "key" key;
    let ssize = input_line fdi in
      val_status "ssize" ssize;
      let isize = ( int_of_string ssize )in
      let ints = isize / 4 in
	
	for i = 0 to (size - 1) do
	  buffer.(i) <- input_binary_int fdi ;
	done;
	
	let other =   input_line fdi in  
	  buffer
;;
