(*
Copyright (c) 2005, ABRAM HINDLE
All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
    * Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
    * Neither the name of the abram nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*)
open Sdl;;
open Sdlvideo;; 
let stdo = stdout;;
open Grabcaml;;
open Unix;;
open Sobel;;
open Marshal;;
let width = 324 ;;
let height = 248 ;;
let twidth = 75 ;;
let theight = 20;;
let size = width*height;;
let buffer = Array.create  size 0;;
let buffer2 = Array.create size 0;;

(* opening a windows 320x200 16 bpp *)

(* open connection *)

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

let val_status name value =
  prerr_endline ("["^name^"] ["^value^"]")
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

let arg_parse_files () = 
  let files = ref [] in
    Arg.parse [] (fun s -> files := s :: !files) "bitmap files";
    let files = List.rev !files in
      files
;;



let keyname = 
  let keys = arg_parse_files() in
    if (List.length keys >= 1) then 
      List.hd keys
    else
      "324x248_rgb_int_webcam" 
;;

let (sock,fdi,fdo) = cm_connect "127.0.0.1" 9911 ;;
cm_write_line fdo ("REQUIRE "^keyname) ;;
flush fdo;;



Sdl.init [`VIDEO];; (* only video *) 
let bpp = 24;;
let screen = set_video_mode ~w:width ~h:height ~bpp:bpp [`HWSURFACE];; (* define some colors *)


while(true) do
  let buffer = cm_read_buffer fdi in
    for y = 0 to (height-1) do
      let wy = width*y in
	for x = 0 to (width-1) do
	  let (r,g,b) = rgb (buffer.(wy + x)) in
	  let c = map_RGB screen (r,g,b) in
	    put_pixel screen ~x:x ~y:y c;
	    
	done;
    done;
    
    flip screen;
    
done;;
quit();;
