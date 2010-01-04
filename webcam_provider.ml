(*
Copyright (c) 2005, ABRAM HINDLE
All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
    * Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
    * Neither the name of the abram nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*)
let stdo = stdout;
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
let cm_endline fd =
  output_char fd '\n';
  flush fd
;;

let keyname = "324x248_rgb_int_webcam"

let (sock,fdi,fdo) = cm_connect "127.0.0.1" 9911 ;;
cm_write_line fdo ("PROVIDE "^keyname) ;;
flush fdo;;


let cm_send_buffer fdo key buf =
  let str =  Marshal.to_string buf [ Marshal.No_sharing ] in
    cm_write_line ~flush:false fdo key;
    cm_write_line ~flush:false fdo (string_of_int (String.length str));
    output_string fdo str;
    cm_endline fdo
(*
  cm_write_line ~flush:false fdo key;
  cm_write_line ~flush:false fdo (string_of_int (size*4));
  for i = 0 to (size - 1) do
    output_binary_int fdo buf.(i)
  done;
  cm_endline fdo;
*)
;;

Grabcaml.init ();;
let cam = Grabcaml.open_camera "/dev/video0" width height;;
Grabcaml.print cam ;;

for i = 0 to 20000000 do
  print_int i; print_string "\n"; flush stdo;
  if (Grabcaml.read cam buffer) then
    begin
      print_string "HAVE IMAGE\n"; flush stdo;
      (* fill screen with white color
	 putting a pixel in center of screen *)
      cm_send_buffer fdo keyname buffer;
    end;
done;;
(* quit();; *)
