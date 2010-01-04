(*
** commlib.ml: UDP sockets for OCaml
** Copyright (C) 2005 Abram Hindle <abez@uvic.ca>
**  
** This program is free software; you can redistribute it and/or modify
** it under the terms of the GNU General Public License as published by
** the Free Software Foundation; either version 2 of the License, or
** (at your option) any later version.
** 
** This program is distributed in the hope that it will be useful,
** but WITHOUT ANY WARRANTY; without even the implied warranty of
** MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
** GNU General Public License for more details.
** 
** You should have received a copy of the GNU General Public License
** along with this program; if not, write to the Free Software 
** Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
*)
open Unix;;
let message_length = 1024 ;;
let resolve_host server =
    let server_addr =
        try
            Unix.inet_addr_of_string server
        with
        | Failure ("inet_addr_of_string") ->
            (Unix.gethostbyname server).Unix.h_addr_list.(0)
    in server_addr
;;
let bind_to_sock sockaddr =
    let sock = Unix.socket Unix.PF_INET Unix.SOCK_DGRAM 0
    in Unix.bind sock (sockaddr);
(*Unix.ADDR_INET (  inet_addr_of_string "127.0.0.1",0)); *)

(* Unix.inet_addr_any, 0)); *)
       sock;
;;
(* USE THIS ONE *)
let get_sock host port = 
        let sockaddr = Unix.ADDR_INET (resolve_host host, port) in
	bind_to_sock sockaddr
;;
(*
let port_of_sockaddr sockaddr =
    match sockaddr with
    | Unix.ADDR_UNIX (_) -> 0
    | Unix.ADDR_INET (_, port) -> port
;;
*)
let send_message message sock servaddr =
    Unix.sendto sock message 0 (String.length message) [] servaddr;
;;
let receive_message sock =
	let buf = ref (String.create message_length) in
	let recv_msg sock = 
		let len, addr = Unix.recvfrom sock !buf 0 message_length [] in
		let answer = String.sub !buf 0 len in
		(answer, addr)
	in 
	recv_msg sock
;;
let canread fdlist seconds =
	match Unix.select fdlist [] [] seconds with
           | [], [], [] -> false
           | x::xs, [], [] -> true
           | _ -> false
;;
