(*
bgrab library for caml
Copyright (C) 2005 Abram Hindle

This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 2.1 of the License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this library; if not, write to the Free Software
Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
*)
type grabhandle (* Abstract *)
external initcam:   int -> int -> grabhandle		= "fgstub_init"
external opencam:   grabhandle -> string -> grabhandle                = "fgstub_open_device"
external print:     grabhandle -> unit                  = "fgstub_print_info"
external close:     grabhandle -> unit                  = "fgstub_close_device"
external read:      grabhandle -> int array -> bool     = "fgstub_read"
external mmap_read:      grabhandle -> int array -> bool     = "fgstub_mmap_read"
external set_brightness: grabhandle-> int -> unit      = "fgstub_set_brightness"
external set_contrast: grabhandle-> int -> unit      = "fgstub_set_contrast"
external set_hue: grabhandle-> int -> unit      = "fgstub_set_hue"
external set_colour: grabhandle-> int -> unit      = "fgstub_set_colour"
external get_brightness: grabhandle-> int          = "fgstub_get_brightness"
external get_contrast: grabhandle-> int            = "fgstub_get_contrast"
external get_hue: grabhandle-> int                 = "fgstub_get_hue"
external get_colour: grabhandle-> int              = "fgstub_get_colour"


exception Error of string;;

let open_camera device width height = opencam (initcam width height) device ;;
let init () =
    let x = Callback.register_exception "grabcaml" (Error "gracaml:") in
    ();
;;
let rgb x =
        let r = (0x00FF0000 land x) lsr 16 in
        let g = (0x0000FF00 land x) lsr 8 in
        let b = (0x000000FF land x) in
        (r,g,b)
;;


