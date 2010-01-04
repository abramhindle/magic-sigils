#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/callback.h>
#include <caml/fail.h>
#include "fg.h"

value fgstub_init(value width, value height) {
	CAMLparam2(width, height);
	struct video_config * camera = init_video_config(Int_val(width), Int_val(height));
	CAMLreturn ((value) camera);
}
value fgstub_open_device(value fg, value devicename) {
	CAMLparam2(fg, devicename);
        char * what = String_val(devicename);
        if (open_camera((struct video_config *)fg,what)!=1) {
                delete_video_config((struct video_config *)fg);
                raise_with_string(*caml_named_value("grabcaml"), "Could not open device");
        }
	get_camera_info((struct video_config *)fg);
        CAMLreturn ((value) fg);
}
value fgstub_print_info(value fg) {
        CAMLparam1 (fg);
	get_camera_info((struct video_config *)fg);
        if (print_camera_info((struct video_config *)fg)!=1) {
                raise_with_string(*caml_named_value("grabcaml"), "Could not print device");
        }
        CAMLreturn(Val_unit);
}
value fgstub_close_device (value fg) {
        CAMLparam1 (fg);
        close_camera((struct video_config *)fg);
	delete_video_config((struct video_config *)fg);
        CAMLreturn(Val_unit);
}
value fgstub_read(value fg, value buffer) {
        CAMLparam2(fg,buffer);
        struct video_config * f = (struct video_config *)fg;
        unsigned char * image = (unsigned char*)f->buffer;
	int depth = f->depth;
	int pixels = f->pixels;
	int c,i;
	read_read(f,0);
        /* if (fg_get_next_image(f)==NULL) {
                CAMLreturn(Val_bool(0));
        }*/
	/* only handles RGB */
        for (i = 0; i < pixels ; i++) {
		c = (image[depth * i + 0] ) | 
		    (image[depth * i + 1] << 8)|
		    (image[depth * i + 2] << 16);
                Store_field(buffer, i, Val_int(c));
        }
        CAMLreturn(Val_bool(1));
}
value fgstub_mmap_read(value fg, value buffer) {
        CAMLparam2(fg,buffer);
        struct video_config * f = (struct video_config *)fg;
        unsigned char * image = (unsigned char*)f->buffer;
	int depth = f->depth;
	int pixels = f->pixels;
	int c,i;
	mmap_read(f,0);
        /* if (fg_get_next_image(f)==NULL) {
                CAMLreturn(Val_bool(0));
        }*/
	/* only handles RGB */
        for (i = 0; i < pixels ; i++) {
		c = (image[depth * i + 0] ) | 
		    (image[depth * i + 1] << 8)|
		    (image[depth * i + 2] << 16);
                Store_field(buffer, i, Val_int(c));
        }
        CAMLreturn(Val_bool(1));
}

value fgstub_get_colour(value fg) {
        CAMLparam1 (fg);
        int out = get_colour((struct video_config *)fg);
	CAMLreturn(Val_int(out));
}
value fgstub_get_brightness(value fg) {
        CAMLparam1 (fg);
        int out = get_brightness((struct video_config *)fg);
	CAMLreturn(Val_int(out));
}
value fgstub_get_contrast(value fg) {
        CAMLparam1 (fg);
        int out = get_contrast((struct video_config *)fg);
	CAMLreturn(Val_int(out));
}
value fgstub_get_hue(value fg) {
        CAMLparam1 (fg);
        int out = get_hue((struct video_config *)fg);
	CAMLreturn(Val_int(out));
}
value fgstub_set_hue(value fg, value val) {
        CAMLparam2 (fg, val);
        set_hue((struct video_config *)fg, Int_val(val));
        CAMLreturn(Val_unit);
}
value fgstub_set_colour(value fg, value val) {
        CAMLparam2 (fg, val);
        set_colour((struct video_config *)fg, Int_val(val));
        CAMLreturn(Val_unit);
}
value fgstub_set_contrast(value fg, value val) {
        CAMLparam2 (fg, val);
        set_contrast((struct video_config *)fg, Int_val(val));
        CAMLreturn(Val_unit);
}
value fgstub_set_brightness(value fg, value val) {
        CAMLparam2 (fg, val);
        set_brightness((struct video_config *)fg, Int_val(val));
        CAMLreturn(Val_unit);
}
