#include <linux/videodev.h>

struct video_config {
	int device_fd;
	struct video_capability vidcap;
	struct video_window vidwin;
	struct video_picture vidpic;
	struct video_clip vidclips[32];
        struct video_mmap vm;
	unsigned char * buffer;
	int bitdepth;
	int depth;
	int palette;
	int pixels;
};

struct video_config * init_video_config(int width, int height);
void delete_video_config(struct video_config * self);
int open_camera(struct video_config * self, const char *devicename);
void close_camera(struct video_config * self);
void get_camera_info(struct video_config * self);
int print_camera_info(struct video_config * self);
void save_ppm(struct video_config * self, const unsigned char *data);
int set_res( struct video_config * self, int width, int height );
void close_camera(struct video_config * self);
void read_read(struct video_config * self, int write_ppm);
void mmap_read(struct video_config * self, int write_ppm);
int get_contrast(struct video_config *self) ;
int get_brightness(struct video_config *self) ;
int get_hue(struct video_config *self) ;
int get_colour(struct video_config *self) ;
int set_contrast(struct video_config *self, int value) ;
int set_brightness(struct video_config *self, int value) ;
int set_hue(struct video_config *self, int value) ;
int set_colour(struct video_config *self, int value);
