/*
 * Test quickcam: Logitech QuickCam Express Video Camera driver test tool.
 * Copyright (C) 2001 Nikolas Zimmermann
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *                                                                            
 * <wildfox@kde.org>
 * 
 */

#include <sys/ioctl.h>
#include <sys/mman.h>
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <fcntl.h>
#include <linux/videodev.h>

struct video_config {
	int device_fd;
	struct video_capability vidcap;
	struct video_window vidwin;
	struct video_picture vidpic;
	struct video_clip vidclips[32];
        struct video_mmap vm;
};

struct video_config * init_video_config(int width, int height) {
	struct video_config * out;
	out = (struct video_config *)malloc(sizeof(struct video_config));
    	memset(out, 0, sizeof(struct video_config));
	out->vm.width = width;
	out->vm.height = height;
	return out;
}
void delete_video_config(struct video_config * self) {
	free(self);
}

#define VERSION "$Id: testquickcam.c,v 1.3 2004/07/28 10:13:12 tuukkat Exp $"

/*int resolution = -1; */

int open_camera(struct video_config * self, const char *devicename)
{
    self->device_fd = open(devicename, O_RDWR);
    if(self->device_fd <= 0)
    {
	printf("Device %s couldn't be opened\n", devicename);
	return 0;
    }
    return 1;
}

void close_camera(struct video_config * self)
{
    close(self->device_fd);
}

void get_camera_info(struct video_config * self)
{
    ioctl(self->device_fd, VIDIOCGCAP,  &(self->vidcap));
    ioctl(self->device_fd, VIDIOCGWIN,  &(self->vidwin));
    ioctl(self->device_fd, VIDIOCGPICT, &(self->vidpic));
    
    self->vidwin.clips = self->vidclips;
    self->vidwin.clipcount = 0;
}

void print_camera_info(struct video_config * self)
{

    printf("    *** Camera Info ***\n");
    printf("Name:           %s\n", self->vidcap.name);
    printf("Type:           %d\n", self->vidcap.type);
    printf("Minimum Width:  %d\n", self->vidcap.minwidth);
    printf("Maximum Width:  %d\n", self->vidcap.maxwidth);
    printf("Minimum Height: %d\n", self->vidcap.minheight);
    printf("Maximum Height: %d\n", self->vidcap.maxheight);
    printf("X:              %d\n", self->vidwin.x);
    printf("Y:              %d\n", self->vidwin.y);
    printf("Width:          %d\n", self->vidwin.width);
    printf("Height:         %d\n", self->vidwin.height);
    printf("Depth:          %d\n", self->vidpic.depth);

    if(self->vidcap.type & VID_TYPE_MONOCHROME)
	printf("Color           false\n");
    else
	printf("Color           true\n");	
    printf("Version:        %s\n", VERSION);
}

static void save_ppm(struct video_config * self, const unsigned char *data)
{
    FILE *fp;
    int len, rc;
    int  i;
    struct video_mmap *vm = &(self->vm);
    len = vm->width * vm->height * 3;
    printf("Save file in %dx%d (len=%d)\n", vm->width, vm->height, len);

    fp = fopen("pic.ppm", "w");
    if(!fp) return;
    fprintf(fp, "P6\n%3d %3d\n255\n", vm->width, vm->height);

 {
   char *p = malloc(len);
    for(i=0; i<len;i+=3) {
      p[i+0] = data[i+2];
      p[i+1] = data[i+1];
      p[i+2] = data[i];
    }
    rc = fwrite(p, len, 1, fp);
    free(p);
    if(rc < 0) printf("error %d\n", rc);
 }
    fclose(fp);
}

#if 0
static void hexdump_data(const unsigned char *data, int len)
{
    const int bytes_per_line = 32;
    char tmp[128];
    int i = 0, k = 0;

    for(i = 0; len > 0; i++, len--)
    {
	if(i > 0 && ((i % bytes_per_line) == 0))
	{
    	    printf("%s\n", tmp);
            k = 0;
        }
        if ((i % bytes_per_line) == 0)
    	    k += sprintf(&tmp[k], "[%04x]: ", i);
        k += sprintf(&tmp[k], "%02x ", data[i]);
    }
    
    if (k > 0)

	printf("%s\n", tmp);
}
#endif

int set_res( struct video_config * self, int width, int height )
{
    struct video_channel vidchan;
    int r;
    struct video_mmap *vm = &(self->vm);
    vm->width  = width; /* 324 */
    vm->height = height;/* 248 */
    ioctl(self->device_fd, VIDIOCGCAP, &(self->vidcap));
    memset(&(self->vidwin), 0, sizeof(self->vidwin));

    self->vidwin.width = vm->width;
    self->vidwin.height = vm->height;

    if((self->vidwin.width >  self->vidcap.maxwidth) ||
       (self->vidwin.height > self->vidcap.maxheight)) {
      self->vidwin.width  = self->vidcap.maxwidth;
      self->vidwin.height = self->vidcap.maxheight;
    }
    r=ioctl(self->device_fd, VIDIOCSWIN, &(self->vidwin));
    if (r!=0) { perror("ioctl VIDIOCSWIN"); exit(1); }

    r=ioctl(self->device_fd, VIDIOCGWIN, &(self->vidwin));
    if (r!=0) { perror("ioctl VIDIOCGWIN"); exit(1); }
    vm->width =  self->vidwin.width;
    vm->height = self->vidwin.height;

    vidchan.channel = 0;
    r=ioctl(self->device_fd, VIDIOCGCHAN, &(vidchan));
    if (r!=0) { perror("ioctl VIDIOCGCHAN"); exit(1); }
    r=ioctl(self->device_fd, VIDIOCSCHAN, &(vidchan));
    if (r!=0) { perror("ioctl VIDIOCSCHAN"); exit(1); }
    r=ioctl(self->device_fd, VIDIOCGPICT, &(self->vidpic));
    if (r!=0) { perror("ioctl VIDIOCGPICT"); exit(1); }
    self->vidpic.depth = 24;
    self->vidpic.palette = VIDEO_PALETTE_RGB24;
    r=ioctl(self->device_fd, VIDIOCSPICT, &(self->vidpic));
    if (r!=0) { perror("ioctl VIDIOCSPICT"); exit(1); }
    r=ioctl(self->device_fd, VIDIOCGPICT, &(self->vidpic));
    if (r!=0) { perror("ioctl VIDIOCGPICT"); exit(1); }
    print_camera_info(self);
	return 1;
}

/* Buffer should be width * height * 3 */
void read_read(struct video_config * self, unsigned char *buffer, int write_ppm)
{
/*   unsigned char *buffer; */
    int len = 0;

    set_res(self,self->vm.width,self->vm.height);
/*    buffer = malloc(vm.width * vm.height * 3); */

    len = read(self->device_fd, buffer, self->vm.width * self->vm.height * 3);
    if(write_ppm)
    {
	save_ppm(self, buffer);
    }
/*    free(buffer); */
}

void mmap_read(struct video_config * self, unsigned char *outbuffer, int write_ppm)
{
    struct video_mbuf vidbuf;
    int numframe = 0;
    unsigned char *buffer;
    ioctl(self->device_fd, VIDIOCGMBUF, &(vidbuf));
    buffer = mmap(NULL, vidbuf.size, PROT_READ, MAP_SHARED, (self->device_fd), 0);
    memset(outbuffer, 0, self->vm.width * self->vm.height * 3);
    memcpy(outbuffer, buffer, vidbuf.size);
    self->vm.format = VIDEO_PALETTE_RGB24;
    self->vm.frame  = 0;

    set_res(self, self->vm.width, self->vm.height);

    if(ioctl(self->device_fd, VIDIOCMCAPTURE, &(self->vm)) < 0) {
	printf("Error in VIDIOCMCAPTURE\n");
    }

    if(ioctl(self->device_fd, VIDIOCSYNC, &numframe) < 0) {
	printf("Error in VIDIOCSYNC\n");
    }

    if(write_ppm)
    {
	save_ppm(self, buffer);
    }
}

void read_loop(struct video_config * self)
{
    int loop = 0;
    unsigned char * buffer;
    buffer = (unsigned char *)malloc((sizeof(unsigned char))*self->vm.width * self->vm.height * 3);
    while(1)
    {
	loop++;
	read_read(self,buffer,1);
	printf("*** Read frames: %d times!\n", loop);
    }
    free(buffer);
}

int main(int argc, char *argv[])
{
    struct video_config * camera;
    if(argc == 1)
    {
	printf(" *** Usage ***\n");
	printf("./testquickcam DEVICE [ -r | -m | -l ] [ -0 | -1 | -2 | -3 | -4 | -5 ]\n\n");
	printf(" -r reads one frame via read() from the camera\n");
	printf(" -m reads one frame via mmap() from the camera\n");
	printf(" -l read() loop...good for debugging gain etc \n");
	printf(" -0-5 set resulution\n");
	exit(1);
    }
    
    camera = init_video_config(324,248);
    if(open_camera(camera,argv[1]) == 1)
    {
	get_camera_info(camera);
	print_camera_info(camera);
	read_loop(camera);
	close_camera(camera);
    }
    delete_video_config(camera);
    exit(1);
}

