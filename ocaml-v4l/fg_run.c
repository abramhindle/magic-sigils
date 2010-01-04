#include <sys/ioctl.h>
#include <sys/mman.h>
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <fcntl.h>
#include "fg.h"
int test_main(int argc, char*argv[]);
int main(int argc, char *argv[]) {
	return test_main(argc,argv);
}
