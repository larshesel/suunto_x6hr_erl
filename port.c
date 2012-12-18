
/* port.c */
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <stdarg.h>


void logger(const char *fmt, ...);

typedef unsigned char byte;

void write_to_suunto(int fd, byte* buf, int len) {
}

int read_from_suunto(int fd, byte* buf) {
  return 0;
}


int main() {
  const char* suunto_dev_name = "/dev/ttyUSB1";
  int fd = open(suunto_dev_name, O_RDWR);
  if (fd == -1) {
    logger("open suunto device failed.\n");
    return(1);
  } else {
    logger("open suunto device OK.\n");
  }

  // read/write buffer
  byte buf[0xffff];
  
  int len = read_cmd(buf);
  while (len > 0) {
    write_to_suunto(fd, buf, len);
    //len = read_from_suunto(fd, buf);
    logger("writing len %d\n", len);
    write_cmd(buf, len);

    // get ready for next loop
    len = read_cmd(buf);
  }
  int res = close(fd);
  if (res == 0) {
    logger("close of suunto device OK\n");
  } else {
    logger("close of suunto device failed with code: %d\n", res);
  }
}

void logger(const char *fmt, ...)
{
   FILE* f = fopen("./port.log", "a+");
   va_list args;

   va_start(args, fmt);
   vfprintf(f, fmt, args);
   va_end(args);
   fclose(f);
}
