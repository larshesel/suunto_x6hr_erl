
/* port.c */
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <stdarg.h>
#include <termios.h>

void logger(const char *fmt, ...);

typedef unsigned char byte;

void write_to_suunto(int fd, byte* buf, int len) {
  int i, wrote = 0;

  do {
    if ((i = write(fd, buf+wrote, len-wrote)) <= 0) {
      return;
    }
    wrote += i;
  } while (wrote<len);

  logger("wrote total bytes to suunto: %d\n", len);
}

int read_from_suunto(int fd, byte* buf) {
  int i = read(fd, buf, 0xffff);
  logger("read bytes from suunto: %d\n", i);
  return i;
}


int main() {
  struct termios termOptions;

  const char* suunto_dev_name = "/dev/ttyUSB1";
  int fd = open(suunto_dev_name, O_RDWR);
  if (fd == -1) {
    logger("open suunto device failed.\n");
    return(1);
  } else {
    logger("open suunto device OK.\n");
  }

  tcgetattr(fd, &termOptions );
  cfsetispeed(&termOptions, B9600);
  cfsetospeed(&termOptions, B9600);
  termOptions.c_cflag == B9600 | CS8 | CLOCAL | CREAD ;
  termOptions.c_iflag = 0;
  termOptions.c_oflag = 0;
  termOptions.c_lflag = 0;
  tcflush(fd, TCIFLUSH);
  tcsetattr(fd, TCSANOW, &termOptions );
  
  // read/write buffer
  byte buf[0xffff];
  
  int len = read_cmd(buf);
  while (len > 0) {
    write_to_suunto(fd, buf, len);
    usleep(100*1000); // wait 100 milliseconds for the watch to write answer
    len = read_from_suunto(fd, buf);
    logger("writing back to erlang %d bytes\n", len);
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
