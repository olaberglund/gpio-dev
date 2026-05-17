#include <linux/gpio.h>
#include <sys/ioctl.h>
#include <fcntl.h>
#include <string.h>
#include <stdio.h>
#include <unistd.h>

int main(int argc, char *argv[])
{
  int chip_fd;
  chip_fd = open("/dev/gpiochip0", O_RDWR);

  struct gpio_v2_line_request req =
    { .offsets = {3} // GPIO3
    , .consumer = "gpio_blink"
    , .config = { .flags = GPIO_V2_LINE_FLAG_OUTPUT }
    , .num_lines = 1
    };

  ioctl(chip_fd, GPIO_V2_GET_LINE_IOCTL, &req);

  struct gpio_v2_line_values line_values =
    { .bits = 1 // set index 0 to high
    , .mask = 1 // set for index 0 in offsets
    };

  ioctl(req.fd, GPIO_V2_LINE_SET_VALUES_IOCTL, &line_values);

  sleep(1);

  line_values.bits = 0;
  ioctl(req.fd, GPIO_V2_LINE_SET_VALUES_IOCTL, &line_values);

  close(chip_fd);
  close(req.fd);

  return 0;
}
