/// Hardware Serial Configuration Driver
///
/// Configures the raw UART settings for the safety-critical radar sensor connection.
///
/// Failure Modes:
/// * Silent data corruption due to parity or framing errors in noisy environments.
/// * Port lockup due to incorrect flow control or canonical mode settings.
///
/// Mitigations:
/// * Enforces raw 8N1 transmission with no software flow control.
/// * Disables all special character handling (ECHO, ISIG) to prevent parsing bugs.
///
/// Traceability:
/// * Requirement FR-DAQ-003: Robust sensor telemetry
/// * Hazard H-SOUP-002: Malformed serial input

#include "hardware_manifest.h"
#include <cstdio>
#include <cstring>
#include <errno.h>
#include <fcntl.h>
#include <termios.h>
#include <unistd.h>

extern "C" {

int configure_serial_port(int fd, int baud_rate) {
  struct termios tty;
  if (tcgetattr(fd, &tty) != 0) {
    if (errno == ENOTTY) {
      return 2; // Simulation Mode
    }
    return -1;
  }

  speed_t speed;
  if (baud_rate == MANIFEST_CONFIG_BAUD) {
    speed = MANIFEST_CONFIG_BAUD_MACRO;
  } else if (baud_rate == MANIFEST_DATA_BAUD) {
    speed = MANIFEST_DATA_BAUD_MACRO;
  } else {
    return -2; // Specific error for unsupported baud rate
  }

  cfsetospeed(&tty, speed);
  cfsetispeed(&tty, speed);

  // 8N1
  tty.c_cflag &= ~PARENB; // No parity
  tty.c_cflag &= ~CSTOPB; // 1 stop bit
  tty.c_cflag &= ~CSIZE;
  tty.c_cflag |= CS8; // 8 data bits

  // No flow control
  tty.c_cflag &= ~CRTSCTS;

  // Turn on READ & ignore ctrl lines (CLOCAL = 1)
  tty.c_cflag |= CREAD | CLOCAL;

  // Disable Canonical Mode (Raw Mode)
  tty.c_lflag &= ~ICANON;
  tty.c_lflag &= ~ECHO;   // Disable echo
  tty.c_lflag &= ~ECHOE;  // Disable erasure
  tty.c_lflag &= ~ECHONL; // Disable new-line echo
  tty.c_lflag &= ~ISIG;   // Disable interpretation of INTR, QUIT and SUSP

  // Disable software flow control
  tty.c_iflag &= ~(IXON | IXOFF | IXANY);
  // Disable special handling of received bytes
  tty.c_iflag &= ~(IGNBRK | BRKINT | PARMRK | ISTRIP | INLCR | IGNCR | ICRNL);

  // Raw Output
  tty.c_oflag &= ~OPOST; // Prevent special interpretation of output bytes (e.g.
                         // newline chars)
  tty.c_oflag &=
      ~ONLCR; // Prevent conversion of newline to carriage return/line feed

  // Blocking read settings
  // VMIN = 1 (read at least 1 byte), VTIME = 0 (no timeout)
  tty.c_cc[VMIN] = 1;
  tty.c_cc[VTIME] = 0;

  if (tcsetattr(fd, TCSANOW, &tty) != 0) {
    return -1;
  }
  return 0;
}
}
