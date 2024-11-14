#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#include "ep36.h"

/*******************************************************************************
 * Internal auxiliary
 ******************************************************************************/

// How long do we need to spin to wait one second?
//
// This is just for demonstration purposes; this is roughly right on my specific
// CPU (AMD Ryzen 9 5950X).
#define ONE_SECOND 2350000000

// https://stackoverflow.com/a/58758133/742991
void spin(double s) {
    for (uint64_t i = 0; i < s * ONE_SECOND; i++) {
        __asm__ __volatile__ ("" : "+g" (i) : :);
    }
}

/*******************************************************************************
 * Exported functions
 *
 * NOTE: This is for demonstration purposes only. Proper implementation of
 * interruptible FFI functions is more involved than we are showing here. For
 * example, if the signal happens to be delivered while `sleepy` is not
 * currently sleeping, it will simply be lost.
 ******************************************************************************/

void sleepy(double s, int n, char c) {
    for (char i = 0; i < n; i++) {
      putchar(c);
      fflush(stdout);
      usleep(s * 1000000);
      if(errno == EINTR) {
        return;
      }
    }
}

void busy(double s, int n, char c) {
    for (char i = 0; i < n; i++) {
      putchar(c);
      fflush(stdout);
      spin(s);
    }
}

void sleepyBuf(volatile char* buf, double s, int n, char c) {
    for (char i = 0; i < n; i++) {
      putchar(c);
      fflush(stdout);
      *(buf + i) = c;
      usleep(s * 1000000);
      if(errno == EINTR) {
        return;
      }
    }

    // Check that nobody else touched our buffer in the meantime
    printf(" (%d, %d)\n", buf[0], buf[n - 1]);
    fflush(stdout);
}
