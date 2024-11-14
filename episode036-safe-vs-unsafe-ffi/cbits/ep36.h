#ifndef EP36_H
#define EP36_H

#include <stdint.h>

void sleepy(double s, int n, char c);
void busy(double s, int n, char c);

void sleepyBuf(volatile char* buf, double s, int n, char c);

#endif