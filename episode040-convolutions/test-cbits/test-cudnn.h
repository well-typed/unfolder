#pragma once

#include "cudnn.h"

/**
 * Convolution
 *
 * NOTE: This is just for testing purposes, and we take various shortcuts.
 * Not intended for production use.
 */
float* test_cudnn_convolve(
  int num_kernels, int kernel_height, int kernel_width,
  float* kernel,
  int num_images, int input_channels, int input_height, int input_width,
  float* input,
  int* output_height, int* output_width
);
