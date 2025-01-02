#pragma once

void ep38_map_add(float x, float* inp_h, float* out_h, unsigned int n);
float ep38_fold_add(float* A_h, unsigned int n);
void ep38_scan_add(float* A_h, float* B_h, unsigned int n);