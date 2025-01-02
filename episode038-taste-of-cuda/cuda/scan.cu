extern "C" {
    #include "ep38-cuda.h"
}

#define SWAP(x, y) { typeof(x) SWAP = x; x = y; y = SWAP; }

__global__ void ep38_scan_add_kernel(float* inout_d, unsigned int n) {
    __shared__ float tmp1[1024];
    __shared__ float tmp2[1024];

    unsigned int i = threadIdx.x;

    float* r = tmp1;
    float* w = tmp2;

    r[i] = inout_d[i];
    __syncthreads();

    for(unsigned int stride = 1; stride < n; stride *= 2) {
        // After `k` iterations, w[i] contains the sum of the `2^k` elements
        // of `r`, leading up to (including) r[i].
        if(i >= stride) {
            w[i] = r[i] + r[i - stride];
        } else {
            w[i] = r[i];
        }
        __syncthreads();

        SWAP(r, w);
    }

    inout_d[i] = r[i];
}

extern "C" {
    void ep38_scan_add(float* inp_h, float* out_h, unsigned int n) {
        float* inout_d = NULL;
        unsigned int numBytes = n * sizeof(float);
        cudaMalloc((void**) &inout_d, numBytes);
        cudaMemcpy(inout_d, inp_h, numBytes, cudaMemcpyHostToDevice);

        ep38_scan_add_kernel <<<1,n>>> (inout_d, n);

        cudaMemcpy(out_h, inout_d, numBytes, cudaMemcpyDeviceToHost);
        cudaFree(inout_d);
    }
}