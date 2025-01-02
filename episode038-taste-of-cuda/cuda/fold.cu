extern "C" {
    #include "ep38-cuda.h"
}

#define FULL_WARP 0xffffffff

__global__ void ep38_fold_add_kernel(float* inout_d) {
    unsigned int i = threadIdx.x;

    for (unsigned int stride = blockDim.x; stride > 16; stride /= 2) {
        if(i < stride) {
            inout_d[i] += inout_d[i + stride];
        }
        __syncthreads();
    }
}

__global__ void ep38_fold_add_warp(float* inout_d, unsigned int n) {
    unsigned int i = threadIdx.x;
    float val = 0;

    if(i < n) {
        val = inout_d[i];
    }

    for (int stride = 16; stride > 0; stride /= 2) {
        val += __shfl_down_sync(FULL_WARP, val, stride);
    }

    if(i == 0) {
        inout_d[i] = val;
    }
}

extern "C" {
    float ep38_fold_add(float* inp_h, unsigned int n) {
        float* inout_d = NULL;
        unsigned int numBytes = n * sizeof(float);
        cudaMalloc((void**) &inout_d, numBytes);
        cudaMemcpy(inout_d, inp_h, numBytes, cudaMemcpyHostToDevice);

        if(n <= 32) {
            ep38_fold_add_warp <<<1,32>>> (inout_d, n);
        } else {
            // https://jameshfisher.com/2018/03/30/round-up-power-2/
            unsigned int num_threads = 1 << (31 - __builtin_clz(n-1));
            ep38_fold_add_kernel <<<1,num_threads>>> (inout_d);
            ep38_fold_add_warp <<<1,32>>> (inout_d, n);
        }

        float result_h;
        cudaMemcpy(&result_h, inout_d, sizeof(float), cudaMemcpyDeviceToHost);
        cudaFree(inout_d);
        return result_h;
    }
}