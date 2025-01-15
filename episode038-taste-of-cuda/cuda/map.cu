extern "C" {
    #include "ep38-cuda.h"
}

__global__ void ep38_map_add_kernel(float x, float* inout_d) {
    unsigned int i = threadIdx.x;
    inout_d[i] += x;
}

extern "C" {
    void ep38_map_add(float x, float* inp_h, float* out_h, unsigned int n) {
        float* inout_d = NULL;
        unsigned int numBytes = n * sizeof(float);
        cudaMalloc((void**) &inout_d, numBytes);
        cudaMemcpy(inout_d, inp_h, numBytes, cudaMemcpyHostToDevice);

        ep38_map_add_kernel <<<1,n>>> (x, inout_d);

        cudaMemcpy(out_h, inout_d, numBytes, cudaMemcpyDeviceToHost);
        cudaFree(inout_d);
    }
}