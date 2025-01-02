.PHONY: all
all: libep38-cuda.so ep38-cuda.pc

libep38-cuda.so: map.o fold.o scan.o
	gcc -shared -o libep38-cuda.so map.o fold.o scan.o

%.o: %.cu
	nvcc -c --compiler-options '-fPIC' -o $@ $<

ep38-cuda.pc: ep38-cuda.pc.in
	echo "prefix=${PWD}" >ep38-cuda.pc
	cat ep38-cuda.pc.in >>ep38-cuda.pc

.PHONY: clean
clean:
	rm -f *.so *.o *.pc
