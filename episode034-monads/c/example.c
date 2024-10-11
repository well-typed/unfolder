#include <stdio.h>

int funA(int x) {
    printf("Hello ");
    return x + 1;
}

int funB(int x) {
    printf("world\n");
    return x + 2;
}

int main() {
    int x = funA(1) + funB(2);
    printf("x = %d\n", x);
    return 0;
}
