#include <stdio.h>
#include <stdint.h>

uint8_t buffer[32] = {0};

uint8_t* itoa(uint32_t n) {
    uint32_t length = 0;

    if (n == 0) return "0";
    while (n != 0) {
        uint32_t q = n % 10;
        n /= 10;
        buffer[32-length-1] = '0' + q;
        length += 1;
    }

    return &buffer[32-length];
}

int main(void) {
    printf("%s\n", itoa(2000));
    return 0;
}
