#include <stdio.h>
#include <stdint.h>

uint8_t add(uint8_t a, uint8_t b);
uint32_t max_u32();

int main(void) {
    printf("Three: %u\n", add(10, 5));
    printf("Max: %u\n", max_u32());
    return 0;
}
