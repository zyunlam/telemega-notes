__flash = 0x20000000;
__flash_size = 12k;
__ram   = 0x20003000;
__ram_size = 8k;
__stack_size = 512;

INCLUDE registers.ld
INCLUDE picolibc.ld
