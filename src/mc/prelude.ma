_start:
        { bl main; r30 <- 0x1000; }
        { r30 <- 0; }
        { break 0x1f; }

print_int:
print_uint:
        { r1 <- r30; }
        { r30 <- 1; }
        { break 0x1f; r31 <- r31 + 16; }
        { b r31; r30 <- r1; }
