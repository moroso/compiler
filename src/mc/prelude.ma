_start:
        { bl _INIT_GLOBALS; r30 <- long; long __STACK_START__; }
        { bl __main; }
        { r30 <- 0; }
        { break 0x1f; }

print_int:
        { r1 <- r30; }
        { r30 <- 1; }
        { break 0x1f; }
        { b r31 + 1; r30 <- r1; }

print_char:
        { r1 <- r30; }
        { r30 <- 2; }
        { break 0x1f; }
        { b r31 + 1; r30 <- r1; }


// TODO: there's room for optimization here.
rt_memcpy: { p1 <- r2 == 0 }
           { p1? b r31 + 1; }
memcpy_loop:
           { r3 <- *b(r1); r2 <- r2 - 1; p0 <- r2 <=s 1; p1 <- r2 == 0; }
           { !p0? b memcpy_loop; !p1? *b(r0) <- r3; r0 <- r0 + 1;
             r1 <- r1 + 1; }
           { p0? b r31 + 1; }

rt_abort:
// TODO: better abort function.
        { b r31 + 1; }
