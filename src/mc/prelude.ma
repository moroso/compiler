_start:
        { bl __INIT_GLOBALS; r30 <- 0x1000; }
        { bl MANGLEDmain; }
        { r30 <- 0; }
        { break 0x1f; }

MANGLEDprelude_print_int:
MANGLEDprelude_print_uint:
        { r1 <- r30; }
        { r30 <- 1; }
        { break 0x1f; }
        { b r31 + 1; r30 <- r1; }

// TODO: there's room for optimization here.
memcopy: { p1 <- r2 == 0 }
         { p1 -> b r31 + 1; }
memcopy_loop:
         { r3 <- *b(r1); r2 <- r2 - 1; p0 <- r2 <=s 1; p1 <- r2 == 0; }
         { !p0 -> b memcopy_loop; !p1 -> *b(r0) <- r3; r0 <- r0 + 1;
           r1 <- r1 + 1; }
         { p0 -> b r31 + 1; }
