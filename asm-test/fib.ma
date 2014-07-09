       { r0 <- 0 ;
         r1 <- 1 ;
         r2 <- 10 }

iter:  { nop; }
       { nop; }
       { nop; }
       { nop; }
       { nop; }
       { nop; }
       { r0 <- r0 + r1 ;
         r1 <- r0 ;
         r2 <- r2 - 1 }
       { p0 <- r2 == 0 }
       { !p0 -> b -1 }
       { break 0x1f }
