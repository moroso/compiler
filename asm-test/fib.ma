       { r0 <- 0 ;
         r1 <- 1 ;
         r2 <- 10 }

iter:  { r0 <- r0 + r1 ;
         r1 <- r0 ;
         r2 <- r2 - 1 }
       { p0 <- r2 == 0 }
       { !p0? b iter }
       { break 0x1f }
