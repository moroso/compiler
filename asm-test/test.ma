.globl alphabet
.globl putchar

alphabet:
        { r0 <- 'A' ;
          r1 <- 25 ;
          r6 <- lr }
.1:     { bl putchar ;
          r4 <- r0 + 1 ;
          r5 <- r1 - 1 ;
          p0 <- r1 < 0 }
        { !p0? b .1b ;
          r0 <- r4;
          r1 <- r5 }
        { b lr ;
          lr <- r6 }
 
putchar:
        { r8 <- long; long 0x81230000 ; r10 <- 1 }
        { r9 <- *l(r8 + 0) }
.1:     { *l(r8 + 4) <- r0 ;
          p1 <- r10 == (r9 >>u 1) }
        { p1? b lr ;
          !p1? r9 <- *l(r8 + 0) }
        { !p1? b .1b }
