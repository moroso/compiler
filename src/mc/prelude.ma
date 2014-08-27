_start:
        { bl _INIT_GLOBALS; r30 <- 0x1000; }
        { bl __main; }
        { r30 <- 0; }
        { break 0x1f; }

__prelude__print_int:
        { r1 <- r30; }
        { r30 <- 1; }
        { break 0x1f; }
        { b r31 + 1; r30 <- r1; }

// TODO: there's room for optimization here.
rt_memcpy: { p1 <- r2 == 0 }
           { p1 -> b r31 + 1; }
memcpy_loop:
           { r3 <- *b(r1); r2 <- r2 - 1; p0 <- r2 <=s 1; p1 <- r2 == 0; }
           { !p0 -> b memcpy_loop; !p1 -> *b(r0) <- r3; r0 <- r0 + 1;
             r1 <- r1 + 1; }
           { p0 -> b r31 + 1; }

// TODO: the following are placeholders.
__prelude__print_char:
__prelude__rt_abort:
__prelude__rt_malloc:
__utils__buddy_alloc__machine_phys_frames:
__context__actually_switch_contexts_with_asm:
__drivers__timer_outb:
__entry__idt_base:
__entry__entry_stubs:
__entry__pic_acknowledge_any_master:
__entry__leave_kernel:
__fs__fs_img_start:
__fs__fs_img_end:
__kernel_main__get_cr0:
__kernel_main__get_cr2:
__kernel_main__get_cr3:
__kernel_main__get_cr4:
__kernel_main__set_cr0:
__kernel_main__set_cr3:
__kernel_main__set_cr4:
__kernel_main__get_esp:
__kernel_main__set_esp0:
__kernel_main__enable_interrupts:
__loader__get_eflags:
__structures__VM__invalidate_tlb:
__structures__VM__get_cr0:
__structures__VM__get_cr2:
__structures__VM__get_cr3:
__structures__VM__get_cr4:
__structures__VM__set_cr0:
__structures__VM__set_cr3:
__structures__VM__set_cr4:
__user__syslib__syscall_0:
__user__syslib__syscall_1:
__user__syslib__syscall_2:
__user__syslib__syscall_3:
        { b r31 + 1; }
