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
rt_memcpy: { p1 <- r2 == 0 }
           { p1 -> b r31 + 1; }
memcpy_loop:
           { r3 <- *b(r1); r2 <- r2 - 1; p0 <- r2 <=s 1; p1 <- r2 == 0; }
           { !p0 -> b memcpy_loop; !p1 -> *b(r0) <- r3; r0 <- r0 + 1;
             r1 <- r1 + 1; }
           { p0 -> b r31 + 1; }

// TODO: the following are placeholders.
MANGLEDprelude_printf3_:
MANGLEDprelude_printf2_:
MANGLEDprelude_printf1_:
MANGLEDprelude_printf0_:
MANGLEDprelude_print_char:
MANGLEDprelude_print_newline:
MANGLEDprelude_rt_abort:
MANGLEDprelude_rt_malloc:
MANGLEDutils_buddy_alloc_machine_phys_frames:
MANGLEDcontext_actually_switch_contexts_with_asm:
MANGLEDdrivers_timer_outb:
MANGLEDentry_idt_base:
MANGLEDentry_entry_stubs:
MANGLEDentry_pic_acknowledge_any_master:
MANGLEDentry_leave_kernel:
MANGLEDfs_fs_img_start:
MANGLEDfs_fs_img_end:
MANGLEDkernel_main_get_cr0:
MANGLEDkernel_main_get_cr2:
MANGLEDkernel_main_get_cr3:
MANGLEDkernel_main_get_cr4:
MANGLEDkernel_main_set_cr0:
MANGLEDkernel_main_set_cr3:
MANGLEDkernel_main_set_cr4:
MANGLEDkernel_main_get_esp:
MANGLEDkernel_main_set_esp0:
MANGLEDkernel_main_enable_interrupts:
MANGLEDloader_get_eflags:
MANGLEDstructures_VM_invalidate_tlb:
MANGLEDstructures_VM_get_cr0:
MANGLEDstructures_VM_get_cr2:
MANGLEDstructures_VM_get_cr3:
MANGLEDstructures_VM_get_cr4:
MANGLEDstructures_VM_set_cr0:
MANGLEDstructures_VM_set_cr3:
MANGLEDstructures_VM_set_cr4:
MANGLEDuser_syslib_syscall_0:
MANGLEDuser_syslib_syscall_1:
MANGLEDuser_syslib_syscall_2:
MANGLEDuser_syslib_syscall_3:
        { b r31 + 1; }
