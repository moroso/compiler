fn rol(n: u32, amt: u8) -> u32 {
    if amt == 0 {
        // this is to prevent overflow in this case.
        n
    } else {
        (n << (amt as usize)) | (n >> ((32 - amt) as usize))
    }
}

pub fn ror(n: u32, amt: u8) -> u32 {
    if amt == 0 {
        n
    } else {
        rol(n, 32 - amt)
    }
}

/// Several instructions are encoded with a value and an even shift amount.
/// Given a number `n` and amount `width` in which it must fit, this function
/// will either return the value (fitting into `width`) bits together with
/// the corresponding shift amount divided by 2, or None if it cannot be done.
// We do this by trying all rotations and seeing if any of them works.
// Yeah, we could be more clever about it, but there's really no need.
pub fn pack_int(n: u32, width: u8) -> Option<(u32, u8)> {
    for i in 0u8 .. 16u8 {
        let new_n = rol(n, 2*i);
        let mut max_bit: u8 = 0;
        for j in 0u8 .. 32u8 {
            if new_n & (1 << (j as usize)) != 0 { max_bit = j }
        }
        if max_bit < width {
            return Some((new_n, i));
        }
    }

    None
}

pub fn fits_in_bits(num: u32, size: u8) -> bool {
    let mask: u32 = !0u32 << (size as usize);
    (num & mask) == 0
}
