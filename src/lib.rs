use core::ffi::c_void;
use core::iter::zip;
use core::ops::{BitOrAssign, Shl};
use core::ptr::NonNull;

use std::collections::HashSet;

use anyhow::Context as _;

#[repr(C)]
#[derive(Debug)]
pub struct List<T> {
    pub ptr: *const T,
    pub len: usize,
}

pub fn flag_bits<'a, T: BitOrAssign + Shl<u8, Output = T> + From<u8>>(
    names: impl IntoIterator<Item = &'a str>,
    flags: impl IntoIterator<Item = &'a str>,
) -> T {
    let mut v = T::from(0);
    let flags: HashSet<&str> = flags.into_iter().collect();
    for (i, name) in zip(0u8.., names) {
        if flags.contains(name) {
            v |= T::from(1) << i;
        }
    }
    v
}

pub fn find_enum_discriminant<'a, T>(
    iter: impl IntoIterator<Item = T>,
    names: impl IntoIterator<Item = &'a str>,
    disc: &str,
) -> anyhow::Result<T> {
    zip(iter, names)
        .find_map(|(i, name)| (name == disc).then_some(i))
        .context("unknown enum discriminant")
}

pub fn deref_arg<T>(args: *const *mut c_void) -> anyhow::Result<(NonNull<T>, *const *mut c_void)> {
    let args: NonNull<*mut c_void> =
        NonNull::new(args.cast_mut()).context("argument cannot be null")?;
    let data = unsafe { args.read() };
    let data = NonNull::new(data.cast()).context("value cannot be null")?;
    Ok((data, args.as_ptr().wrapping_add(1)))
}
