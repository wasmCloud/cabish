use core::alloc::Layout;
use core::ffi::c_void;
use core::iter::zip;
use core::ptr::{copy_nonoverlapping, null, NonNull};

use std::alloc::{alloc, alloc_zeroed};

use anyhow::{bail, ensure, Context as _};
use cabish::{find_enum_discriminant, flag_bits};
use tracing::{instrument, trace};
use wasmtime::component::{Type, Val};
use wasmtime::Store;
use wasmtime_wasi::WasiView;

use crate::{
    align_of, align_of_result, find_variant_discriminant, max_case_alignment, size_of,
    size_of_option, size_of_result, size_of_variant,
};

#[instrument(level = "debug", skip(store, ty, src), ret(level = "debug"))]
fn lower(
    store: &mut Store<impl WasiView>,
    ty: &Type,
    dst: NonNull<c_void>,
    src: Val,
) -> anyhow::Result<*mut c_void> {
    match (src, ty) {
        (Val::Bool(val), Type::Bool) => {
            let dst = dst.cast();
            unsafe { dst.write(val) };
            Ok(dst.as_ptr().wrapping_add(1).cast())
        }
        (Val::S8(val), Type::S8) => {
            let dst = dst.cast();
            unsafe { dst.write(val) };
            Ok(dst.as_ptr().wrapping_add(1).cast())
        }
        (Val::U8(val), Type::U8) => {
            let dst = dst.cast();
            unsafe { dst.write(val) };
            Ok(dst.as_ptr().wrapping_add(1).cast())
        }
        (Val::S16(val), Type::S16) => {
            let dst = dst.cast();
            unsafe { dst.write(val) };
            Ok(dst.as_ptr().wrapping_add(1).cast())
        }
        (Val::U16(val), Type::U16) => {
            let dst = dst.cast();
            unsafe { dst.write(val) };
            Ok(dst.as_ptr().wrapping_add(1).cast())
        }
        (Val::S32(val), Type::S32) => {
            let dst = dst.cast();
            unsafe { dst.write(val) };
            Ok(dst.as_ptr().wrapping_add(1).cast())
        }
        (Val::U32(val), Type::U32) => {
            let dst = dst.cast();
            unsafe { dst.write(val) };
            Ok(dst.as_ptr().wrapping_add(1).cast())
        }
        (Val::S64(val), Type::S64) => {
            let dst = dst.cast();
            unsafe { dst.write(val) };
            Ok(dst.as_ptr().wrapping_add(1).cast())
        }
        (Val::U64(val), Type::U64) => {
            let dst = dst.cast();
            unsafe { dst.write(val) };
            Ok(dst.as_ptr().wrapping_add(1).cast())
        }
        (Val::Float32(val), Type::Float32) => {
            let dst = dst.cast();
            unsafe { dst.write(val) };
            Ok(dst.as_ptr().wrapping_add(1).cast())
        }
        (Val::Float64(val), Type::Float64) => {
            let dst = dst.cast();
            unsafe { dst.write(val) };
            Ok(dst.as_ptr().wrapping_add(1).cast())
        }
        (Val::Char(val), Type::Char) => {
            let dst = dst.cast();
            unsafe { dst.write(val) };
            Ok(dst.as_ptr().wrapping_add(1).cast())
        }
        (Val::String(val), Type::String) => {
            let dst = dst.cast::<(*const u8, usize)>();
            if val.is_empty() {
                unsafe { dst.write((null(), 0)) }
            } else {
                let len = val.len();
                let layout = Layout::from_size_align(len, 1)
                    .context("failed to construct string memory layout")?;
                trace!(?layout, "allocating string");
                let data = unsafe { alloc(layout) };
                ensure!(!data.is_null(), "failed to allocate list");
                unsafe { copy_nonoverlapping(val.as_ptr(), data, len) }
                trace!(?data, len, "writing string");
                unsafe { dst.write((data, len)) }
            }
            Ok(dst.as_ptr().wrapping_add(1).cast())
        }
        (Val::List(vals), Type::List(ty)) => {
            let dst = dst.cast::<(*const c_void, usize)>();
            if vals.is_empty() {
                unsafe { dst.write((null(), 0)) }
            } else {
                let ty = ty.ty();
                let len = vals.len();
                let size = size_of(&ty).saturating_mul(len);
                let align = align_of(&ty);
                let layout = Layout::from_size_align(size, align)
                    .context("failed to construct list memory layout")?;
                trace!(?layout, "allocating list");
                let start = unsafe { alloc_zeroed(layout) }.cast::<c_void>();
                ensure!(!start.is_null(), "failed to allocate list");
                let mut data = start;
                for (i, val) in vals.into_iter().enumerate() {
                    let dst = NonNull::new(data)
                        .with_context(|| format!("list element `{i}` cannot be null"))?;
                    data = lower(store, &ty, dst, val)
                        .with_context(|| format!("failed to lower list element `{i}`"))?;
                }
                trace!(?start, len, "writing list");
                unsafe { dst.write((start, len)) }
            }
            Ok(dst.as_ptr().wrapping_add(1).cast())
        }
        (Val::Record(vals), Type::Record(ty)) => {
            let mut data = dst.as_ptr();
            for (i, (ty, (_, val))) in zip(ty.fields(), vals).enumerate() {
                let offset = data.cast::<u8>().align_offset(align_of(&ty.ty));
                if offset > 0 {
                    data = data.wrapping_add(offset).cast();
                }
                let dst = NonNull::new(data)
                    .with_context(|| format!("record field `{i}` cannot be null"))?;
                data = lower(store, &ty.ty, dst, val)
                    .with_context(|| format!("failed to lower record field `{i}`"))?;
            }
            Ok(data)
        }
        (Val::Tuple(vals), Type::Tuple(ty)) => {
            let mut data = dst.as_ptr();
            for (i, (ty, val)) in zip(ty.types(), vals).enumerate() {
                let offset = data.cast::<u8>().align_offset(align_of(&ty));
                if offset > 0 {
                    data = data.wrapping_add(offset).cast();
                }
                let dst = NonNull::new(data)
                    .with_context(|| format!("tuple element `{i}` cannot be null"))?;
                data = lower(store, &ty, dst, val)
                    .with_context(|| format!("failed to lower tuple element `{i}`"))?;
            }
            Ok(data)
        }
        (Val::Variant(disc, val), Type::Variant(ty)) => {
            let cases = ty.cases();
            let data = dst.cast::<u8>().as_ptr().wrapping_add(size_of_variant(ty));
            let align = max_case_alignment(ty.cases());
            let (mut dst, ty) = match cases.len() {
                ..=0x0000_00ff => {
                    let (disc, ty) = find_variant_discriminant(0u8.., cases, &disc)?;
                    let dst = dst.cast();
                    unsafe { dst.write(disc) };
                    (dst.as_ptr().wrapping_add(1).cast::<c_void>(), ty)
                }
                0x0000_0100..=0x0000_ffff => {
                    let (disc, ty) = find_variant_discriminant(0u16.., cases, &disc)?;
                    let dst = dst.cast();
                    unsafe { dst.write(disc) };
                    (dst.as_ptr().wrapping_add(1).cast::<c_void>(), ty)
                }
                0x0001_0000..=0xffff_ffff => {
                    let (disc, ty) = find_variant_discriminant(0u32.., cases, &disc)?;
                    let dst = dst.cast();
                    unsafe { dst.write(disc) };
                    (dst.as_ptr().wrapping_add(1).cast::<c_void>(), ty)
                }
                0x1_0000_0000.. => bail!("variant case count does not fit in u32"),
            };
            if let Some((ty, val)) = ty.zip(val) {
                let offset = dst.cast::<u8>().align_offset(align);
                if offset > 0 {
                    dst = dst.wrapping_add(offset).cast();
                }
                let dst = NonNull::new(dst).with_context(|| {
                    format!("variant value for discriminant `{disc}` cannot be null")
                })?;
                lower(store, &ty, dst, *val).with_context(|| {
                    format!("failed to lower variant value for discriminant `{disc}`")
                })?;
            }
            Ok(data.cast())
        }
        (Val::Enum(disc), Type::Enum(ty)) => {
            let names = ty.names();
            match names.len() {
                ..=0x0000_00ff => {
                    let disc = find_enum_discriminant(0u8.., names, &disc)?;
                    let dst = dst.cast();
                    unsafe { dst.write(disc) };
                    Ok(dst.as_ptr().wrapping_add(1).cast())
                }
                0x0000_0100..=0x0000_ffff => {
                    let disc = find_enum_discriminant(0u16.., names, &disc)?;
                    let dst = dst.cast();
                    unsafe { dst.write(disc) };
                    Ok(dst.as_ptr().wrapping_add(1).cast())
                }
                0x0001_0000..=0xffff_ffff => {
                    let disc = find_enum_discriminant(0u32.., names, &disc)?;
                    let dst = dst.cast();
                    unsafe { dst.write(disc) };
                    Ok(dst.as_ptr().wrapping_add(1).cast())
                }
                0x1_0000_0000.. => bail!("enum name count does not fit in u32"),
            }
        }
        (Val::Option(val), Type::Option(ty)) => {
            let dst = dst.cast::<u8>();
            let data = dst.as_ptr().wrapping_add(size_of_option(ty));
            match val {
                None => {
                    unsafe { dst.write(0) };
                }
                Some(val) => {
                    unsafe { dst.write(1) };
                    let mut dst = dst.as_ptr().wrapping_add(1);
                    let offset = dst.align_offset(align_of(&ty.ty()));
                    if offset > 0 {
                        dst = dst.wrapping_add(offset);
                    }
                    let dst = NonNull::new(dst.cast())
                        .context("`option::some` payload cannot be null")?;
                    lower(store, &ty.ty(), dst, *val).context("failed to lower `option::some`")?;
                }
            }
            Ok(data.cast())
        }
        (Val::Result(val), Type::Result(ty)) => {
            let dst = dst.cast::<u8>();
            let data = dst.as_ptr().wrapping_add(size_of_result(ty));
            let align = align_of_result(ty);
            match val {
                Ok(val) => {
                    unsafe { dst.write(0) };
                    if let Some((ty, val)) = ty.ok().zip(val) {
                        let mut dst = dst.as_ptr().wrapping_add(1);
                        let offset = dst.align_offset(align);
                        if offset > 0 {
                            dst = dst.wrapping_add(offset);
                        }
                        let dst = NonNull::new(dst.cast())
                            .context("typed `result::ok` payload cannot be null")?;
                        lower(store, &ty, dst, *val).context("failed to lower `result::ok`")?;
                    }
                }
                Err(val) => {
                    unsafe { dst.write(1) };
                    if let Some((ty, val)) = ty.err().zip(val) {
                        let mut dst = dst.as_ptr().wrapping_add(1);
                        let offset = dst.align_offset(align);
                        if offset > 0 {
                            dst = dst.wrapping_add(offset);
                        }
                        let dst = NonNull::new(dst.cast())
                            .context("typed `result::err` payload cannot be null")?;
                        lower(store, &ty, dst, *val).context("failed to lower `result::error`")?;
                    }
                }
            }
            Ok(data.cast())
        }
        (Val::Flags(val), Type::Flags(ty)) => {
            let names = ty.names();
            let vs = val.iter().map(String::as_str);
            match names.len() {
                ..=8 => {
                    let v: u8 = flag_bits(names, vs);
                    let dst = dst.cast();
                    unsafe { dst.write(v) };
                    Ok(dst.as_ptr().wrapping_add(1).cast())
                }
                9..=16 => {
                    let v: u16 = flag_bits(names, vs);
                    let dst = dst.cast();
                    unsafe { dst.write(v) };
                    Ok(dst.as_ptr().wrapping_add(1).cast())
                }
                17..=32 => {
                    let v: u32 = flag_bits(names, vs);
                    let dst = dst.cast();
                    unsafe { dst.write(v) };
                    Ok(dst.as_ptr().wrapping_add(1).cast())
                }
                _ => bail!("flags with over 32 cases are not currently supported"),
            }
        }
        (Val::Resource(val), Type::Own(..) | Type::Borrow(..)) => {
            let dst = dst.cast::<u32>();
            let res = store.data_mut().table().push(val)?;
            unsafe { dst.write(res.rep()) };
            Ok(dst.as_ptr().wrapping_add(1).cast())
        }
        _ => bail!("type mismatch"),
    }
}

#[instrument(level = "debug", skip_all, ret(level = "debug"))]
pub fn lower_results(
    store: &mut Store<impl WasiView>,
    vals: Vec<Val>,
    tys: &[Type],
    args: *const *mut c_void,
) -> anyhow::Result<()> {
    if vals.is_empty() {
        return Ok(());
    }
    let args: NonNull<*mut c_void> =
        NonNull::new(args.cast_mut()).context("argument cannot be null")?;
    let data = unsafe { args.read() };
    zip(vals, tys)
        .enumerate()
        .try_fold(data, |data, (i, (val, ty))| -> anyhow::Result<_> {
            let data =
                NonNull::new(data).with_context(|| format!("result value {i} cannot be null"))?;
            lower(store, ty, data, val).with_context(|| format!("failed to lower result value {i}"))
        })?;
    Ok(())
}
