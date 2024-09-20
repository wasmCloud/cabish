use core::iter::zip;
use core::mem;

use anyhow::Context as _;
use tracing::instrument;
use wasmtime::component::{types, Type};

mod lift;
mod lower;

pub use cabish::*;
pub use lift::lift_params;
pub use lower::lower_results;

#[instrument(level = "trace")]
pub fn align_of_record(ty: &types::Record) -> usize {
    ty.fields().map(|ty| align_of(&ty.ty)).max().unwrap_or(1)
}

#[instrument(level = "trace")]
pub fn align_of_tuple(ty: &types::Tuple) -> usize {
    ty.types().map(|ty| align_of(&ty)).max().unwrap_or(1)
}

#[instrument(level = "trace", skip_all)]
pub fn max_case_alignment<'a>(cases: impl IntoIterator<Item = types::Case<'a>>) -> usize {
    cases
        .into_iter()
        .filter_map(|types::Case { ty, .. }| ty.as_ref().map(align_of))
        .max()
        .unwrap_or(1)
}

#[instrument(level = "trace")]
pub fn align_of_variant(ty: &types::Variant) -> usize {
    let cases = ty.cases();
    let disc = match cases.len() {
        ..=0x0000_00ff => 1,
        0x0000_0100..=0x0000_ffff => 2,
        0x0001_0000.. => 4,
    };
    max_case_alignment(cases).max(disc)
}

#[instrument(level = "trace")]
pub fn align_of_option(ty: &types::OptionType) -> usize {
    align_of(&ty.ty())
}

#[instrument(level = "trace")]
pub fn align_of_result(ty: &types::ResultType) -> usize {
    let ok = ty.ok().as_ref().map_or(1, align_of);
    let err = ty.err().as_ref().map_or(1, align_of);
    ok.max(err)
}

#[instrument(level = "trace")]
pub fn align_of(ty: &Type) -> usize {
    match ty {
        Type::Bool | Type::S8 | Type::U8 => 1,
        Type::S16 | Type::U16 => 2,
        Type::S32 | Type::U32 | Type::Float32 | Type::Char | Type::Own(_) | Type::Borrow(_) => 4,
        Type::S64 | Type::U64 | Type::Float64 => 8,
        Type::String | Type::List(_) => mem::align_of::<(*const (), usize)>(),
        Type::Record(ty) => align_of_record(ty),
        Type::Tuple(ty) => align_of_tuple(ty),
        Type::Variant(ty) => align_of_variant(ty),
        Type::Enum(ty) => match ty.names().len() {
            ..=0x0000_00ff => 1,
            0x0000_0100..=0x0000_ffff => 2,
            0x0001_0000.. => 4,
        },
        Type::Option(ty) => align_of_option(ty),
        Type::Result(ty) => align_of_result(ty),
        Type::Flags(ty) => match ty.names().len() {
            ..=8 => 1,
            9..=16 => 2,
            _ => 4,
        },
    }
}

#[instrument(level = "trace")]
pub fn align_to(addr: usize, align: usize) -> usize {
    addr.div_ceil(align).saturating_mul(align)
}

#[instrument(level = "trace")]
pub fn size_of_record(ty: &types::Record) -> usize {
    let mut size = 0usize;
    for types::Field { ty, .. } in ty.fields() {
        size = align_to(size, align_of(&ty)).saturating_add(size_of(&ty));
    }
    align_to(size, align_of_record(ty))
}

#[instrument(level = "trace")]
pub fn size_of_tuple(ty: &types::Tuple) -> usize {
    let mut size = 0usize;
    for ty in ty.types() {
        size = align_to(size, align_of(&ty)).saturating_add(size_of(&ty));
    }
    align_to(size, align_of_tuple(ty))
}

#[instrument(level = "trace")]
pub fn size_of_variant(ty: &types::Variant) -> usize {
    let cases = ty.cases();
    let size: usize = match cases.len() {
        ..=0x0000_00ff => 1,
        0x0000_0100..=0x0000_ffff => 2,
        0x0001_0000.. => 4,
    };
    let size = align_to(size, max_case_alignment(ty.cases()));
    let size = size.saturating_add(
        cases
            .map(|types::Case { ty, .. }| ty.as_ref().map(size_of).unwrap_or_default())
            .max()
            .unwrap_or_default(),
    );
    align_to(size, align_of_variant(ty))
}

#[instrument(level = "trace")]
pub fn size_of_option(ty: &types::OptionType) -> usize {
    let size = size_of(&ty.ty()).saturating_add(1);
    align_to(size, align_of_option(ty))
}

#[instrument(level = "trace")]
pub fn size_of_result(ty: &types::ResultType) -> usize {
    let ok = ty.ok().as_ref().map(size_of).unwrap_or_default();
    let err = ty.err().as_ref().map(size_of).unwrap_or_default();
    let size = ok.max(err).saturating_add(1);
    align_to(size, align_of_result(ty))
}

#[instrument(level = "trace")]
pub fn size_of(ty: &Type) -> usize {
    match ty {
        Type::Bool | Type::S8 | Type::U8 => 1,
        Type::S16 | Type::U16 => 2,
        Type::S32 | Type::U32 | Type::Float32 | Type::Char | Type::Own(_) | Type::Borrow(_) => 4,
        Type::S64 | Type::U64 | Type::Float64 => 8,
        Type::String | Type::List(_) => mem::size_of::<(*const (), usize)>(),
        Type::Record(ty) => size_of_record(ty),
        Type::Tuple(ty) => size_of_tuple(ty),
        Type::Variant(ty) => size_of_variant(ty),
        Type::Enum(ty) => match ty.names().len() {
            ..=0x0000_00ff => 1,
            0x0000_0100..=0x0000_ffff => 2,
            0x0001_0000.. => 4,
        },
        Type::Option(ty) => size_of_option(ty),
        Type::Result(ty) => size_of_result(ty),
        Type::Flags(ty) => match ty.names().len() {
            ..=8 => 1,
            9..=16 => 2,
            _ => 4,
        },
    }
}

#[instrument(level = "trace")]
pub fn args_of_variant(ty: &types::Variant) -> usize {
    ty.cases()
        .map(|ty| ty.ty.map(|ty| args_of(&ty)).unwrap_or_default())
        .max()
        .unwrap_or_default()
        .saturating_add(1)
}

#[instrument(level = "trace")]
pub fn args_of_result(ty: &types::ResultType) -> usize {
    let ok = ty.ok().as_ref().map(args_of).unwrap_or_default();
    let err = ty.err().as_ref().map(args_of).unwrap_or_default();
    ok.max(err).saturating_add(1)
}

#[instrument(level = "trace")]
pub fn args_of(ty: &Type) -> usize {
    match ty {
        Type::Bool
        | Type::S8
        | Type::U8
        | Type::S16
        | Type::U16
        | Type::S32
        | Type::U32
        | Type::Float32
        | Type::Char
        | Type::Own(_)
        | Type::Borrow(_)
        | Type::S64
        | Type::U64
        | Type::Float64
        | Type::Enum(_)
        | Type::Flags(_) => 1,
        Type::String | Type::List(_) => 2,
        Type::Record(ty) => ty.fields().map(|ty| args_of(&ty.ty)).sum(),
        Type::Tuple(ty) => ty.types().map(|ty| args_of(&ty)).sum(),
        Type::Variant(ty) => args_of_variant(ty),
        Type::Option(ty) => args_of(&ty.ty()).saturating_add(1),
        Type::Result(ty) => args_of_result(ty),
    }
}

fn find_variant_discriminant<'a, T>(
    iter: impl IntoIterator<Item = T>,
    cases: impl IntoIterator<Item = types::Case<'a>>,
    disc: &str,
) -> anyhow::Result<(T, Option<Type>)> {
    zip(iter, cases)
        .find_map(|(i, types::Case { name, ty })| (name == disc).then_some((i, ty)))
        .context("unknown variant discriminant")
}
