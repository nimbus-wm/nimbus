use core_graphics_types::geometry as cg;
use icrate::Foundation as ic;
use serde::{Deserialize, Deserializer, Serialize};
use serde_with::{DeserializeAs, SerializeAs};

/*
#[derive(Serialize, Deserialize)]
#[serde(transparent)]
#[repr(transparent)]
pub struct CGRect(#[serde(with = "CGRectDef")] ic::CGRect);

impl Deref for CGRect {
    type Target = ic::CGRect;
    fn deref(&self) -> &Self::Target {
        // SAFETY: repr(transparent)
        unsafe { std::mem::transmute(self) }
    }
}

impl DerefMut for CGRect {
    fn deref_mut(&mut self) -> &mut Self::Target {
        // SAFETY: repr(transparent)
        unsafe { std::mem::transmute(self) }
    }
}

#[derive(Serialize, Deserialize)]
#[serde(remote = "ic::CGPoint")]
pub struct CGPointDef {
    pub x: f64,
    pub y: f64,
}

#[derive(Serialize, Deserialize)]
#[serde(remote = "ic::CGSize")]
pub struct CGSizeDef {
    pub width: f64,
    pub height: f64,
}
*/

pub trait ToICrate<T> {
    fn to_icrate(&self) -> T;
}

impl ToICrate<ic::CGPoint> for cg::CGPoint {
    fn to_icrate(&self) -> ic::CGPoint {
        ic::CGPoint { x: self.x, y: self.y }
    }
}

impl ToICrate<ic::CGSize> for cg::CGSize {
    fn to_icrate(&self) -> ic::CGSize {
        ic::CGSize {
            width: self.width,
            height: self.height,
        }
    }
}

impl ToICrate<ic::CGRect> for cg::CGRect {
    fn to_icrate(&self) -> ic::CGRect {
        ic::CGRect {
            origin: self.origin.to_icrate(),
            size: self.size.to_icrate(),
        }
    }
}

pub trait ToCGType<T> {
    fn to_cgtype(&self) -> T;
}

impl ToCGType<cg::CGPoint> for ic::CGPoint {
    fn to_cgtype(&self) -> cg::CGPoint {
        cg::CGPoint { x: self.x, y: self.y }
    }
}

impl ToCGType<cg::CGSize> for ic::CGSize {
    fn to_cgtype(&self) -> cg::CGSize {
        cg::CGSize {
            width: self.width,
            height: self.height,
        }
    }
}

impl ToCGType<cg::CGRect> for ic::CGRect {
    fn to_cgtype(&self) -> cg::CGRect {
        cg::CGRect {
            origin: self.origin.to_cgtype(),
            size: self.size.to_cgtype(),
        }
    }
}

pub trait Round {
    fn round(&self) -> Self;
}

impl Round for ic::CGRect {
    fn round(&self) -> Self {
        // Round each corner to pixel boundaries, then use that to calculate the size.
        let min_rounded = self.min().round();
        let max_rounded = self.max().round();
        ic::CGRect {
            origin: min_rounded,
            size: ic::CGSize {
                width: max_rounded.x - min_rounded.x,
                height: max_rounded.y - min_rounded.y,
            },
        }
    }
}

impl Round for ic::CGPoint {
    fn round(&self) -> Self {
        ic::CGPoint {
            x: self.x.round(),
            y: self.y.round(),
        }
    }
}

impl Round for ic::CGSize {
    fn round(&self) -> Self {
        ic::CGSize {
            width: self.width.round(),
            height: self.height.round(),
        }
    }
}

pub trait IsWithin {
    fn is_within(&self, how_much: f64, other: Self) -> bool;
}

impl IsWithin for ic::CGRect {
    fn is_within(&self, how_much: f64, other: Self) -> bool {
        self.origin.is_within(how_much, other.origin) && self.size.is_within(how_much, other.size)
    }
}

impl IsWithin for ic::CGPoint {
    fn is_within(&self, how_much: f64, other: Self) -> bool {
        self.x.is_within(how_much, other.x) && self.y.is_within(how_much, other.y)
    }
}

impl IsWithin for ic::CGSize {
    fn is_within(&self, how_much: f64, other: Self) -> bool {
        self.width.is_within(how_much, other.width) && self.height.is_within(how_much, other.height)
    }
}

impl IsWithin for f64 {
    fn is_within(&self, how_much: f64, other: Self) -> bool {
        (self - other).abs() < how_much
    }
}

pub trait SameAs: IsWithin + Sized {
    fn same_as(&self, other: Self) -> bool {
        self.is_within(0.1, other)
    }
}

impl SameAs for ic::CGRect {}
impl SameAs for ic::CGPoint {}
impl SameAs for ic::CGSize {}

#[derive(Serialize, Deserialize)]
#[serde(remote = "ic::CGRect")]
pub struct CGRectDef {
    #[serde(with = "CGPointDef")]
    pub origin: ic::CGPoint,
    #[serde(with = "CGSizeDef")]
    pub size: ic::CGSize,
}

#[derive(Serialize, Deserialize)]
#[serde(remote = "ic::CGPoint")]
pub struct CGPointDef {
    pub x: f64,
    pub y: f64,
}

#[derive(Serialize, Deserialize)]
#[serde(remote = "ic::CGSize")]
pub struct CGSizeDef {
    pub width: f64,
    pub height: f64,
}

impl SerializeAs<ic::CGRect> for CGRectDef {
    fn serialize_as<S>(value: &ic::CGRect, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        CGRectDef::serialize(value, serializer)
    }
}

impl<'de> DeserializeAs<'de, ic::CGRect> for CGRectDef {
    fn deserialize_as<D>(deserializer: D) -> Result<ic::CGRect, D::Error>
    where
        D: Deserializer<'de>,
    {
        CGRectDef::deserialize(deserializer)
    }
}
