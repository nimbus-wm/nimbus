use core_graphics_types::geometry as cg;
use icrate::Foundation as ic;

pub(crate) trait ToICrate<T> {
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

pub(crate) trait ToCGType<T> {
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
