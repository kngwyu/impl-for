use impl_for::impl_for;

trait Trait {
    fn length(&self) -> usize;
}

#[impl_for(N = 0..32)]
#[impl_for(Ty = [u32, u64])]
impl Trait for [Ty; N] {
    fn length(&self) -> usize {
        N
    }
}

#[impl_for(N = [32, 64, 128])]
impl Trait for [u32; N] {
    fn length(&self) -> usize {
        N
    }
}

#[test]
fn test() {
    assert_eq!([1u32, 2, 3, 4, 5].length(), 5);
    assert_eq!([1u64, 2, 3, 4, 5].length(), 5);
}
