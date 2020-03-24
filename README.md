# impl-for

An experimental crate that provides a macro for iterative declaration.

## Example

```rust
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
```

# License

This project itself is licensed under either of

 * Apache License, Version 2.0, ([LICENSE-APACHE](LICENSE-APACHE) or http://www.apache.org/licenses/LICENSE-2.0)
 * MIT license ([LICENSE-MIT](LICENSE-MIT) or http://opensource.org/licenses/MIT)

at your option.
