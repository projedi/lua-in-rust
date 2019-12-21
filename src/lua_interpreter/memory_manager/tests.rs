use super::*;

pub struct A {
    x: u64,
}

impl Object for A {}

pub struct B<'g> {
    a: ObjectRef<'g, A>,
}

impl<'g> Object for B<'g> {}

#[test]
fn test_allocating() {
    let mut mm = MemoryManager::new();
    let a_ref = mm.allocate(Box::new(A{x: 123}));
    let b_ref = mm.allocate(Box::new(B{a: a_ref}));
    assert_eq!(123, b_ref.pin().as_ref().a.pin().as_ref().x);
}