use super::*;

pub struct A {
    x: u64,
}

impl Object for A {}

pub struct B<'o, 'm: 'o> {
    a: ObjectRef<'o, 'm, A>,
}

impl<'o, 'm: 'o> Object for B<'o, 'm> {}

fn allocating_a<'o, 'm: 'o>(mm: &'m mut MemoryManager<'o>) {
    let a_ref = mm.allocate(Box::new(A { x: 123 }));
    assert_eq!(123, a_ref.pin().as_ref().x);
}

fn allocating_b<'o, 'm: 'o>(mm: &'m mut MemoryManager<'o>) {
    let a_ref = mm.allocate(Box::new(A { x: 123 }));
    let b_ref = mm.allocate(Box::new(B{a: a_ref}));
    assert_eq!(123, b_ref.pin().as_ref().a.pin().as_ref().x);
}

#[test]
fn test_allocating_a() {
    let mut mm = MemoryManager::new();
    allocating_a(&mut mm)
}

#[test]
fn test_allocating_b() {
    let mut mm = MemoryManager::new();
    allocating_b(&mut mm)
}
