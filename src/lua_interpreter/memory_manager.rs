#[derive(Clone, Copy)]
struct ObjectId(usize);

pub trait Object {}

pub struct ObjectRef<'o, 'm: 'o, T: Object + 'o> {
    id: ObjectId,
    manager: &'m MemoryManager<'o>,
    marker: std::marker::PhantomData<T>,
}

// Non-copyable for a reason. Forbids moving of the object during collection.
pub struct ObjectPin<'o, 'm: 'o, T: Object + 'o> {
    object_ref: ObjectRef<'o, 'm, T>,
}

// TODO: Make it a trait with different implementations.
pub struct MemoryManager<'o> {
    objects: Vec<Box<dyn Object + 'o>>,
}

impl<'o> MemoryManager<'o> {
    pub fn new() -> MemoryManager<'o> {
        MemoryManager{
            objects: vec![],
        }
    }

    pub fn allocate<'m: 'o, T: Object + 'o>(&'m mut self, obj: Box<T>) -> ObjectRef<'m, 'o, T> {
        let id = self.objects.len();
        self.objects.push(obj);
        ObjectRef{
            id: ObjectId(id),
            manager: self,
            marker: std::marker::PhantomData,
        }
    }
}

impl<'o, 'm: 'o, T: Object + 'o> ObjectRef<'o, 'm, T> {
    pub fn pin(&self) -> ObjectPin<'o, 'm, T> {
        ObjectPin{
            object_ref: self.clone()
        }
    }
}

impl<'o, 'm: 'o, T: Object + 'o> Clone for ObjectRef<'o, 'm, T> {
    fn clone(&self) -> Self {
        ObjectRef{
            id: self.id.clone(),
            manager: self.manager,
            marker: std::marker::PhantomData,
        }
    }
}

impl<'o, 'm: 'o, T: Object + 'o> Copy for ObjectRef<'o, 'm, T> {}

impl<'o, 'm: 'o, T: Object + 'o> ObjectPin<'o, 'm, T> {
    pub fn as_ref<'a>(&'a self) -> &'a T where 'o: 'a {
        let obj = self.object_ref.manager.objects[self.object_ref.id.0].as_ref();
        let obj_ptr = obj as *const dyn Object;
        unsafe { &*(obj_ptr as *const T) }
    }
}

#[cfg(test)]
mod tests;