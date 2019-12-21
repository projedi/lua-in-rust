#[derive(Clone, Copy)]
struct ObjectId(usize);

pub trait Object {}

pub struct ObjectRef<'g, T: Object + 'g> {
    id: ObjectId,
    manager: &'g MemoryManager<'g>,
    marker: std::marker::PhantomData<T>,
}

// Non-copyable for a reason. Forbids moving of the object during collection.
pub struct ObjectPin<'g, T: Object + 'g> {
    object_ref: ObjectRef<'g, T>,
}

// TODO: Make it a trait with different implementations.
pub struct MemoryManager<'g> {
    objects: Vec<Box<dyn Object + 'g>>,
}

impl<'g> MemoryManager<'g> {
    pub fn new() -> MemoryManager<'g> {
        MemoryManager{
            objects: vec![],
        }
    }

    pub fn allocate<T: Object + 'g>(&'g mut self, obj: Box<T>) -> ObjectRef<'g, T> {
        let id = self.objects.len();
        self.objects.push(obj);
        ObjectRef{
            id: ObjectId(id),
            manager: self,
            marker: std::marker::PhantomData,
        }
    }
}

impl<'g, T: Object + 'g> ObjectRef<'g, T> {
    pub fn pin(&self) -> ObjectPin<'g, T> {
        ObjectPin{
            object_ref: self.clone()
        }
    }
}

impl<'g, T: Object + 'g> Clone for ObjectRef<'g, T> {
    fn clone(&self) -> Self {
        ObjectRef{
            id: self.id.clone(),
            manager: self.manager,
            marker: std::marker::PhantomData,
        }
    }
}

impl<'g, T: Object + 'g> Copy for ObjectRef<'g, T> {}

impl<'g, T: Object + 'g> ObjectPin<'g, T> {
    pub fn as_ref<'a>(&'a self) -> &'a T where 'g: 'a {
        let obj = self.object_ref.manager.objects[self.object_ref.id.0].as_ref();
        let obj_ptr = obj as *const dyn Object;
        unsafe { &*(obj_ptr as *const T) }
    }
}

#[cfg(test)]
mod tests;