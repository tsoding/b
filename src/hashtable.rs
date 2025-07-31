use crate::crust::libc;
use core::hash::{BuildHasher, Hash, Hasher};
use core::{cmp, mem, ptr};

#[derive(Clone, Copy)]
pub struct HashTable<K, V, S = DefaultHasher> {
    pub entries: *mut Entry<K, V>,
    pub capacity: usize,
    pub count: usize,
    pub occupied: *mut u8,
    pub hasher_builder: S,
}

#[derive(Clone, Copy)]
pub struct Entry<K, V> {
    pub key: K,
    pub value: V,
}

#[derive(Clone, Copy)]
pub enum HtEntry<K, V> {
    Occupied(*mut Entry<K, V>),
    Vacant(*mut Entry<K, V>),
}

impl<K, V, S, H> HashTable<K, V, S>
where
    K: Clone + Copy + Hash + Eq,
    V: Clone + Copy,
    S: BuildHasher<Hasher = H>,
    H: Hasher,
{
    // Must be power of 2 and greater than or equal to 8
    pub const MIN_CAPACITY: usize = 32;

    /// Returns previous value stored by this `key` or `None`
    pub unsafe fn insert(ht: *mut Self, key: K, value: V) -> Option<V> {
        match Self::find(ht, key) {
            HtEntry::Occupied(entry) => Some(mem::replace(&mut (*entry).value, value)),
            HtEntry::Vacant(entry) => {
                Self::insert_new_key(ht, entry, key, value);
                None
            }
        }
    }

    pub unsafe fn get(ht: *const Self, key: K) -> Option<*const V> {
        match Self::find(ht, key) {
            HtEntry::Occupied(entry) => Some(&(*entry).value),
            HtEntry::Vacant(_) => None,
        }
    }

    pub unsafe fn get_mut(ht: *mut Self, key: K) -> Option<*mut V> {
        match Self::find(ht, key) {
            HtEntry::Occupied(entry) => Some(&mut (*entry).value),
            HtEntry::Vacant(_) => None,
        }
    }

    pub unsafe fn clear(ht: *mut Self) {
        (*ht).count = 0;
        ptr::write_bytes((*ht).occupied, 0, (*ht).capacity >> 3);
    }

    pub unsafe fn find(ht: *const Self, key: K) -> HtEntry<K, V> {
        if (*ht).capacity == 0 {
            return HtEntry::Vacant(ptr::null_mut());
        }

        let hash = Self::hash_key(ht, key);
        let mut index = Self::index_from_hash(hash, (*ht).capacity);

        let mut step = 1;
        loop {
            let entry = (*ht).entries.add(index);
            if Self::is_occupied(ht, index) {
                if (*entry).key == key {
                    return HtEntry::Occupied(entry);
                }
            } else {
                return HtEntry::Vacant(entry);
            }

            index = (index + step) & ((*ht).capacity - 1);
            step += 1;
        }
    }

    pub unsafe fn insert_new_key(ht: *mut Self, entry: *mut Entry<K, V>, key: K, value: V) {
        if entry.is_null() {
            Self::realloc_rehash(ht);

            // Executes only when capacity was 0
            let hash = Self::hash_key(ht, key);
            let index = Self::index_from_hash(hash, (*ht).capacity);
            Self::fill_entry(ht, (*ht).entries.add(index), index, key, value);
        } else {
            let index = entry.offset_from((*ht).entries);
            debug_assert!(index >= 0);
            Self::fill_entry(ht, entry, index as usize, key, value);

            // When load factor > 0.75
            if (3 * (*ht).capacity) / 4 < (*ht).count {
                Self::realloc_rehash(ht);
            }
        }
    }

    pub unsafe fn realloc_rehash(ht: *mut Self) {
        let old_entries = (*ht).entries;
        let old_occupied = (*ht).occupied;
        let old_capacity = (*ht).capacity;

        (*ht).capacity = cmp::max(old_capacity << 1, Self::MIN_CAPACITY);
        debug_assert!((*ht).capacity.is_power_of_two());
        
        // We need new allocations here, to properly copy entries
        (*ht).entries = libc::realloc_items(ptr::null_mut(), (*ht).capacity);
        (*ht).occupied = libc::realloc_items(ptr::null_mut(), (*ht).capacity >> 3);
        debug_assert!(!(*ht).entries.is_null());
        debug_assert!(!(*ht).occupied.is_null());

        // Fill occupied with zeros
        ptr::write_bytes((*ht).occupied, 0, (*ht).capacity >> 3);

        // Rehash all occupoed entries
        let buckets_count = old_capacity >> 3;
        for i in 0..buckets_count {
            let bucket = *old_occupied.add(i);
            for j in 0..8 {
                if (bucket >> j) & 1 == 1 {
                    let index = (i << 3) + (7 - j);
                    let entry = *old_entries.add(index);
                    assert!(Self::insert(ht, entry.key, entry.value).is_none()); 
                }
            }
        }

        // free old allocations
        libc::free(old_entries);
        libc::free(old_occupied);
    }

    pub unsafe fn fill_entry(ht: *mut Self, entry: *mut Entry<K, V>, index: usize, key: K, value: V) {
        *entry = Entry { key, value };
        Self::occupy_index(ht, index);
        (*ht).count += 1;
    }

    pub unsafe fn occupy_index(ht: *mut Self, index: usize) {
        let bucket = (*ht).occupied.add(index >> 3);
        let sub_index = 7 - (index & 7);
        *bucket |= 1 << sub_index;
    }

    pub unsafe fn is_occupied(ht: *const Self, index: usize) -> bool {
        let bucket = *(*ht).occupied.add(index >> 3);
        let sub_index = 7 - (index & 7);
        (bucket >> sub_index) & 1 == 1
    }

    pub unsafe fn index_from_hash(hash: u64, capacity: usize) -> usize {
        (hash & (capacity as u64 - 1)) as usize
    }

    pub unsafe fn hash_key(ht: *const Self, key: K) -> u64 {
        let mut hasher = (*ht).hasher_builder.build_hasher();
        key.hash(&mut hasher);
        hasher.finish()
    }
}

#[derive(Clone, Copy)]
pub struct DefaultHasher;

impl BuildHasher for DefaultHasher {
    type Hasher = Fnv1aHasher;

    fn build_hasher(&self) -> Self::Hasher {
        Fnv1aHasher {
            hash: Fnv1aHasher::OFFSET,
        }
    }
}

#[derive(Clone, Copy)]
pub struct Fnv1aHasher {
    pub hash: u64,
}

impl Fnv1aHasher {
    const OFFSET: u64 = 14695981039346656037;
    const PRIME: u64 = 1099511628211;
}

impl Hasher for Fnv1aHasher {
    fn finish(&self) -> u64 {
        self.hash
    }

    fn write(&mut self, bytes: &[u8]) {
        for byte in bytes {
            self.hash ^= *byte as u64;
            self.hash = self.hash.wrapping_mul(Self::PRIME);
        }
    }

    fn write_u8(&mut self, i: u8) {
        self.hash ^= i as u64;
        self.hash = self.hash.wrapping_mul(Self::PRIME);
    }

    fn write_u16(&mut self, i: u16) {
        self.hash ^= i as u64;
        self.hash = self.hash.wrapping_mul(Self::PRIME);
    }

    fn write_u32(&mut self, i: u32) {
        self.hash ^= i as u64;
        self.hash = self.hash.wrapping_mul(Self::PRIME);
    }

    fn write_u64(&mut self, i: u64) {
        self.hash ^= i;
        self.hash = self.hash.wrapping_mul(Self::PRIME);
    }

    fn write_usize(&mut self, i: usize) {
        self.hash ^= i as u64;
        self.hash = self.hash.wrapping_mul(Self::PRIME);
    }
}
