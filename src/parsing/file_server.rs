use std::borrow::Cow;
use std::collections::HashMap;
use std::io;
use std::path::{Path, PathBuf};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct FileHandle(u64);
impl FileHandle {
    pub const NULL: Self = Self(0);

    #[inline]
    pub const fn is_null(self) -> bool {
        self.0 == 0
    }
}

trait FileSource {
    fn full_name(&self) -> Cow<str>;
    fn read_text(&self) -> io::Result<Box<[char]>>;
}

struct RealFile {
    path: PathBuf,
}
impl RealFile {
    pub fn create<P: AsRef<Path>>(path: P) -> io::Result<Self> {
        let path = Path::canonicalize(path.as_ref())?;

        Ok(Self { path })
    }
}
impl FileSource for RealFile {
    fn full_name(&self) -> Cow<str> {
        self.path.to_string_lossy()
    }

    fn read_text(&self) -> io::Result<Box<[char]>> {
        let s = std::fs::read_to_string(&self.path)?;
        let text: Box<[char]> = s.chars().collect();
        Ok(text)
    }
}

struct DummyFile {
    name: String,
    contents: String,
}
impl DummyFile {
    pub fn new<S1, S2>(name: S1, contents: S2) -> Self
    where
        S1: ToString,
        S2: ToString,
    {
        Self {
            name: name.to_string(),
            contents: contents.to_string(),
        }
    }
}
impl FileSource for DummyFile {
    fn full_name(&self) -> Cow<str> {
        Cow::Borrowed(&self.name)
    }

    fn read_text(&self) -> io::Result<Box<[char]>> {
        let text: Box<[char]> = self.contents.chars().collect();
        Ok(text)
    }
}

pub struct FileServer {
    next_handle: FileHandle,
    files: HashMap<FileHandle, Box<dyn FileSource>>,
    cache: HashMap<FileHandle, Box<[char]>>,
}
#[allow(dead_code)]
impl FileServer {
    pub fn new() -> Self {
        Self {
            next_handle: FileHandle(1), // 0 is reserved for null handle
            files: HashMap::new(),
            cache: HashMap::new(),
        }
    }

    pub fn add_file<P: AsRef<Path>>(&mut self, path: P) -> io::Result<FileHandle> {
        let handle = self.next_handle;
        let file = Box::new(RealFile::create(path)?);
        let prev = self.files.insert(handle, file);
        assert!(prev.is_none());
        self.next_handle.0 += 1;
        Ok(handle)
    }

    pub fn add_dummy_file<S1, S2>(&mut self, name: S1, contents: S2) -> FileHandle
    where
        S1: ToString,
        S2: ToString,
    {
        let handle = self.next_handle;
        let file = Box::new(DummyFile::new(name, contents));
        let prev = self.files.insert(handle, file);
        assert!(prev.is_none());
        self.next_handle.0 += 1;
        handle
    }

    pub fn files(&self) -> Vec<FileHandle> {
        self.files.keys().map(|h| *h).collect()
    }

    pub fn get_file_name(&self, handle: FileHandle) -> Cow<str> {
        if handle.is_null() {
            Cow::Borrowed("<None>")
        } else {
            let file = self.files.get(&handle).expect("Invalid file handle");
            file.full_name()
        }
    }

    pub fn get_text<'a>(&'a mut self, handle: FileHandle) -> io::Result<&'a [char]> {
        const EMPTY: &[char] = &[];

        if handle.is_null() {
            Ok(EMPTY)
        } else if self.cache.contains_key(&handle) {
            Ok(&self.cache[&handle])
        } else {
            let file = self.files.get(&handle).expect("Invalid file handle");
            let text = file.read_text()?;
            let prev = self.cache.insert(handle, text);
            assert!(prev.is_none());
            Ok(&self.cache[&handle])
        }
    }
}
