use anyhow::Result;
use crate::TestConfigurer;
use crate::Wasi;
use wasmtime::component::{Component, Linker};
use wasmtime::Store;

wasmtime::component::bindgen!({
    path: "tests/runtime/strings",
});

#[derive(Default)]
pub struct MyImports;

impl test::strings::imports::Host for MyImports {
    fn take_basic(&mut self, s: String) -> Result<()> {
        assert_eq!(s, "latin utf16");
        Ok(())
    }

    fn return_unicode(&mut self) -> Result<String> {
        Ok("🚀🚀🚀 𠈄𓀀".to_string())
    }
}

struct StringsConfigurer {}

impl TestConfigurer<MyImports, Strings> for StringsConfigurer {
    fn instantiate(
        &self,
        store: &mut Store<Wasi<MyImports>>,
        component: &Component,
        linker: &Linker<Wasi<MyImports>>,
    ) -> Result<(Strings, wasmtime::component::Instance)> {
        Strings::instantiate(store, component, linker)
    }

    fn test(&self, exports: Strings, store: &mut Store<Wasi<MyImports>>) -> Result<()> {
        run_test(exports, store)
    }
}

#[test]
fn run() -> Result<()> {
    let configurer = StringsConfigurer {};

    crate::run_test(
        "strings",
        |linker| Strings::add_to_linker(linker, |x| &mut x.0),
        configurer
    )
}

fn run_test(exports: Strings, store: &mut Store<crate::Wasi<MyImports>>) -> Result<()> {
    exports.call_test_imports(&mut *store)?;
    assert_eq!(exports.call_return_empty(&mut *store)?, "");
    assert_eq!(exports.call_roundtrip(&mut *store, "str")?, "str");
    assert_eq!(
        exports.call_roundtrip(&mut *store, "🚀🚀🚀 𠈄𓀀")?,
        "🚀🚀🚀 𠈄𓀀"
    );
    Ok(())
}
