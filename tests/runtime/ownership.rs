use anyhow::Result;
use crate::TestConfigurer;
use crate::Wasi;
use wasmtime::component::{Component, Linker};
use wasmtime::Store;

wasmtime::component::bindgen!({
    path: "tests/runtime/ownership",
});

#[derive(Default)]
pub struct MyImports {
    called_foo: bool,
    called_bar: bool,
    called_baz: bool,
}

impl lists::Host for MyImports {
    fn foo(&mut self, list: Vec<Vec<String>>) -> Result<Vec<Vec<String>>> {
        self.called_foo = true;
        Ok(list)
    }
}

impl thing_in::Host for MyImports {
    fn bar(&mut self, _value: thing_in::Thing) -> Result<()> {
        self.called_bar = true;
        Ok(())
    }
}

impl thing_in_and_out::Host for MyImports {
    fn baz(&mut self, value: thing_in_and_out::Thing) -> Result<thing_in_and_out::Thing> {
        self.called_baz = true;
        Ok(value)
    }
}

struct OwnershipConfigurer {}

impl TestConfigurer<MyImports, Ownership> for OwnershipConfigurer {
    fn instantiate(
        &self,
        store: &mut Store<Wasi<MyImports>>,
        component: &Component,
        linker: &Linker<Wasi<MyImports>>,
    ) -> Result<(Ownership, wasmtime::component::Instance)> {
        Ownership::instantiate(store, component, linker)
    }

    fn test(&self, exports: Ownership, store: &mut Store<Wasi<MyImports>>) -> Result<()> {
        run_test(exports, store)
    }
}

#[test]
fn run() -> Result<()> {
    let configurer = OwnershipConfigurer {};

    for name in ["owning", "borrowing", "borrowing-duplicate-if-necessary"] {
        crate::run_test_from_dir(
            "ownership",
            name,
            |linker| Ownership::add_to_linker(linker, |x| &mut x.0),
            &configurer,
        )?;
    }

    Ok(())
}

fn run_test(exports: Ownership, store: &mut Store<crate::Wasi<MyImports>>) -> Result<()> {
    exports.call_foo(&mut *store)?;

    assert!(store.data().0.called_foo);
    assert!(store.data().0.called_bar);
    assert!(store.data().0.called_baz);

    Ok(())
}
