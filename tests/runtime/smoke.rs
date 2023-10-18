use anyhow::Result;
use crate::TestConfigurer;
use crate::Wasi;
use wasmtime::component::{Component, Linker};
use wasmtime::Store;

wasmtime::component::bindgen!({
    path: "tests/runtime/smoke",
});

#[derive(Default)]
pub struct MyImports {
    hit: bool,
}


impl test::smoke::imports::Host for MyImports {
    fn thunk(&mut self) -> Result<()> {
        self.hit = true;
        println!("in the host");
        Ok(())
    }
}

struct SmokeConfigurer {}

impl TestConfigurer<MyImports, Smoke> for SmokeConfigurer {
    fn instantiate(
        &self,
        store: &mut Store<Wasi<MyImports>>,
        component: &Component,
        linker: &Linker<Wasi<MyImports>>,
    ) -> Result<(Smoke, wasmtime::component::Instance)> {
        Smoke::instantiate(store, component, linker)
    }

    fn test(&self, exports: Smoke, store: &mut Store<Wasi<MyImports>>) -> Result<()> {
        run_test(exports, store)
    }
}

#[test]
fn run() -> Result<()> {
    let configurer = SmokeConfigurer {};

    crate::run_test(
        "smoke",
        |linker| Smoke::add_to_linker(linker, |x| &mut x.0),
        configurer,
    )
}

fn run_test(exports: Smoke, store: &mut Store<crate::Wasi<MyImports>>) -> Result<()> {
    exports.call_thunk(&mut *store)?;

    assert!(store.data().0.hit);

    Ok(())
}
