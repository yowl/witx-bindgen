import { data_view, UTF8_DECODER, utf8_encode, UTF8_ENCODED_LEN, Slab } from './intrinsics.js';
export const Lang = Object.freeze({
  0: "Js",
  "Js": 0,
  1: "Rust",
  "Rust": 1,
  2: "Wasmtime",
  "Wasmtime": 2,
  3: "WasmtimePy",
  "WasmtimePy": 3,
  4: "C",
  "C": 4,
  5: "Markdown",
  "Markdown": 5,
  6: "Spidermonkey",
  "Spidermonkey": 6,
});
export class Demo {
  constructor() {
    this._resource0_slab = new Slab();
  }
  addToImports(imports) {
    if (!("canonical_abi" in imports)) imports["canonical_abi"] = {};
    
    imports.canonical_abi['resource_drop_config'] = i => {
      this._resource0_slab.remove(i).drop();
    };
    imports.canonical_abi['resource_clone_config'] = i => {
      const obj = this._resource0_slab.get(i);
      return this._resource0_slab.insert(obj.clone())
    };
    imports.canonical_abi['resource_get_config'] = i => {
      return this._resource0_slab.get(i)._wasm_val;
    };
    imports.canonical_abi['resource_new_config'] = i => {
      const registry = this._registry0;
      return this._resource0_slab.insert(new Config(i, this));
    };
  }
  
  async instantiate(module, imports) {
    imports = imports || {};
    this.addToImports(imports);
    
    if (module instanceof WebAssembly.Instance) {
      this.instance = module;
    } else if (module instanceof WebAssembly.Module) {
      this.instance = await WebAssembly.instantiate(module, imports);
    } else if (module instanceof ArrayBuffer || module instanceof Uint8Array) {
      const { instance } = await WebAssembly.instantiate(module, imports);
      this.instance = instance;
    } else {
      const { instance } = await WebAssembly.instantiateStreaming(module, imports);
      this.instance = instance;
    }
    this._exports = this.instance.exports;
    this._registry0 = new FinalizationRegistry(this._exports['canonical_abi_drop_config']);
  }
}

export class Config {
  constructor(wasm_val, obj) {
    this._wasm_val = wasm_val;
    this._obj = obj;
    this._refcnt = 1;
    obj._registry0.register(this, wasm_val, this);
  }
  
  clone() {
    this._refcnt += 1;
    return this;
  }
  
  drop() {
    this._refcnt -= 1;
    if (this._refcnt !== 0)
    return;
    this._obj._registry0.unregister(this);
    const dtor = this._obj._exports['canonical_abi_drop_config'];
    const wasm_val = this._wasm_val;
    delete this._obj;
    delete this._refcnt;
    delete this._wasm_val;
    dtor(wasm_val);
  }
  static new(demo) {
    const ret = demo._exports['config::new']();
    return demo._resource0_slab.remove(ret);
  }
  render(arg1, arg2, arg3) {
    const memory = this._obj._exports.memory;
    const realloc = this._obj._exports["canonical_abi_realloc"];
    const free = this._obj._exports["canonical_abi_free"];
    const obj0 = this;
    const variant1 = arg1;
    if (!(variant1 in Lang))
    throw new RangeError("invalid variant specified for Lang");
    const ptr2 = utf8_encode(arg2, realloc, memory);
    const len2 = UTF8_ENCODED_LEN;
    const variant3 = arg3;
    let variant3_0;
    switch (variant3) {
      case false: {
        variant3_0 = 0;
        break;
      }
      case true: {
        variant3_0 = 1;
        break;
      }
      default:
      throw new RangeError("invalid variant specified for bool");
    }
    const ret = this._obj._exports['config::render'](this._obj._resource0_slab.insert(obj0.clone()), Number.isInteger(variant1) ? variant1 : Lang[variant1], ptr2, len2, variant3_0);
    let variant8;
    switch (data_view(memory).getInt32(ret + 0, true)) {
      case 0: {
        const len6 = data_view(memory).getInt32(ret + 16, true);
        const base6 = data_view(memory).getInt32(ret + 8, true);
        const result6 = [];
        for (let i = 0; i < len6; i++) {
          const base = base6 + i * 16;
          const ptr4 = data_view(memory).getInt32(base + 0, true);
          const len4 = data_view(memory).getInt32(base + 4, true);
          const list4 = UTF8_DECODER.decode(new Uint8Array(memory.buffer, ptr4, len4));
          free(ptr4, len4, 1);
          const ptr5 = data_view(memory).getInt32(base + 8, true);
          const len5 = data_view(memory).getInt32(base + 12, true);
          const list5 = UTF8_DECODER.decode(new Uint8Array(memory.buffer, ptr5, len5));
          free(ptr5, len5, 1);
          result6.push([list4, list5]);
        }
        free(base6, len6 * 16, 4);
        variant8 = {
          tag: "ok",
          val: result6,
        };
        break;
      }
      case 1: {
        const ptr7 = data_view(memory).getInt32(ret + 8, true);
        const len7 = data_view(memory).getInt32(ret + 16, true);
        const list7 = UTF8_DECODER.decode(new Uint8Array(memory.buffer, ptr7, len7));
        free(ptr7, len7, 1);
        variant8 = {
          tag: "err",
          val: list7,
        };
        break;
      }
      default:
      throw new RangeError("invalid variant discriminant for expected");
    }
    return variant8;
  }
  setRustUnchecked(arg1) {
    const obj0 = this;
    const variant1 = arg1;
    let variant1_0;
    switch (variant1) {
      case false: {
        variant1_0 = 0;
        break;
      }
      case true: {
        variant1_0 = 1;
        break;
      }
      default:
      throw new RangeError("invalid variant specified for bool");
    }
    this._obj._exports['config::set_rust_unchecked'](this._obj._resource0_slab.insert(obj0.clone()), variant1_0);
  }
  setWasmtimeTracing(arg1) {
    const obj0 = this;
    const variant1 = arg1;
    let variant1_0;
    switch (variant1) {
      case false: {
        variant1_0 = 0;
        break;
      }
      case true: {
        variant1_0 = 1;
        break;
      }
      default:
      throw new RangeError("invalid variant specified for bool");
    }
    this._obj._exports['config::set_wasmtime_tracing'](this._obj._resource0_slab.insert(obj0.clone()), variant1_0);
  }
  setWasmtimeAsync(arg1) {
    const memory = this._obj._exports.memory;
    const realloc = this._obj._exports["canonical_abi_realloc"];
    const obj0 = this;
    const variant3 = arg1;
    let variant3_0;
    let variant3_1;
    let variant3_2;
    switch (variant3.tag) {
      case "all": {
        variant3_0 = 0;
        variant3_1 = 0;
        variant3_2 = 0;
        break;
      }
      case "none": {
        variant3_0 = 1;
        variant3_1 = 0;
        variant3_2 = 0;
        break;
      }
      case "only": {
        const e = variant3.val;
        const vec2 = e;
        const len2 = vec2.length;
        const result2 = realloc(0, 0, 4, len2 * 8);
        for (let i = 0; i < vec2.length; i++) {
          const e = vec2[i];
          const base = result2 + i * 8;
          const ptr1 = utf8_encode(e, realloc, memory);
          const len1 = UTF8_ENCODED_LEN;
          data_view(memory).setInt32(base + 4, len1, true);
          data_view(memory).setInt32(base + 0, ptr1, true);
        }
        variant3_0 = 2;
        variant3_1 = result2;
        variant3_2 = len2;
        break;
      }
      default:
      throw new RangeError("invalid variant specified for WasmtimeAsync");
    }
    this._obj._exports['config::set_wasmtime_async'](this._obj._resource0_slab.insert(obj0.clone()), variant3_0, variant3_1, variant3_2);
  }
  setWasmtimeCustomError(arg1) {
    const obj0 = this;
    const variant1 = arg1;
    let variant1_0;
    switch (variant1) {
      case false: {
        variant1_0 = 0;
        break;
      }
      case true: {
        variant1_0 = 1;
        break;
      }
      default:
      throw new RangeError("invalid variant specified for bool");
    }
    this._obj._exports['config::set_wasmtime_custom_error'](this._obj._resource0_slab.insert(obj0.clone()), variant1_0);
  }
}
