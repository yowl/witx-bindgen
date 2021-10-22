import { UTF8_DECODER } from './intrinsics.js';
export function addBrowserToImports(imports, obj, get_export) {
  if (!("browser" in imports)) imports["browser"] = {};
  imports["browser"]["log"] = function(arg0, arg1) {
    const memory = get_export("memory");
    const ptr0 = arg0;
    const len0 = arg1;
    obj.log(UTF8_DECODER.decode(new Uint8Array(memory.buffer, ptr0, len0)));
  };
  imports["browser"]["error"] = function(arg0, arg1) {
    const memory = get_export("memory");
    const ptr0 = arg0;
    const len0 = arg1;
    obj.error(UTF8_DECODER.decode(new Uint8Array(memory.buffer, ptr0, len0)));
  };
}