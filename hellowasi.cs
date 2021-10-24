using System;
using System.Runtime.InteropServices;
using System.Text;


class hellowasi
{
  [WasmImport("hellowasi"), FuncName("reverse")))]
  private static extern void __wasm_import_hellowasi_reverse(int, int, int);
  
  string reverse(string s)
  {
    var returnedArea = new byte[16];
    GCHandle gcHandle = GCHandle.Alloc(returnedArea, GCHandleType.Pinned);
    IntPtr ptr = gcHandle.AddrOfPinnedObject();
    __wasm_import_hellowasi_reverse((int32_t) (s).ptr, (int32_t) (s).len, ptr);
    return (string) { (char*)(*((int32_t*) (ptr + 0))), (size_t)(*((int32_t*) (ptr + 8))) };
  }
  
}