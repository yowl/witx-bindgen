using System;
using System.Runtime.InteropServices;
using System.Text;


unsafe class hellowasi
{
  [WasmImport(ModuleName="hellowasi", FunctionName="reverse")]
  private static extern void __wasm_import_hellowasi_reverse(int p0, int p1, int p2);
  
  string reverse(string s)
  {
    var utf8Bytes = Encoding.UTF8.GetBytes(s);
    GCHandle pinnedArray = GCHandle.Alloc(utf8Bytes, GCHandleType.Pinned);
    IntPtr ptr = pinnedArray.AddrOfPinnedObject();
    
    var returnedArea = new byte[16];
    GCHandle gcHandle = GCHandle.Alloc(returnedArea, GCHandleType.Pinned);
    int ptr0 = gcHandle.AddrOfPinnedObject().ToInt32();
    __wasm_import_hellowasi_reverse(ptr.ToInt32(), utf8Bytes.Length, ptr0);
    
    int loadPtr = *(int*)(ptr0 + 0);
    
    int loadPtr1 = *(int*)(ptr0 + 8);
    pinnedArray.Free();
    
    var resultArray = new char[loadPtr1];
    Encoding.UTF8.GetChars(new ReadOnlySpan<byte>((void*)loadPtr, loadPtr1), new Span<char>(resultArray));
    gcHandle.Free();
    
    return new string(resultArray);
  }
  
}