program TheLinker;

{$ASMMODE INTEL}

uses
  Windows,
  SysUtils,
  Objects;

  procedure Print(AStr: PChar); cdecl;
  begin
    WriteLn(AStr);
  end;

  procedure LoadPascalProgram;
  var
    SystemObj, ObjPasObj, OFileObj, IntResObj: TCoff64Object;
    CoffLinker: TCoff64Linker;
    Kernel32Lib, Oleaut32Lib, User32Lib: TLibHandle;
  begin
    SystemObj := TCoff64Object.Create('../tests/pascal_common_object_files/system.o');
    ObjPasObj := TCoff64Object.Create('../tests/pascal_common_object_files/objpas.o');
    IntResObj := TCoff64Object.Create('../tests/pascal_common_object_files/fpintres.o');
    OFileObj := TCoff64Object.Create('../tests/ofile/OFile.o');

    CoffLinker := TCoff64Linker.Create;
    with CoffLinker do
    begin
      AddObject(SystemObj);
      AddObject(ObjPasObj);
      AddObject(IntResObj);
      AddObject(OFileObj);
      Kernel32Lib := DLLFunctionFakeSymbolGenerator('C:\Windows\System32\kernel32.dll', CoffLinker, '_$dll$kernel32$');
      Oleaut32Lib := DLLFunctionFakeSymbolGenerator('C:\Windows\System32\oleaut32.dll', CoffLinker, '_$dll$oleaut32$');
      User32Lib := DLLFunctionFakeSymbolGenerator('C:\Windows\System32\user32.dll', CoffLinker, '_$dll$user32$');
      Link;
      Run('P$OFILE_$$_FAKEMAIN');
      Free;
    end;

    UnloadLibrary(Kernel32Lib);
    UnloadLibrary(Oleaut32Lib);
    UnloadLibrary(User32Lib);
    SystemObj.Free;
    ObjPasObj.Free;
    OFileObj.Free;
    IntResObj.Free;
  end;

  procedure LoadCPPProgram;
  var
    MainObj, AnotherObj: TCoff64Object;
    CoffLinker: TCoff64Linker;
  begin
    MainObj := TCoff64Object.Create('../tests/cpp_print/main.cpp.obj');
    AnotherObj := TCoff64Object.Create('../tests/cpp_print/another.cpp.obj');
    CoffLinker := TCoff64Linker.Create;
    with CoffLinker do
    begin
      AddObject(MainObj);
      AddObject(AnotherObj);
      AddFakeSymbol(@Print, 'print');
      DLLFunctionFakeSymbolGenerator('C:\Windows\System32\kernel32.dll', CoffLinker, '_$dll$kernel32$');
      Link;
      Run('main');
      Free;
    end;
    MainObj.Free;
    AnotherObj.Free;
  end;

begin
  LoadPascalProgram;
  ReadLn;
end.
