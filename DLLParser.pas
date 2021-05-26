unit DLLParser;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  JwaWindows;

procedure ExtractDLLFunctions(APath: String; AFunctionList: TStringList);

implementation

procedure ExtractDLLFunctions(APath: String; AFunctionList: TStringList);
var
  I: Integer;
  Mem: Pointer;
  DosHeader: PImageDosHeader;
  ImageFileHeader: PImageFileHeader;
  OptionalHeader: PImageOptionalHeader64;
  Sections: PImageSectionHeader;
  ExportDirectory: PImageExportDirectory;
  Names: PDWORD;
  Name: String;
  offset: QWord;
  fs: TFileStream;

  function RVAToOffset(AAddress: QWord): QWord;
  var
    J: Integer;
  begin
    Result := 0;
    for J := 0 to ImageFileHeader^.NumberOfSections - 1 do
      with Sections[J] do
        if (AAddress >= VirtualAddress) and (AAddress < VirtualAddress + Misc.VirtualSize) then
        begin
          Result := AAddress - VirtualAddress + PointerToRawData;
          Exit;
        end;
  end;

begin
  fs := TFileStream.Create(APath, fmOpenRead);
  Mem := AllocMem(fs.Size);
  fs.Read(Pbyte(Mem)^, fs.Size);
  fs.Free;

  DosHeader := PImageDosHeader(Mem);
  ImageFileHeader := PImageFileHeader(Mem + DosHeader^.e_lfanew + SizeOf(DWORD)); //Skip Signature
  OptionalHeader := PImageOptionalHeader64(Pointer(ImageFileHeader) + IMAGE_SIZEOF_FILE_HEADER);
  Sections := PImageSectionHeader(Pointer(OptionalHeader) + IMAGE_SIZEOF_NT_OPTIONAL64_HEADER);
  offset := RVAToOffset(OptionalHeader^.DataDirectory[IMAGE_DIRECTORY_ENTRY_EXPORT].VirtualAddress);
  ExportDirectory := PImageExportDirectory(Pbyte(Mem) + offset);
  Names := PDWORD(Mem + RVAToOffset(ExportDirectory^.AddressOfNames));
  for I := 0 to ExportDirectory^.NumberOfNames - 1 do
  begin
    Name := Pchar(Mem + RVAToOffset(Names[I]));
    AFunctionList.Add(Name);
  end;
  FreeMemAndNil(mem);
end;

end.
