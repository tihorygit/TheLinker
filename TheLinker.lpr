program TheLinker;

{$ASMMODE INTEL}

uses
  Windows,
  SysUtils,
  Linker,
  COFF64;

var
  Coff: TCOFF64Object;
  I: Integer;
begin

  Coff := TCOFF64Object.Create('../tests/ofile/OFile.o');

  WriteLn('Section Count : ', Coff.SectionCount);
  for I := 0 to Coff.SectionCount - 1 do
    WriteLn(I, ' - ', Coff.Section[I].Name);

  WriteLn('Symbol Count : ', Coff.SymbolCount);
  for I := 0 to Coff.SymbolCount - 1 do
    WriteLn(I, ' - ', Coff.Symbol[I].Name);

  ReadLn;
end.
