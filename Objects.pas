unit Objects;

{$mode objfpc}{$H+}

interface

uses
  JwaWindows, Classes, windows, SysUtils, fgl, Math;

type

  TCoff64Object = class;

  { TCoff64List }

  generic TCoff64List<T> = class(specialize TFPGList<T>)
  private
    FOwner: TCoff64Object;
    FOwned: Boolean;
  protected
  public
    constructor Create(AOwner: TCoff64Object; AOwned: Boolean = True);
    destructor Destroy; override;
  end;

  { TCoff64Table }

  generic TCoff64Table<T> = class
  private
    FOwner: TCoff64Object;
    FTable: Pbyte;
    FCount: DWORD;
  protected
    function GetElement(Index: DWORD): T; virtual; abstract;
  public
    constructor Create(AOwner: TCoff64Object; ATable: Pbyte; ACount: DWORD);
    property Item[Index: DWORD]: T read GetElement; default;
    property Count: DWORD read FCount;
  end;

  { TCoff64Symbol }

  TCoff64Symbol = class
  private
    FOwner: TCoff64Object;
    FSymbol: PImageSymbol;
    function GetAddress: Pointer;
    function GetIsExternal: Boolean;
    function GetIsStatic: Boolean;
    function GetIsUndefined: Boolean;
    function GetName: String;
    function GetNumberOfAuxSymbol: DWord;
    function GetSectionNumber: SHORT;
  private //Friends
    function GetIsAbsolute: Boolean;
    function GetIsDebug: Boolean;
  public
    constructor Create(AOwner: TCoff64Object; ASymbol: PImageSymbol);
    property Name: String read GetName;
    property Address: Pointer read GetAddress;
    property SectionNumber: SHORT read GetSectionNumber;
    property NumberOfAuxSymbol: DWord read GetNumberOfAuxSymbol;

    property IsExternal: Boolean read GetIsExternal;
    property IsStatic: Boolean read GetIsStatic;
    property IsUndefined: Boolean read GetIsUndefined;
    property IsAbsolute: Boolean read GetIsAbsolute;
    property IsDebug: Boolean read GetIsDebug;

  end;

  TCoff64SymbolList = specialize TCoff64List<TCoff64Symbol>;

  { TCoff64Relocation }

  TCoff64Relocation = class
  private
    FOwner: TCoff64Object;
    FRelocation: PImageRelocation;
    function GetTyp: QWord;
    function GetVirtualAddress: QWord;
    function GetSymbolTableIndex: QWord;
  public
    constructor Create(AOwner: TCoff64Object; ARelocation: PImageRelocation);
    property VirtualAddress: QWord read GetVirtualAddress;
    property SymbolTableIndex: QWord read GetSymbolTableIndex;
    property Typ: QWord read GetTyp;
  end;

  TCoff64RelocationList = specialize TCoff64List<TCoff64Relocation>;

  { TCoff64Section }

  TCoff64Section = class
  private
    FOwner: TCoff64Object;
    FSection: PImageSectionHeader;
    FInternalMemory: Pointer;
    FRelocations: TCoff64RelocationList;
    function GetIsCode: Boolean;
    function GetIsDiscardable: Boolean;
    function GetIsInitializedData: Boolean;
    function GetIsUninitializedData: Boolean;
    function GetIsWritable: Boolean;
    function GetRawData: Pointer;
    function GetSize: QWord;

    procedure ParseRelocation;

  private //Friends
    procedure AllocateMemory;
    function GetRelocation(Index: QWord): TCoff64Relocation;
    function GetRelocationCount: QWord;
  public
    constructor Create(AOwner: TCoff64Object; ASection: PImageSectionHeader);
    destructor Destroy; override;
    property RawData: Pointer read GetRawData;
    property Size: QWord read GetSize;
    property IsUninitializedData: Boolean read GetIsUninitializedData;
    property IsCode: Boolean read GetIsCode;
    property IsInitializedData: Boolean read GetIsInitializedData;
    property IsDiscardable: Boolean read GetIsDiscardable;
    property IsWritable: Boolean read GetIsWritable;
    property RelocationCount: QWord read GetRelocationCount;
    property Relocation[Index: QWord]: TCoff64Relocation read GetRelocation;
  end;

  TCoff64SectionList = specialize TCoff64List<TCoff64Section>;

  { TCoff64SymbolTable }

  TCoff64SymbolTable = class(specialize TCoff64Table<TCoff64Symbol>)
  private
    FSymbols: TCoff64SymbolList;
  protected
    function GetElement(Index: DWORD): TCoff64Symbol; override;
  public
    constructor Create(AOwner: TCoff64Object; ATable: Pbyte; ACount: DWORD);
    destructor Destroy; override;
  end;

  { TCoff64StringTable }

  TCoff64StringTable = class(specialize TCoff64Table<String>)
  private
  protected
    function GetElement(Index: DWORD): String; override;
  public

  end;

  { TCoff64Object }

  TCoff64Object = class
  private
    FImage: Pbyte;
    FImageBase: Pbyte;
    FImageFileHeader: PImageFileHeader;
    FSymbolTable: TCoff64SymbolTable;
    FStringTable: TCoff64StringTable;
    FSections: TCoff64SectionList;
    function GetSection(Index: Integer): TCoff64Section;
    function GetSectionCount: DWORD;
    function GetSymbolCount: DWORD;
    procedure ParseSections;
    procedure LoadFromFile(AFileName: String);
  public
    constructor Create(AFileName: String);
    destructor Destroy; override;
    function FindSymbol(AName: String): TCoff64Symbol;
    property Image: Pbyte read FImage;
    property SectionCount: DWORD read GetSectionCount;
    property SymbolCount: DWORD read GetSymbolCount;
    property SymbolTable: TCoff64SymbolTable read FSymbolTable;
    property StringTable: TCoff64StringTable read FStringTable;
    property Sections[Index: Integer]: TCoff64Section read GetSection;
    property ImageBase: Pbyte read FImageBase;
  end;

  TCoff64ObjectList = specialize TFPGList<TCoff64Object>;

  { TCoff64DelayRelocation }

  TCoff64DelayRelocation = class
  private
    FRelocation: TCoff64Relocation;
    FRelocationObject: TCoff64Object;
    FRelocationSection: TCoff64Section;
  public
    property RelocationSection: TCoff64Section read FRelocationSection write FRelocationSection;
    property Relocation: TCoff64Relocation read FRelocation write FRelocation;
    property RelocationObject: TCoff64Object read FRelocationObject write FRelocationObject;
  end;

  TCoff64DelayRelocationList = specialize TCoff64List<TCoff64DelayRelocation>;

  { TCoff64FakeSymbol }

  TCoff64FakeSymbol = class
  private
    FAddress: Pointer;
    FName: String;
  public
    constructor Create(AAddress: Pointer; AName: String);
    property Address: Pointer read FAddress write FAddress;
    property Name: String read FName write FName;
  end;

  TCoff64FakeSymbolList = specialize TCoff64List<TCoff64FakeSymbol>;

  { TCoff64Linker }

  TCoff64Linker = class
  private
    FObjects: TCoff64ObjectList;
    FExports: TCoff64SymbolList;
    FImports: TCoff64SymbolList;
    FFakeSymbols: TCoff64FakeSymbolList;
    FDelayRelocations: TCoff64DelayRelocationList;
    FCodeSize, FBSSSize, FDataSize, FRDataSize: QWord;
    FRelocCount: QWord;
    FResolvedRelocCount: QWord;

    FFakeSymbolsJumpMemory: Pointer;
    FFakeSymbolsBase: QWord;

    procedure LinkObject(AObject: TCoff64Object);
    procedure ResolveDelayLink;
    procedure RelocateRelative(AObject: TCoff64Object; ARelSection, ASymSection: TCoff64Section; ASymbol: TCoff64Symbol; ARelocation: TCoff64Relocation);
    procedure RelocateAbsolute(AObject: TCoff64Object; ARelSection, ASymSection: TCoff64Section; ASymbol: TCoff64Symbol; ARelocation: TCoff64Relocation);
    procedure RelocateImageBase(AObject: TCoff64Object; ARelSection, ASymSection: TCoff64Section; ASymbol: TCoff64Symbol; ARelocation: TCoff64Relocation);
    procedure RelocateSectionRelative(AObject: TCoff64Object; ARelSection, ASymSection: TCoff64Section; ASymbol: TCoff64Symbol; ARelocation: TCoff64Relocation);

    procedure RelocateRelativeFake(ARelSection: TCoff64Section; ASymbol: TCoff64FakeSymbol; ARelocation: TCoff64Relocation);
    procedure RelocateAbsoluteFake(ARelSection: TCoff64Section; ASymbol: TCoff64FakeSymbol; ARelocation: TCoff64Relocation);
    procedure RelocateImageBaseFake(ARelSection: TCoff64Section; ASymbol: TCoff64FakeSymbol; ARelocation: TCoff64Relocation);
    procedure RelocateSectionRelativeFake(ARelSection: TCoff64Section; ASymbol: TCoff64FakeSymbol; ARelocation: TCoff64Relocation);
  public
    constructor Create;
    procedure AddObject(AObject: TCoff64Object);
    procedure AddFakeSymbol(AAddress: Pointer; AName: String);
    procedure Link;
    procedure LogSymbols;
    function FindSymbol(AName: String): TCoff64Symbol;
    function FindFakeSymbol(AName: String): TCoff64FakeSymbol;
    procedure Run(AMainSymbolName: String);
    destructor Destroy; override;
  end;

function DLLFunctionFakeSymbolGenerator(ADll: String; ALinker: TCoff64Linker; Mangle: String): TLibHandle;

implementation

function DLLFunctionFakeSymbolGenerator(ADll: String; ALinker: TCoff64Linker; Mangle: String): TLibHandle;
var
  I: Integer;
  Mem, P: Pointer;
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
  Result := system.LoadLibrary(ADll);
  if (Result = 0) then
    Exit;

  fs := TFileStream.Create(ADll, fmOpenRead);
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
    P := system.GetProcAddress(Result, Name);
    ALinker.AddFakeSymbol(P, Mangle + Name);
  end;
  FreeMemAndNil(mem);
end;

{ TCoff64FakeSymbol }

constructor TCoff64FakeSymbol.Create(AAddress: Pointer; AName: String);
begin
  FAddress := AAddress;
  FName := AName;
end;

{ TCoff64Relocation }

function TCoff64Relocation.GetVirtualAddress: QWord;
begin
  Result := FRelocation^.Union.VirtualAddress;
end;

function TCoff64Relocation.GetTyp: QWord;
begin
  Result := FRelocation^.Type_;
end;

function TCoff64Relocation.GetSymbolTableIndex: QWord;
begin
  Result := FRelocation^.SymbolTableIndex;
end;

constructor TCoff64Relocation.Create(AOwner: TCoff64Object; ARelocation: PImageRelocation);
begin
  FOwner := AOwner;
  FRelocation := ARelocation;
end;

{ TCoff64Linker }

procedure TCoff64Linker.LinkObject(AObject: TCoff64Object);
var
  I, J: Integer;
  Sect, RSect, SSect: TCoff64Section;
  Sym: TCoff64Symbol;
  Rel: TCoff64Relocation;
  DRel: TCoff64DelayRelocation;
begin
  //Calculate Sizes & Allocate memories
  for I := 0 to AObject.SectionCount - 1 do
  begin
    Sect := AObject.Sections[I];
    if Sect.IsUninitializedData then
    begin
      FBSSSize += Sect.Size;
      Sect.AllocateMemory;
    end;
    if Sect.IsCode then
      FCodeSize += Sect.Size;
    if Sect.IsInitializedData then
      if Sect.IsWritable then
        FDataSize += Sect.Size
      else
        FRDataSize += Sect.Size;
  end;

  //Find Import & Export Symbols
  I := 0;
  while I < AObject.SymbolCount do
  begin
    Sym := AObject.SymbolTable[I];
    with Sym do
    begin
      if IsExternal or (IsStatic and (SectionNumber > 0) and (NumberOfAuxSymbol = 0)) then
        if IsUndefined then
          FImports.Add(Sym)
        else
          FExports.Add(Sym);
      I += NumberOfAuxSymbol + 1;
    end;
  end;

  //Relocation
  for I := 0 to AObject.SectionCount - 1 do
  begin
    RSect := AObject.Sections[I];
    if RSect.RelocationCount = 0 then
      Continue;
    if RSect.IsDiscardable then
      Continue;
    for J := 0 to RSect.RelocationCount - 1 do
    begin
      Rel := RSect.Relocation[J];
      Sym := AObject.SymbolTable[Rel.SymbolTableIndex];

      if Sym.IsUndefined then
      begin
        Inc(FRelocCount);
        DRel := TCoff64DelayRelocation.Create;
        with DRel do
        begin
          Relocation := Rel;
          RelocationSection := RSect;
          RelocationObject := AObject;
        end;
        FDelayRelocations.Add(DRel);
        Continue;
      end;

      if Sym.IsAbsolute then Continue; //Not supported yet
      if Sym.IsDebug then Continue; //Not supported yet

      SSect := AObject.Sections[Sym.SectionNumber - 1];

      case Rel.Typ of
        IMAGE_REL_AMD64_REL32..IMAGE_REL_AMD64_REL32_5: RelocateRelative(AObject, RSect, SSect, Sym, Rel);
        IMAGE_REL_AMD64_ADDR32NB: RelocateImageBase(AObject, RSect, SSect, Sym, Rel);
        IMAGE_REL_AMD64_ADDR64: RelocateAbsolute(AObject, RSect, SSect, Sym, Rel);
        IMAGE_REL_AMD64_SECREL: RelocateSectionRelative(AObject, RSect, SSect, Sym, Rel);
      end;
    end;
  end;
end;

procedure TCoff64Linker.ResolveDelayLink;
var
  DRel: TCoff64DelayRelocation;
  Rel: TCoff64Relocation;
  RSect, SSect: TCoff64Section;
  Sym, RSym: TCoff64Symbol;
  I: Integer;
  FSym: TCoff64FakeSymbol;
begin
  for I := 0 to FDelayRelocations.Count - 1 do
  begin
    DRel := FDelayRelocations[I];
    Rel := DRel.Relocation;
    RSect := DRel.RelocationSection;
    Sym := DRel.RelocationObject.SymbolTable[Rel.SymbolTableIndex];
    RSym := FindSymbol(Sym.Name);
    if RSym <> nil then
    begin
      SSect := RSym.FOwner.Sections[RSym.SectionNumber - 1];
      case Rel.Typ of
        IMAGE_REL_AMD64_REL32..IMAGE_REL_AMD64_REL32_5: RelocateRelative(RSym.FOwner, RSect, SSect, RSym, Rel);
        IMAGE_REL_AMD64_ADDR32NB: RelocateImageBase(RSym.FOwner, RSect, SSect, RSym, Rel);
        IMAGE_REL_AMD64_ADDR64: RelocateAbsolute(RSym.FOwner, RSect, SSect, RSym, Rel);
        IMAGE_REL_AMD64_SECREL: RelocateSectionRelative(RSym.FOwner, RSect, SSect, RSym, Rel);
      end;
    end
    else
    begin
      FSym := FindFakeSymbol(Sym.Name);
      if FSym <> nil then
      begin
        case Rel.Typ of
          IMAGE_REL_AMD64_REL32..IMAGE_REL_AMD64_REL32_5: RelocateRelativeFake(RSect, FSym, Rel);
          IMAGE_REL_AMD64_ADDR32NB: RelocateImageBaseFake(RSect, FSym, Rel);
          IMAGE_REL_AMD64_ADDR64: RelocateAbsoluteFake(RSect, FSym, Rel);
          IMAGE_REL_AMD64_SECREL: RelocateSectionRelativeFake(RSect, FSym, Rel);
        end;
      end
      else
        raise Exception.Create('Can''t Find Symbol: ' + Sym.Name);
    end;
  end;
end;

procedure TCoff64Linker.RelocateRelative(AObject: TCoff64Object; ARelSection, ASymSection: TCoff64Section; ASymbol: TCoff64Symbol; ARelocation: TCoff64Relocation);
var
  Delta: DWORD;
  RelAddress, SymAddress: Pointer;
begin
  Delta := 4 + ARelocation.Typ - IMAGE_REL_AMD64_REL32;
  RelAddress := ARelSection.RawData + ARelocation.VirtualAddress;
  SymAddress := ASymbol.Address;
  PDWORD(RelAddress)^ := DWord(QWord(SymAddress) - QWord(RelAddress) - Delta);
end;

procedure TCoff64Linker.RelocateAbsolute(AObject: TCoff64Object; ARelSection, ASymSection: TCoff64Section; ASymbol: TCoff64Symbol; ARelocation: TCoff64Relocation);
var
  RelAddress, SymAddress: Pointer;
begin
  RelAddress := ARelSection.RawData + ARelocation.VirtualAddress;
  SymAddress := ASymbol.Address;
  PQWord(RelAddress)^ := QWord(SymAddress);
end;

procedure TCoff64Linker.RelocateImageBase(AObject: TCoff64Object; ARelSection, ASymSection: TCoff64Section; ASymbol: TCoff64Symbol; ARelocation: TCoff64Relocation);
var
  RelAddress, SymAddress: Pointer;
begin
  RelAddress := ARelSection.RawData + ARelocation.VirtualAddress;
  SymAddress := ASymbol.Address;
  PDWORD(RelAddress)^ := DWord(QWord(SymAddress) - QWord(AObject.ImageBase));
end;

procedure TCoff64Linker.RelocateSectionRelative(AObject: TCoff64Object; ARelSection, ASymSection: TCoff64Section; ASymbol: TCoff64Symbol; ARelocation: TCoff64Relocation);
begin
  raise Exception.Create('Section Relative Relocation not supported yet');
end;

procedure TCoff64Linker.RelocateRelativeFake(ARelSection: TCoff64Section; ASymbol: TCoff64FakeSymbol; ARelocation: TCoff64Relocation);
var
  Delta: DWORD;
  RelAddress, SymAddress: Pointer;
begin
  Delta := 4 + ARelocation.Typ - IMAGE_REL_AMD64_REL32;
  RelAddress := ARelSection.RawData + ARelocation.VirtualAddress;
  SymAddress := ASymbol.Address;
  PDWORD(RelAddress)^ := DWord(QWord(SymAddress) - QWord(RelAddress) - Delta);
end;

procedure TCoff64Linker.RelocateAbsoluteFake(ARelSection: TCoff64Section; ASymbol: TCoff64FakeSymbol; ARelocation: TCoff64Relocation);
var
  RelAddress, SymAddress: Pointer;
begin
  RelAddress := ARelSection.RawData + ARelocation.VirtualAddress;
  SymAddress := ASymbol.Address;
  PQWord(RelAddress)^ := QWord(SymAddress);
end;

procedure TCoff64Linker.RelocateImageBaseFake(ARelSection: TCoff64Section; ASymbol: TCoff64FakeSymbol; ARelocation: TCoff64Relocation);
var
  RelAddress, SymAddress: Pointer;
begin
  RelAddress := ARelSection.RawData + ARelocation.VirtualAddress;
  SymAddress := ASymbol.Address;
  PDWORD(RelAddress)^ := DWord(QWord(SymAddress) - QWord(GetModuleHandle(nil)));
end;

procedure TCoff64Linker.RelocateSectionRelativeFake(ARelSection: TCoff64Section; ASymbol: TCoff64FakeSymbol; ARelocation: TCoff64Relocation);
begin
  raise Exception.Create('Fake Section Relative Relocation not supported yet');
end;

constructor TCoff64Linker.Create;
begin
  FObjects := TCoff64ObjectList.Create;
  FExports := TCoff64SymbolList.Create(nil, False);
  FImports := TCoff64SymbolList.Create(nil, False);
  FDelayRelocations := TCoff64DelayRelocationList.Create(nil);
  FFakeSymbols := TCoff64FakeSymbolList.Create(nil);
  FCodeSize := 0;
  FBSSSize := 0;
  FDataSize := 0;
  FRDataSize := 0;
  FRelocCount := 0;
  FResolvedRelocCount := 0;
  FFakeSymbolsJumpMemory := VirtualAlloc(nil, 49152, MEM_COMMIT, PAGE_EXECUTE_READWRITE);
  FFakeSymbolsBase := 0;
end;

procedure TCoff64Linker.AddObject(AObject: TCoff64Object);
begin
  FObjects.Add(AObject);
end;

procedure TCoff64Linker.AddFakeSymbol(AAddress: Pointer; AName: String);
var
  Mem: Pbyte;
  FSym: TCoff64FakeSymbol;
begin
  Mem := Pbyte(FFakeSymbolsJumpMemory + FFakeSymbolsBase);
  FSym := TCoff64FakeSymbol.Create(Mem, AName);
  Mem[0] := 72;
  Mem[1] := 184;
  PQWord(@Mem[2])^ := QWord(AAddress);
  Mem[10] := 255;
  Mem[11] := 224;
  FFakeSymbolsBase += 12;
  FFakeSymbols.Add(FSym);
end;

procedure TCoff64Linker.Link;
var
  I: Integer;
begin
  for I := 0 to FObjects.Count - 1 do
    LinkObject(FObjects[I]);
  ResolveDelayLink;
end;

procedure TCoff64Linker.LogSymbols;
var
  I, J: Integer;
begin
  for I := 0 to FObjects.Count - 1 do
  begin
    with FObjects[I] do
      for J := 0 to SymbolCount - 1 do
        with SymbolTable[J] do
          if SectionNumber > 0 then
            WriteLn(IntToHex(QWord(Address)), #9, Name);
  end;

  for I := 0 to FFakeSymbols.Count - 1 do
  begin
    with FFakeSymbols[I] do
      WriteLn(IntToHex(QWord(Address)), #9, Name);
  end;
end;

function TCoff64Linker.FindSymbol(AName: String): TCoff64Symbol;
var
  I, J: Integer;
  Obj: TCoff64Object;
  Sym: TCoff64Symbol;
begin
  Result := nil;
  for I := 0 to FObjects.Count - 1 do
  begin
    Obj := FObjects[I];
    for J := 0 to Obj.SymbolCount - 1 do
    begin
      Sym := Obj.SymbolTable[J];
      if (Sym.IsUndefined or Sym.IsAbsolute or Sym.IsDebug) then Continue;
      if (Sym.Name = AName) then
      begin
        Result := Sym;
        Exit;
      end;
    end;
  end;
end;

function TCoff64Linker.FindFakeSymbol(AName: String): TCoff64FakeSymbol;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to FFakeSymbols.Count - 1 do
  begin
    if FFakeSymbols[I].Name = AName then
    begin
      Result := FFakeSymbols[I];
      Exit;
    end;
  end;
end;

procedure TCoff64Linker.Run(AMainSymbolName: String);
type
  TMainProc = procedure;
var
  Sym: TCoff64Symbol;
  Main: TMainProc;
begin
  Sym := FindSymbol(AMainSymbolName);
  if Sym = nil then
    raise Exception.Create('Can''t find main procedure');
  Main := TMainProc(Sym.Address);
  Main();
end;

destructor TCoff64Linker.Destroy;
begin
  FreeAndNil(FObjects);
  FreeAndNil(FExports);
  FreeAndNil(FImports);
  FreeAndNil(FDelayRelocations);
  FreeAndNil(FFakeSymbols);
  inherited Destroy;
end;

{ TCoff64SymbolTable }

function TCoff64SymbolTable.GetElement(Index: DWORD): TCoff64Symbol;
begin
  Result := FSymbols[Index];
end;

constructor TCoff64SymbolTable.Create(AOwner: TCoff64Object; ATable: Pbyte; ACount: DWORD);
var
  PSym: PImageSymbol;
  I: Integer;
begin
  inherited Create(AOwner, ATable, ACount);
  FSymbols := TCoff64SymbolList.Create(AOwner);
  for I := 0 to ACount - 1 do
  begin
    PSym := PImageSymbol(ATable + IMAGE_SIZEOF_SYMBOL * I);
    FSymbols.Add(TCoff64Symbol.Create(AOwner, PSym));
  end;
end;

destructor TCoff64SymbolTable.Destroy;
begin
  FSymbols.Free;
  inherited Destroy;
end;

{ TCoff64StringTable }

function TCoff64StringTable.GetElement(Index: DWORD): String;
var
  P: PChar;
begin
  P := Pchar(FTable + Index);
  Result := StrPas(p);
end;

{ TCoff64Symbol }

function TCoff64Symbol.GetName: String;
begin
  if (FSymbol^.N.Short = 0) then
  begin
    Result := FOwner.StringTable[FSymbol^.N.Long];
  end
  else
  begin
    SetString(Result, Pchar(@FSymbol^.N.ShortName[0]), 9);
    SetLength(Result, strlen(@Result[1]));
  end;
end;

function TCoff64Symbol.GetNumberOfAuxSymbol: DWord;
begin
  Result := FSymbol^.NumberOfAuxSymbols;
end;

function TCoff64Symbol.GetSectionNumber: SHORT;
begin
  Result := FSymbol^.SectionNumber;
end;

function TCoff64Symbol.GetIsAbsolute: Boolean;
begin
  Result := FSymbol^.SectionNumber = IMAGE_SYM_ABSOLUTE;
end;

function TCoff64Symbol.GetIsDebug: Boolean;
begin
  Result := FSymbol^.SectionNumber = IMAGE_SYM_DEBUG;
end;

function TCoff64Symbol.GetAddress: Pointer;
begin
  Result := FSymbol^.Value + FOwner.Sections[FSymbol^.SectionNumber - 1].RawData;
end;

function TCoff64Symbol.GetIsExternal: Boolean;
begin
  Result := FSymbol^.StorageClass = IMAGE_SYM_CLASS_EXTERNAL;
end;

function TCoff64Symbol.GetIsStatic: Boolean;
begin
  Result := FSymbol^.StorageClass = IMAGE_SYM_CLASS_STATIC;
end;

function TCoff64Symbol.GetIsUndefined: Boolean;
begin
  Result := FSymbol^.SectionNumber = IMAGE_SYM_UNDEFINED;
end;

constructor TCoff64Symbol.Create(AOwner: TCoff64Object; ASymbol: PImageSymbol);
begin
  FOwner := AOwner;
  FSymbol := ASymbol;
end;

{ TCoff64Section }

function TCoff64Section.GetRawData: Pointer;
begin
  if FSection^.PointerToRawData = 0 then
    if IsUninitializedData then
      Result := FInternalMemory
    else
      Result := nil
  else
    Result := FOwner.Image + FSection^.PointerToRawData;
end;

function TCoff64Section.GetSize: QWord;
begin
  Result := FSection^.SizeOfRawData;
end;

procedure TCoff64Section.ParseRelocation;
var
  Item: PImageRelocation;
  I: Integer;
begin
  if FSection^.PointerToRelocations <> 0 then
    for I := 0 to FSection^.NumberOfRelocations - 1 do
    begin
      Item := PImageRelocation(FOwner.Image + FSection^.PointerToRelocations + IMAGE_SIZEOF_RELOCATION * I);
      FRelocations.Add(TCoff64Relocation.Create(FOwner, Item));
    end;
end;

procedure TCoff64Section.AllocateMemory;
begin
  if FInternalMemory <> nil then Exit;
  if not IsUninitializedData then raise Exception.Create('Only uninitialized data section can allocate memory');
  FInternalMemory := AllocMem(Size);
end;

function TCoff64Section.GetRelocation(Index: QWord): TCoff64Relocation;
begin
  Result := FRelocations[Index];
end;

function TCoff64Section.GetRelocationCount: QWord;
begin
  Result := FSection^.NumberOfRelocations;
end;

function TCoff64Section.GetIsCode: Boolean;
begin
  Result := FSection^.Characteristics and IMAGE_SCN_CNT_CODE <> 0;
end;

function TCoff64Section.GetIsDiscardable: Boolean;
begin
  Result := FSection^.Characteristics and IMAGE_SCN_MEM_DISCARDABLE <> 0;
end;

function TCoff64Section.GetIsInitializedData: Boolean;
begin
  Result := FSection^.Characteristics and IMAGE_SCN_CNT_INITIALIZED_DATA <> 0;
end;

function TCoff64Section.GetIsUninitializedData: Boolean;
begin
  Result := FSection^.Characteristics and IMAGE_SCN_CNT_UNINITIALIZED_DATA <> 0;
end;

function TCoff64Section.GetIsWritable: Boolean;
begin
  Result := FSection^.Characteristics and IMAGE_SCN_MEM_WRITE <> 0;
end;

constructor TCoff64Section.Create(AOwner: TCoff64Object; ASection: PImageSectionHeader);
begin
  FOwner := AOwner;
  FSection := ASection;
  FInternalMemory := nil;
  FRelocations := TCoff64RelocationList.Create(AOwner);
  ParseRelocation;
end;

destructor TCoff64Section.Destroy;
begin
  FRelocations.Free;
  inherited Destroy;
end;

{ TCoff64List }

constructor TCoff64List.Create(AOwner: TCoff64Object; AOwned: Boolean);
begin
  inherited Create;
  FOwner := AOwner;
  FOwned := AOwned;
end;

destructor TCoff64List.Destroy;
var
  It: T;
begin
  if FOwned then
    while Count > 0 do
    begin
      It := Items[0];
      Delete(0);
      FreeAndNil(It);
    end;
  inherited Destroy;
end;

{ TCoff64Table }

constructor TCoff64Table.Create(AOwner: TCoff64Object; ATable: Pbyte; ACount: DWORD);
begin
  FOwner := AOwner;
  FTable := ATable;
  FCount := ACount;
end;

{ TCoff64Object }

function TCoff64Object.GetSectionCount: DWORD;
begin
  Result := FImageFileHeader^.NumberOfSections;
end;

function TCoff64Object.GetSection(Index: Integer): TCoff64Section;
begin
  Result := FSections[Index];
end;

function TCoff64Object.GetSymbolCount: DWORD;
begin
  Result := FImageFileHeader^.NumberOfSymbols;
end;

procedure TCoff64Object.ParseSections;
var
  PSec: PImageSectionHeader;
  I: Integer;
  IB: QWord;
begin
  IB := MAXLONGLONG;
  PSec := PImageSectionHeader(FImage + SizeOf(TImageFileHeader));
  for I := 0 to FImageFileHeader^.NumberOfSections - 1 do
  begin
    if (PSec^.PointerToRawData <> 0) then
    begin
      FSections.Add(TCoff64Section.Create(Self, PSec));
      IB := Min(PSec^.PointerToRawData, IB);
    end
    else
      FSections.Add(TCoff64Section.Create(Self, PSec));
    Inc(PSec);
  end;
  FImageBase := FImage + IB;
end;

procedure TCoff64Object.LoadFromFile(AFileName: String);
var
  fs: TFileStream;
begin
  try
    fs := TFileStream.Create(AFileName, fmOpenRead);
    FImage := VirtualAlloc(nil, ((fs.Size + 4096 - 1) div 4096) * 4096, MEM_COMMIT, PAGE_EXECUTE_READWRITE);
    if (FImage = nil) then
      raise Exception.Create('Can''t create virual memory');
    fs.Read(FImage^, fs.Size);
    FImageFileHeader := PImageFileHeader(FImage);
    if (FImageFileHeader^.Machine <> IMAGE_FILE_MACHINE_AMD64) then
      raise Exception.Create('Invalid COFF object file');
    FStringTable := TCoff64StringTable.Create(self, FImage + FImageFileHeader^.PointerToSymbolTable + IMAGE_SIZEOF_SYMBOL * FImageFileHeader^.NumberOfSymbols, 0);
    FSymbolTable := TCoff64SymbolTable.Create(Self, FImage + FImageFileHeader^.PointerToSymbolTable, FImageFileHeader^.NumberOfSymbols);
    ParseSections;
  finally
    fs.Free;
  end;
end;

constructor TCoff64Object.Create(AFileName: String);
begin
  FSections := TCoff64SectionList.Create(Self);
  FImage := nil;
  FSymbolTable := nil;
  FStringTable := nil;
  LoadFromFile(AFileName);
end;

destructor TCoff64Object.Destroy;
begin
  FreeAndNil(FSymbolTable);
  FreeAndNil(FStringTable);
  FreeAndNil(FSections);
  if (FImage <> nil) then
    VirtualFree(FImage, 0, MEM_RELEASE);
  inherited Destroy;
end;

function TCoff64Object.FindSymbol(AName: String): TCoff64Symbol;
var
  I: Integer;
begin
  Result := nil;
  I := 0;
  while I < SymbolCount do
  begin
    try
      if (SymbolTable[I].Name = AName) then
      begin
        Result := SymbolTable[I];
        Exit;
      end;
    except
      Inc(I);
      Continue;
    end;
    Inc(I);
  end;
end;

end.
