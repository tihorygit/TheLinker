unit COFF64;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Contnrs, Math,
  JwaWindows,
  Linker;

type

  TCOFF64SymbolFlag = (sfAbsolute, sfDebug, sfExternal, sfStatic, sfUndefined);
  TCOFF64SymbolFlags = set of TCOFF64SymbolFlag;

  TCOFF64SectionFlag = (sfDiscardable, sfInitializedData, sfUninitializedData, sfCode, sfWritable);
  TCOFF64SectionFlags = set of TCOFF64SectionFlag;

  TCOFF64RelocationFlag = (rfAbsolute, rfAddr64, rfAddr32, rfAddr32NB, rfRel32, rfRel32_1, rfRel32_2, rfRel32_3, rfRel32_4, rfRel32_5, rfSection, rfSecRel, rfToken, rfPair, rfSSPAN32);

  TCOFF64Object = class;
  TCOFF64Section = class;

  { TCOFF64Symbol }

  TCOFF64Symbol = class(TLinkerObjectSymbol)
  private
    FOwner: TCOFF64Object;
    FInternalSymbol: TImageSymbol;
    FSection: TCOFF64Section;
    FName: String;
    FFlags: TCOFF64SymbolFlags;
    FValue: QWord;
    FNumberOfAuxSymbol: QWord;
  protected
    function GetIsAbsolute: Boolean; override;
    function GetIsDebug: Boolean; override;
    function GetIsExternal: Boolean; override;
    function GetIsStatic: Boolean; override;
    function GetIsUndefined: Boolean; override;
    function GetName: String; override;
    function GetNumberOfAuxSymbol: DWord; override;
    function GetSection: TLinkerObjectSection; override;
    function GetValue: QWord; override;
    procedure SetValue(AValue: QWord); override;
  public
    constructor Create(AOwner: TCOFF64Object);
    destructor Destroy; override;
  end;

  { TCOFF64Relocation }

  TCOFF64Relocation = class(TLinkerObjectRelocation)
  private
    FOwner: TCOFF64Object;
    FInternalRelocation: TImageRelocation;
    FSymbol: TCOFF64Symbol;
    FVirtualAddress: QWord;
    FFlag: TCOFF64RelocationFlag;
    FSection: TCOFF64Section;
  protected
    function GetSymbol: TLinkerObjectSymbol; override;
    function GetVirtualAddress: QWord; override;
    procedure SetVirtualAddress(AValue: QWord); override;
    function GetSection: TLinkerObjectSection; override;
  public
    constructor Create(AOwner: TCOFF64Object);
    property Flag: TCOFF64RelocationFlag read FFlag;
  end;

  { TCOFF64Section }

  TCOFF64Section = class(TLinkerObjectSection)
  private
    FOwner: TCOFF64Object;
    FInternalSection: TImageSectionHeader;
    FData: Pointer;
    FDataSize: QWord;
    FSymbols: TObjectList;
    FRelocations: TObjectList;
    FName: String;
    FFlags: TCOFF64SectionFlags;
  protected
    function GetName: String; override;
    function GetRelocation(Index: QWord): TLinkerObjectRelocation; override;
    function GetRelocationCount: QWord; override;
    function GetSymbol(Index: QWord): TLinkerObjectSymbol; override;
    function GetSymbolCount: QWord; override;
    function GetData: Pbyte; override;
    function GetSize: QWord; override;
    function GetIsCode: Boolean; override;
    function GetIsDicardable: Boolean; override;
    function GetIsIntializedData: Boolean; override;
    function GetIsUninitalizedData: Boolean; override;
    function GetIsWritable: Boolean; override;
  public
    constructor Create(AOwner: TCOFF64Object);
    destructor Destroy; override;
    procedure AddSymbol(ASymbol: TCOFF64Symbol);
  end;

  { TCOFF64Object }

  TCOFF64Object = class(TLinkerObject)
  private
    FPath: String;
    FSections: TObjectList;
    FSymbols: TObjectList;
    FStream: TStream;
    FImageFileHeader: TImageFileHeader;
    FImageBase: QWord;
    procedure LoadFromFile;
    procedure AddSection(ASection: TCOFF64Section);
    procedure AddSymbol(ASymbol: TCOFF64Symbol);
    procedure AddRelocation(ASection: TCOFF64Section; ARelocation: TCOFF64Relocation);
  protected
    function GetFileName: String; override;
    function GetSection(Index: QWord): TLinkerObjectSection; override;
    function GetSectionCount: QWord; override;
    function GetSymbol(Index: QWord): TLinkerObjectSymbol; override;
    function GetSymbolCount: QWord; override;
    function GetImageBase: QWord; override;
  public
    constructor Create(APath: String);
    destructor Destroy; override;
    function GetString(At: QWord): String;
  end;

implementation

{ TCOFF64Relocation }

function TCOFF64Relocation.GetSymbol: TLinkerObjectSymbol;
begin
  Result := FSymbol;
end;

function TCOFF64Relocation.GetVirtualAddress: QWord;
begin
  Result := FVirtualAddress;
end;

procedure TCOFF64Relocation.SetVirtualAddress(AValue: QWord);
begin
  FVirtualAddress := AValue;
end;

function TCOFF64Relocation.GetSection: TLinkerObjectSection;
begin
  Result := FSection;
end;

constructor TCOFF64Relocation.Create(AOwner: TCOFF64Object);
begin
  FOwner := AOwner;
end;

{ TCOFF64Symbol }

function TCOFF64Symbol.GetIsAbsolute: Boolean;
begin
  Result := sfAbsolute in FFlags;
end;

function TCOFF64Symbol.GetIsDebug: Boolean;
begin
  Result := sfDebug in FFlags;
end;

function TCOFF64Symbol.GetIsExternal: Boolean;
begin
  Result := sfExternal in FFlags;
end;

function TCOFF64Symbol.GetIsStatic: Boolean;
begin
  Result := sfStatic in FFlags;
end;

function TCOFF64Symbol.GetIsUndefined: Boolean;
begin
  Result := sfUndefined in FFlags;
end;

function TCOFF64Symbol.GetName: String;
begin
  Result := FName;
end;

function TCOFF64Symbol.GetNumberOfAuxSymbol: DWord;
begin
  Result := FNumberOfAuxSymbol;
end;

function TCOFF64Symbol.GetSection: TLinkerObjectSection;
begin
  Result := FSection;
end;

function TCOFF64Symbol.GetValue: QWord;
begin
  Result := FValue;
end;

procedure TCOFF64Symbol.SetValue(AValue: QWord);
begin
  FValue := AValue;
end;

constructor TCOFF64Symbol.Create(AOwner: TCOFF64Object);
begin
  FOwner := AOwner;
end;

destructor TCOFF64Symbol.Destroy;
begin
  inherited Destroy;
end;

{ TCOFF64Section }

function TCOFF64Section.GetName: String;
begin
  Result := FName;
end;

function TCOFF64Section.GetRelocation(Index: QWord): TLinkerObjectRelocation;
begin
  Result := TCOFF64Relocation(FRelocations[Index]);
end;

function TCOFF64Section.GetRelocationCount: QWord;
begin
  Result := FRelocations.Count;
end;

function TCOFF64Section.GetSymbol(Index: QWord): TLinkerObjectSymbol;
begin
  Result := TCOFF64Symbol(FSymbols[Index]);
end;

function TCOFF64Section.GetSymbolCount: QWord;
begin
  Result := FSymbols.Count;
end;

function TCOFF64Section.GetData: Pbyte;
begin
  Result := FData;
end;

function TCOFF64Section.GetSize: QWord;
begin
  Result := FDataSize;
end;

function TCOFF64Section.GetIsCode: Boolean;
begin
  Result := sfCode in FFlags;
end;

function TCOFF64Section.GetIsDicardable: Boolean;
begin
  Result := sfDiscardable in FFlags;
end;

function TCOFF64Section.GetIsIntializedData: Boolean;
begin
  Result := sfInitializedData in FFlags;
end;

function TCOFF64Section.GetIsUninitalizedData: Boolean;
begin
  Result := sfUninitializedData in FFlags;
end;

function TCOFF64Section.GetIsWritable: Boolean;
begin
  Result := sfWritable in FFlags;
end;

constructor TCOFF64Section.Create(AOwner: TCOFF64Object);
begin
  FOwner := AOwner;
  FSymbols := TObjectList.Create(False);
  FRelocations := TObjectList.Create(True);
end;

destructor TCOFF64Section.Destroy;
begin
  if (FData <> nil) then Freemem(FData);
  FSymbols.Free;
  FRelocations.Free;
  inherited Destroy;
end;

procedure TCOFF64Section.AddSymbol(ASymbol: TCOFF64Symbol);
begin
  FSymbols.Add(ASymbol);
end;

{ TCOFF64Object }

procedure TCOFF64Object.LoadFromFile;
var
  I, J: Integer;
begin
  FStream := TFileStream.Create(FPath, fmOpenRead);
  FStream.Read(FImageFileHeader, IMAGE_SIZEOF_FILE_HEADER);
  if (FImageFileHeader.Machine <> IMAGE_FILE_MACHINE_AMD64) then
    raise Exception.Create('Invalid COFF64 file');
  //Read sections
  for I := 0 to FImageFileHeader.NumberOfSections - 1 do
    AddSection(TCOFF64Section.Create(Self));
  //Read symbols
  FStream.Position := FImageFileHeader.PointerToSymbolTable;
  for I := 0 to FImageFileHeader.NumberOfSymbols - 1 do
    AddSymbol(TCOFF64Symbol.Create(Self));
  //Read relocations
  for I := 0 to FImageFileHeader.NumberOfSections - 1 do
    with TCOFF64Section(FSections[I]) do
      if FInternalSection.NumberOfRelocations > 0 then
      begin
        FStream.Position := FInternalSection.PointerToRelocations;
        for J := 0 to FInternalSection.NumberOfRelocations - 1 do
          AddRelocation(TCOFF64Section(FSections[I]), TCOFF64Relocation.Create(Self));
      end;
end;

procedure TCOFF64Object.AddSection(ASection: TCOFF64Section);
var
  P: Int64;
begin
  //Read section and section data
  with ASection do
  begin
    FStream.Read(FInternalSection, IMAGE_SIZEOF_SECTION_HEADER);
    FName := StrPas(Pchar(@FInternalSection.Name[0]));
    if FName[1] = '/' then
    begin
      if FName[2] = '/' then
        raise Exception.Create('Section with base64 name is not implemented yet');
      P := StrToInt64(Copy(FName, 2, 8));
      FName := GetString(P);
    end;
    FDataSize := FInternalSection.SizeOfRawData;
    FFlags := [];
    if (FInternalSection.Characteristics and IMAGE_SCN_CNT_CODE) <> 0 then
      Include(FFlags, sfCode);
    if (FInternalSection.Characteristics and IMAGE_SCN_MEM_DISCARDABLE) <> 0 then
      Include(FFlags, sfDiscardable);
    if (FInternalSection.Characteristics and IMAGE_SCN_CNT_INITIALIZED_DATA) <> 0 then
      Include(FFlags, sfInitializedData);
    if (FInternalSection.Characteristics and IMAGE_SCN_CNT_UNINITIALIZED_DATA) <> 0 then
      Include(FFlags, sfUninitializedData);
    if (FInternalSection.Characteristics and IMAGE_SCN_MEM_WRITE) <> 0 then
      Include(FFlags, sfWritable);
    if FInternalSection.PointerToRawData <> 0 then
    begin
      FImageBase := min(FInternalSection.PointerToRawData, FImageBase);
      P := FStream.Position;
      FData := AllocMem(FDataSize);
      FStream.Position := FInternalSection.PointerToRawData;
      FStream.Read(Pbyte(FData)^, FDataSize);
      FStream.Position := P;
    end
    else
      FData := nil;
  end;
  FSections.Add(ASection);
end;

procedure TCOFF64Object.AddSymbol(ASymbol: TCOFF64Symbol);
begin
  with ASymbol do
  begin
    //Read symbol
    FStream.Read(FInternalSymbol, IMAGE_SIZEOF_SYMBOL);
    FFlags := [];
    FNumberOfAuxSymbol := FInternalSymbol.NumberOfAuxSymbols;
    FValue := FInternalSymbol.Value;
    if FInternalSymbol.SectionNumber = IMAGE_SYM_UNDEFINED then
      Include(FFlags, sfUndefined)
    else if FInternalSymbol.SectionNumber = IMAGE_SYM_ABSOLUTE then
      Include(FFlags, sfAbsolute)
    else if FInternalSymbol.SectionNumber = IMAGE_SYM_DEBUG then
      Include(FFlags, sfDebug);
    if FInternalSymbol.StorageClass = IMAGE_SYM_CLASS_EXTERNAL then
      Include(FFlags, sfExternal)
    else if FInternalSymbol.StorageClass = IMAGE_SYM_CLASS_STATIC then
      Include(FFlags, sfStatic);

    if FInternalSymbol.SectionNumber <= 0 then
      FSection := nil
    else
    begin
      FSection := Self.Section[FInternalSymbol.SectionNumber - 1] as TCOFF64Section;
      FSection.AddSymbol(ASymbol);
    end;
    if FInternalSymbol.N.Short = 0 then
      FName := GetString(FInternalSymbol.N.Long)
    else
    begin
      SetString(FName, Pchar(@FInternalSymbol.N.ShortName[0]), 9);
      SetLength(FName, StrLen(@FName[1]));
    end;
  end;
  FSymbols.Add(ASymbol);
end;

procedure TCOFF64Object.AddRelocation(ASection: TCOFF64Section; ARelocation: TCOFF64Relocation);
begin
  with ARelocation do
  begin
    FStream.Read(FInternalRelocation, IMAGE_SIZEOF_RELOCATION);
    FSymbol := Self.Symbol[FInternalRelocation.SymbolTableIndex] as TCOFF64Symbol;
    FVirtualAddress := FInternalRelocation.Union.VirtualAddress;
    FFlag := TCOFF64RelocationFlag(FInternalRelocation.Type_);
    FSection := ASection;
    ASection.FRelocations.Add(ARelocation);
  end;
end;

function TCOFF64Object.GetFileName: String;
begin
  Result := ExtractFileName(FPath);
end;

function TCOFF64Object.GetSection(Index: QWord): TLinkerObjectSection;
begin
  if {(Index = 0) or} (Index >= FSections.Count) then
    Result := nil
  else
    Result := TCOFF64Section(FSections[Index {- 1}]);
end;

function TCOFF64Object.GetSectionCount: QWord;
begin
  Result := FSections.Count;
end;

function TCOFF64Object.GetSymbol(Index: QWord): TLinkerObjectSymbol;
begin
  Result := FSymbols[Index] as TLinkerObjectSymbol;
end;

function TCOFF64Object.GetSymbolCount: QWord;
begin
  Result := FSymbols.Count;
end;

function TCOFF64Object.GetImageBase: QWord;
begin
  Result := FImageBase;
end;

constructor TCOFF64Object.Create(APath: String);
begin
  FPath := APath;
  FSections := TObjectList.Create(True);
  FSymbols := TObjectList.Create(True);
  FImageBase := $FFFFFFFFFFFFFFFF;
  LoadFromFile;
end;

destructor TCOFF64Object.Destroy;
begin
  FSections.Free;
  FSymbols.Free;
  inherited Destroy;
end;

function TCOFF64Object.GetString(At: QWord): String;
var
  S: array [0..255] of Ansichar;
  P: Int64;
begin
  P := FStream.Position;
  //Goto string table + String location
  FStream.Position := FImageFileHeader.PointerToSymbolTable + FImageFileHeader.NumberOfSymbols * IMAGE_SIZEOF_SYMBOL + At;
  FStream.Read(S[0], 255);
  Result := StrPas(S);
  FStream.Position := P;
end;

end.
