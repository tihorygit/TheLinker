unit Linker;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Contnrs, JwaWindows;

type

  TLinkerObject = class;
  TLinkerObjectSection = class;

  TLinkerSection = class;
  TLinker = class;
  TLinkerOutputFormat = (lofPeX86_64);

  { TLinkerMemoryManager }

  TLinkerMemoryManager = class
  private
    FCapacity: QWord;
    FAllocated: QWord;
    FPageSize: DWord;
    FMem: Pointer;
  public
    constructor Create(AReservedPageCount: DWord);
    destructor Destroy; override;
    procedure ExpandMemory(APageCount: DWord);
    property Capacity: QWord read FCapacity;
    property Allocated: QWord read FAllocated;
    property PageSize: DWord read FPageSize;
    property Mem: Pointer read FMem;
  end;

  { TLinkerObjectSymbol }

  TLinkerObjectSymbol = class
  protected
    function GetIsAbsolute: Boolean; virtual; abstract;
    function GetIsDebug: Boolean; virtual; abstract;
    function GetIsExternal: Boolean; virtual; abstract;
    function GetIsStatic: Boolean; virtual; abstract;
    function GetIsUndefined: Boolean; virtual; abstract;
    function GetName: String; virtual; abstract;
    function GetNumberOfAuxSymbol: DWord; virtual; abstract;
    function GetSection: TLinkerObjectSection; virtual; abstract;
    function GetValue: QWord; virtual; abstract;
    procedure SetValue(AValue: QWord); virtual; abstract;
  public
    property Name: String read GetName;
    property Section: TLinkerObjectSection read GetSection;
    property Value: QWord read GetValue write SetValue;
    property NumberOfAuxSymbol: DWord read GetNumberOfAuxSymbol;

    property IsExternal: Boolean read GetIsExternal;
    property IsStatic: Boolean read GetIsStatic;
    property IsUndefined: Boolean read GetIsUndefined;
    property IsAbsolute: Boolean read GetIsAbsolute;
    property IsDebug: Boolean read GetIsDebug;
  end;

  { TLinkerObjectRelocation }

  TLinkerObjectRelocation = class
  protected
    function GetSymbol: TLinkerObjectSymbol; virtual; abstract;
    function GetVirtualAddress: QWord; virtual; abstract;
    procedure SetVirtualAddress(AValue: QWord); virtual; abstract;
    function GetSection: TLinkerObjectSection; virtual; abstract;
  public
    property Symbol: TLinkerObjectSymbol read GetSymbol;
    property VirtualAddress: QWord read GetVirtualAddress write SetVirtualAddress;
    property Section: TLinkerObjectSection read GetSection;
  end;

  { TLinkerObjectSection }

  TLinkerObjectSection = class
  private
    FLinkerSection: TLinkerSection;
    FLinkerSectionData: Pointer;
  protected
    function GetName: String; virtual; abstract;
    function GetRelocation(Index: QWord): TLinkerObjectRelocation; virtual; abstract;
    function GetRelocationCount: QWord; virtual; abstract;
    function GetSymbol(Index: QWord): TLinkerObjectSymbol; virtual; abstract;
    function GetSymbolCount: QWord; virtual; abstract;
    function GetData: Pbyte; virtual; abstract;
    function GetSize: QWord; virtual; abstract;
    function GetIsCode: Boolean; virtual; abstract;
    function GetIsDicardable: Boolean; virtual; abstract;
    function GetIsIntializedData: Boolean; virtual; abstract;
    function GetIsUninitalizedData: Boolean; virtual; abstract;
    function GetIsWritable: Boolean; virtual; abstract;
  public
    property Name: String read GetName;
    property Relocation[Index: QWord]: TLinkerObjectRelocation read GetRelocation;
    property RelocationCount: QWord read GetRelocationCount;
    property Symbol[Index: QWord]: TLinkerObjectSymbol read GetSymbol;
    property SymbolCount: QWord read GetSymbolCount;
    property LinkerSection: TLinkerSection read FLinkerSection write FLinkerSection;
    property LinkerSectionData: Pointer read FLinkerSectionData write FLinkerSectionData;
    property Data: Pbyte read GetData;
    property Size: QWord read GetSize;
    property IsDicardable: Boolean read GetIsDicardable;
    property IsIntializedData: Boolean read GetIsIntializedData;
    property IsUninitalizedData: Boolean read GetIsUninitalizedData;
    property IsCode: Boolean read GetIsCode;
    property IsWritable: Boolean read GetIsWritable;
  end;

  { TLinkerObjectSectionEnumerator }

  TLinkerObjectSectionEnumerator = class
  private
    FCurrentIndex: QWord;
    FOwner: TLinkerObject;
    FQuery: String;
    function GetCurrent: TLinkerObjectSection;
  public
    constructor Create(AOwner: TLinkerObject; AQuery: String);
    function GetEnumerator: TLinkerObjectSectionEnumerator;
    function MoveNext: Boolean;
    property Current: TLinkerObjectSection read GetCurrent;
  end;

  TLinkerObject = class
  private
  protected
    function GetFileName: String; virtual; abstract;
    function GetSection(Index: QWord): TLinkerObjectSection; virtual; abstract;
    function GetSectionByName(Name: String): TLinkerObjectSection; virtual;
    function GetSectionCount: QWord; virtual; abstract;
    function GetSymbol(Index: QWord): TLinkerObjectSymbol; virtual; abstract;
    function GetSymbolCount: QWord; virtual; abstract;
    function GetImageBase: QWord; virtual; abstract;
  public
    function Find(AQuery: String): TLinkerObjectSectionEnumerator;
    property FileName: String read GetFileName;
    property SectionCount: QWord read GetSectionCount;
    property Section[Index: QWord]: TLinkerObjectSection read GetSection;
    property SectionByName[Name: String]: TLinkerObjectSection read GetSectionByName;
    property Symbol[Index: QWord]: TLinkerObjectSymbol read GetSymbol;
    property SymbolCount: QWord read GetSymbolCount;
    property ImageBase: QWord read GetImageBase;
  end;

  { TLinkerSection }

  TLinkerSection = class
  private
    FOwner: TLinker;
    FName: String;
    FObjectSections: TObjectList;
    FStartLocation: QWord;
    function GetObjectSection(Index: QWord): TLinkerObjectSection;
    function GetObjectSectionCount: QWord;
    function GetSize: QWord;
  public
    constructor Create(AOwner: TLinker; AName: String);
    destructor Destroy; override;

    procedure AddObjectSection(AObjectSection: TLinkerObjectSection);
    procedure WriteByte(AValue: Byte);
    procedure WriteWord(AValue: Word);
    procedure WriteDWord(AValue: DWord);
    procedure WriteQWord(AValue: QWord);

    property ObjectSectionCount: QWord read GetObjectSectionCount;
    property ObjectSection[Index: QWord]: TLinkerObjectSection read GetObjectSection;
    property Size: QWord read GetSize;

  end;

  { TLinkerSymbol }

  TLinkerSymbol = class(TLinkerObjectSymbol)
  private
    FValue: QWord;
    FName: String;
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
    constructor Create(AName: String; AValue: QWord);
  end;

  TLinker = class
  private
    FCurrentLocation: QWord;
    FEntryPoint: String;
    FOutputFormat: TLinkerOutputFormat;
    FMemoryManager: TLinkerMemoryManager;

    FSearchDirectories: TStringList;
    FInputs: TStringList;
    FGroups: TStringList;
    FDLLs: TList;

    FObjects: TObjectList;

    FSymbols: TObjectList;
    FSections: TObjectList;
    FCurrentSection: TLinkerSection;

    function GetObject(Index: Integer): TLinkerObject;
    function GetObjectCount: QWord;
    procedure SetCurrentLocation(AValue: QWord);
    procedure ResolveRelocation(AObject: TLinkerObject; ARelocation: TLinkerObjectRelocation);
  public

    constructor Create;
    destructor Destroy; override;

    procedure AddSearchDirectory(APath: String);
    procedure AddInput(APath: String);
    procedure AddGroup(APath: String);
    procedure AddDLL(APath: String; AMangle: String);

    function Align(AAlignment: DWord): QWord;

    procedure WriteByte(AValue: Byte);
    procedure WriteWord(AValue: Word);
    procedure WriteDWord(AValue: DWord);
    procedure WriteQWord(AValue: QWord);

    function FindSymbol(AName: String): TLinkerObjectSymbol;
    procedure NewSymbol(AName: String; AValue: QWord);
    procedure AddSymbol(ASymbol: TLinkerObjectSymbol);
    procedure AddFakeCallSymbol(AName: String; AAddress: Pointer);
    procedure ProvideSymbol(AName: String; AValue: QWord);

    procedure NewSection(AName: String);

    procedure Link(AFileName: String; ASectionName: String);

    procedure MakeExecutable;
    procedure Execute;

    property EntryPoint: String read FEntryPoint write FEntryPoint;
    property OutputFormat: TLinkerOutputFormat read FOutputFormat write FOutputFormat;
    property CurrentLocation: QWord read FCurrentLocation write SetCurrentLocation;

    property ObjectCount: QWord read GetObjectCount;
    property &Object[Index: Integer]: TLinkerObject read GetObject;
  end;

implementation

uses COFF64, DLLParser;

{ TLinkerSymbol }

function TLinkerSymbol.GetIsAbsolute: Boolean;
begin
  Result := False;
end;

function TLinkerSymbol.GetIsDebug: Boolean;
begin
  Result := False;
end;

function TLinkerSymbol.GetIsExternal: Boolean;
begin
  Result := False;
end;

function TLinkerSymbol.GetIsStatic: Boolean;
begin
  Result := True;
end;

function TLinkerSymbol.GetIsUndefined: Boolean;
begin
  Result := False;
end;

function TLinkerSymbol.GetName: String;
begin
  Result := FName;
end;

function TLinkerSymbol.GetNumberOfAuxSymbol: DWord;
begin
  Result := 0;
end;

function TLinkerSymbol.GetSection: TLinkerObjectSection;
begin
  Result := nil;
end;

function TLinkerSymbol.GetValue: QWord;
begin
  Result := FValue;
end;

procedure TLinkerSymbol.SetValue(AValue: QWord);
begin
  FValue := AValue;
end;

constructor TLinkerSymbol.Create(AName: String; AValue: QWord);
begin
  FName := AName;
  FValue := AValue;
end;

{ TLinkerSection }

function TLinkerSection.GetObjectSection(Index: QWord): TLinkerObjectSection;
begin
  Result := FObjectSections[Index] as TLinkerObjectSection;
end;

function TLinkerSection.GetObjectSectionCount: QWord;
begin
  Result := FObjectSections.Count;
end;

function TLinkerSection.GetSize: QWord;
begin
  Result := FOwner.CurrentLocation - FStartLocation;
end;

constructor TLinkerSection.Create(AOwner: TLinker; AName: String);
begin
  FOwner := AOwner;
  FObjectSections := TObjectList.Create(False);
  FStartLocation := AOwner.CurrentLocation;
  FName := AName;
end;

destructor TLinkerSection.Destroy;
begin
  FObjectSections.Free;
  inherited Destroy;
end;

procedure TLinkerSection.AddObjectSection(AObjectSection: TLinkerObjectSection);
begin
  FObjectSections.Add(AObjectSection);
  AObjectSection.LinkerSectionData := Pointer(FOwner.CurrentLocation);
  AObjectSection.LinkerSection := Self;
  while AObjectSection.Size > QWord(FOwner.FMemoryManager.Mem) - FOwner.CurrentLocation + FOwner.FMemoryManager.Allocated do
  begin
    FOwner.FMemoryManager.ExpandMemory(1);
  end;
  if AObjectSection.Data <> nil then
    CopyMemory(Pointer(FOwner.CurrentLocation), AObjectSection.Data, AObjectSection.Size);
  FOwner.CurrentLocation := FOwner.CurrentLocation + AObjectSection.Size;
end;

procedure TLinkerSection.WriteByte(AValue: Byte);
begin
  if 1 > QWord(FOwner.FMemoryManager.Mem) - FOwner.CurrentLocation + FOwner.FMemoryManager.Allocated then
  begin
    FOwner.FMemoryManager.ExpandMemory(1);
  end;
  Pbyte(FOwner.CurrentLocation)^ := AValue;
  FOwner.CurrentLocation := FOwner.CurrentLocation + 1;
end;

procedure TLinkerSection.WriteWord(AValue: Word);
begin
  if 2 > QWord(FOwner.FMemoryManager.Mem) - FOwner.CurrentLocation + FOwner.FMemoryManager.Allocated then
  begin
    FOwner.FMemoryManager.ExpandMemory(1);
  end;
  PWord(FOwner.CurrentLocation)^ := AValue;
  FOwner.CurrentLocation := FOwner.CurrentLocation + 2;
end;

procedure TLinkerSection.WriteDWord(AValue: DWord);
begin
  if 4 > QWord(FOwner.FMemoryManager.Mem) - FOwner.CurrentLocation + FOwner.FMemoryManager.Allocated then
  begin
    FOwner.FMemoryManager.ExpandMemory(1);
  end;
  PDWORD(FOwner.CurrentLocation)^ := AValue;
  FOwner.CurrentLocation := FOwner.CurrentLocation + 4;
end;

procedure TLinkerSection.WriteQWord(AValue: QWord);
begin
  if 8 > QWord(FOwner.FMemoryManager.Mem) - FOwner.CurrentLocation + FOwner.FMemoryManager.Allocated then
  begin
    FOwner.FMemoryManager.ExpandMemory(1);
  end;
  PQWord(FOwner.CurrentLocation)^ := AValue;
  FOwner.CurrentLocation := FOwner.CurrentLocation + 8;
end;

{ TLinkerObject }

function TLinkerObject.GetSectionByName(Name: String): TLinkerObjectSection;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to SectionCount - 1 do
  begin
    if (Section[I].Name = Name) then
    begin
      Result := Section[I];
      Exit;
    end;
  end;
end;

function TLinkerObject.Find(AQuery: String): TLinkerObjectSectionEnumerator;
begin
  Result := TLinkerObjectSectionEnumerator.Create(Self, AQuery);
end;

{ TLinkerObjectSectionEnumerator }

function TLinkerObjectSectionEnumerator.GetCurrent: TLinkerObjectSection;
begin
  Result := FOwner.Section[FCurrentIndex];
end;

constructor TLinkerObjectSectionEnumerator.Create(AOwner: TLinkerObject; AQuery: String);
begin
  FOwner := AOwner;
  FQuery := AQuery;
  FCurrentIndex := QWord(-1);
end;

function TLinkerObjectSectionEnumerator.GetEnumerator: TLinkerObjectSectionEnumerator;
begin
  Result := Self;
end;

function TLinkerObjectSectionEnumerator.MoveNext: Boolean;
var
  Q: String;
  Exact: Boolean;
begin
  Q := FQuery;
  Exact := True;
  if (Q[Length(Q)] = '*') then
  begin
    System.Delete(Q, Length(Q), 1);
    Exact := False;
  end;
  Result := False;
  Inc(FCurrentIndex);
  if (Exact) then
    while FCurrentIndex < FOwner.SectionCount do
    begin
      if (FOwner.Section[FCurrentIndex].Name = Q) then
      begin
        Result := True;
        Break;
      end;
      Inc(FCurrentIndex);
    end
  else
    while FCurrentIndex < FOwner.SectionCount do
    begin
      if (Pos(Q, FOwner.Section[FCurrentIndex].Name) = 1) then
      begin
        Result := True;
        Break;
      end;
      Inc(FCurrentIndex);
    end;
end;

{ TLinker }

procedure TLinker.SetCurrentLocation(AValue: QWord);
begin
  if FCurrentLocation = AValue then Exit;
  FCurrentLocation := AValue;
end;

procedure TLinker.ResolveRelocation(AObject: TLinkerObject; ARelocation: TLinkerObjectRelocation);
var
  RS: TLinkerObjectSection;
  Sym: TLinkerObjectSymbol;
  R: TCOFF64Relocation;
  RelAddr, SymAddr: QWord;
begin
  R := ARelocation as TCOFF64Relocation;
  RS := ARelocation.Section;
  if RS.IsDicardable then
    Exit;
  Sym := R.Symbol;
  if Sym.IsUndefined then
    Sym := FindSymbol(Sym.Name);
  if Sym = nil then
  begin
    raise Exception.Create('Undifined symbol : ' + R.Symbol.Name);
    Exit;
  end;
  if Sym.IsAbsolute then
    Exit; //Not supported yet
  if Sym.IsDebug then
    Exit; //Not supported yet

  RelAddr := QWord(R.Section.LinkerSectionData) + R.VirtualAddress;
  if Sym.Section <> nil then
    SymAddr := Sym.Value + QWord(Sym.Section.LinkerSectionData)
  else
    SymAddr := Sym.Value; // Fake Symbol

  case R.Flag of
    rfAbsolute: ;
    rfAddr64: PQWord(RelAddr)^ := QWord(SymAddr);
    rfAddr32NB: PDWORD(RelAddr)^ := DWORD(SymAddr - AObject.ImageBase - QWord(FMemoryManager.Mem));
    rfRel32: PDWORD(RelAddr)^ := DWORD(SymAddr - RelAddr - 4);
    rfRel32_1: PDWORD(RelAddr)^ := DWORD(SymAddr - RelAddr - 5);
    rfRel32_2: PDWORD(RelAddr)^ := DWORD(SymAddr - RelAddr - 6);
    rfRel32_3: PDWORD(RelAddr)^ := DWORD(SymAddr - RelAddr - 7);
    rfRel32_4: PDWORD(RelAddr)^ := DWORD(SymAddr - RelAddr - 8);
    rfRel32_5: PDWORD(RelAddr)^ := DWORD(SymAddr - RelAddr - 9);
    else
      raise Exception.Create('Not supported relocation');
  end;

end;

function TLinker.GetObject(Index: Integer): TLinkerObject;
begin
  Result := FObjects[Index] as TLinkerObject;
end;

function TLinker.GetObjectCount: QWord;
begin
  Result := FObjects.Count;
end;

constructor TLinker.Create;
begin
  FMemoryManager := TLinkerMemoryManager.Create(10000);
  FMemoryManager.ExpandMemory(1);
  FSearchDirectories := TStringList.Create;
  FInputs := TStringList.Create;
  FGroups := TStringList.Create;
  FDLLs := TList.Create;
  FObjects := TObjectList.Create(True);
  FSymbols := TObjectList.Create(False);
  FSections := TObjectList.Create(True);
  FCurrentLocation := QWord(FMemoryManager.Mem);
end;

destructor TLinker.Destroy;
var
  I: Integer;
begin
  FSearchDirectories.Free;
  FInputs.Free;
  FGroups.Free;
  for I := 0 to FDLLs.Count - 1 do
    UnloadLibrary(QWord(FDLLs[I]));
  FDLLs.Free;
  FObjects.Free;
  for I := 0 to FSymbols.Count - 1 do
    if FSymbols[I] is TLinkerSymbol then
      FSymbols[I].Free;
  FSymbols.Free;
  FSections.Free;
  FMemoryManager.Free;
  inherited Destroy;
end;

procedure TLinker.AddSearchDirectory(APath: String);
begin
  FSearchDirectories.Add(APath);
end;

procedure TLinker.AddInput(APath: String);
begin
  FInputs.Add(APath);
  FObjects.Add(TCOFF64Object.Create(APath));
end;

procedure TLinker.AddGroup(APath: String);
begin
  FGroups.Add(APath);
end;

procedure TLinker.AddDLL(APath: String; AMangle: String);
var
  SL: TStringList;
  Lib: TLibHandle;
  P: Pointer;
  I: Integer;
begin
  //FDLLs.Add(APath);
  SL := TStringList.Create;
  ExtractDLLFunctions(APath, SL);
  Lib := System.LoadLibrary(APath);
  if Lib = 0 then
    raise Exception.Create('Can''t load dll');
  FDLLs.Add(Pointer(Lib));
  for I := 0 to SL.Count - 1 do
  begin
    P := System.GetProcAddress(Lib, SL[I]);
    AddFakeCallSymbol(AMangle + SL[I], P);
  end;
end;

function TLinker.Align(AAlignment: DWord): QWord;
var
  Diff: QWord;
begin
  Diff := FCurrentLocation mod AAlignment;
  if Diff <> 0 then
    Result := FCurrentLocation - (FCurrentLocation mod AAlignment) + AAlignment
  else
    Result := FCurrentLocation;
end;

procedure TLinker.WriteByte(AValue: Byte);
begin
  FCurrentSection.WriteByte(AValue);
end;

procedure TLinker.WriteWord(AValue: Word);
begin
  FCurrentSection.WriteWord(AValue);
end;

procedure TLinker.WriteDWord(AValue: DWord);
begin
  FCurrentSection.WriteDWord(AValue);
end;

procedure TLinker.WriteQWord(AValue: QWord);
begin
  FCurrentSection.WriteQWord(AValue);
end;

function TLinker.FindSymbol(AName: String): TLinkerObjectSymbol;
var
  I, J: Integer;
begin
  Result := nil;
  for I := 0 to FSymbols.Count - 1 do
  begin
    if TLinkerObjectSymbol(FSymbols[I]).Name = AName then
    begin
      Result := TLinkerObjectSymbol(FSymbols[I]);
      Exit;
    end;
  end;
  for I := 0 to FObjects.Count - 1 do
  begin
    with TLinkerObject(FObjects[I]) do
      for J := 0 to SymbolCount - 1 do
      begin
        if Symbol[J].IsUndefined then Continue;
        if Symbol[J].Name = AName then
        begin
          Result := Symbol[J];
          Exit;
        end;
      end;
  end;
end;

procedure TLinker.NewSymbol(AName: String; AValue: QWord);
begin
  AddSymbol(TLinkerSymbol.Create(AName, AValue));
end;

procedure TLinker.AddSymbol(ASymbol: TLinkerObjectSymbol);
begin
  if FindSymbol(ASymbol.Name) <> nil then
    raise Exception.Create('Duplicate Symbol : ' + ASymbol.Name);
  FSymbols.Add(ASymbol);
end;

procedure TLinker.AddFakeCallSymbol(AName: String; AAddress: Pointer);
var
  Mem: Pbyte;
begin
  if FCurrentLocation - QWord(FMemoryManager.Mem) + 12 > FMemoryManager.Allocated then
    FMemoryManager.ExpandMemory(1);
  Mem := Pbyte(FCurrentLocation);
  Mem[0] := 72;
  Mem[1] := 184;
  PQWord(@Mem[2])^ := QWord(AAddress);
  Mem[10] := 255;
  Mem[11] := 224;
  FCurrentLocation += 12;
  FSymbols.Add(TLinkerSymbol.Create(AName, QWord(Mem)));
end;

procedure TLinker.ProvideSymbol(AName: String; AValue: QWord);
begin
  if FindSymbol(AName) <> nil then
    Exit;
  FSymbols.Add(TLinkerSymbol.Create(AName, AValue));
end;

procedure TLinker.NewSection(AName: String);
begin
  //Todo: Check duplicated section name
  FCurrentSection := TLinkerSection.Create(Self, AName);
  FSections.Add(FCurrentSection);
end;

procedure TLinker.Link(AFileName: String; ASectionName: String);
var
  I: Integer;
  S: TLinkerObjectSection;
begin
  for I := 0 to FObjects.Count - 1 do
    with TLinkerObject(FObjects[I]) do
      if FileName = AFileName then
      begin
        S := SectionByName[ASectionName];
        if S <> nil then
          FCurrentSection.AddObjectSection(S);
      end;
end;

procedure TLinker.MakeExecutable;
var
  I, J, K: Integer;
  O: TLinkerObject;
begin
  for I := 0 to FObjects.Count - 1 do
  begin
    O := TLinkerObject(FObjects[I]);
    with O do
      for J := 0 to SectionCount - 1 do
        with Section[J] do
          for K := 0 to RelocationCount - 1 do
            ResolveRelocation(O, Relocation[K]);
  end;
end;

procedure TLinker.Execute;
type
  TMainProc = procedure;
var
  Sym: TLinkerObjectSymbol;
  Main: TMainProc;
begin
  Sym := FindSymbol(EntryPoint);
  if Sym = nil then
    raise Exception.Create('Can''t find main procedure');
  Main := TMainProc(Sym.Value + Sym.Section.FLinkerSectionData);
  Main();
end;

{ TLinkerMemoryManager }

constructor TLinkerMemoryManager.Create(AReservedPageCount: DWord);
var
  info: TSYSTEMINFO;
begin
  GetSystemInfo(info);
  FPageSize := info.dwPageSize;
  FCapacity := FPageSize * AReservedPageCount;
  FAllocated := 0;
  FMem := VirtualAlloc(nil, FCapacity, MEM_RESERVE, PAGE_EXECUTE_READWRITE);
  if (FMem = nil) then
    raise Exception.Create('Memory reservation failed.');
end;

destructor TLinkerMemoryManager.Destroy;
begin
  if (FMem <> nil) then
    VirtualFree(FMem, 0, MEM_RELEASE);
  inherited Destroy;
end;

procedure TLinkerMemoryManager.ExpandMemory(APageCount: DWord);
var
  o: LPVOID;
begin
  o := VirtualAlloc(FMem + FAllocated, APageCount * FPageSize, MEM_COMMIT, PAGE_EXECUTE_READWRITE);
  if (o = nil) then
    raise Exception.Create('Memory allocation failed.');
  FAllocated += FPageSize;
end;

end.
