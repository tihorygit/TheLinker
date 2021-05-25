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
  public
    property Symbol: TLinkerObjectSymbol read GetSymbol;
    property VirtualAddress: QWord read GetVirtualAddress write SetVirtualAddress;
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
  public
    function Find(AQuery: String): TLinkerObjectSectionEnumerator;
    property FileName: String read GetFileName;
    property SectionCount: QWord read GetSectionCount;
    property Section[Index: QWord]: TLinkerObjectSection read GetSection;
    property SectionByName[Name: String]: TLinkerObjectSection read GetSectionByName;
    property Symbol[Index: QWord]: TLinkerObjectSymbol read GetSymbol;
    property SymbolCount: QWord read GetSymbolCount;
  end;

  { TLinkerSection }

  TLinkerSection = class
  private
    FOwner: TLinker;
    FName: String;
    FSize: QWord;
    FCapacity: QWord;
    FObjectSections: TObjectList;

    function GetObjectSection(Index: QWord): TLinkerObjectSection;
    function GetObjectSectionCount: QWord;
  public
    constructor Create(AOwner: TLinker; ACapacity: QWord; AName: String);
    destructor Destroy; override;

    procedure AddObjectSection(AObjectSection: TLinkerObjectSection);
    procedure WriteByte(AValue: Byte);
    procedure WriteWord(AValue: Word);
    procedure WriteDWord(AValue: DWord);
    procedure WriteQWord(AValue: QWord);

    property ObjectSectionCount: QWord read GetObjectSectionCount;
    property ObjectSection[Index: QWord]: TLinkerObjectSection read GetObjectSection;
    property Size: QWord read FSize;

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
    FDLLs: TStringList;

    FObjects: TObjectList;

    FSymbols: TObjectList;
    FSections: TObjectList;
    FCurrentSection: TLinkerSection;

    function GetObject(Index: Integer): TLinkerObject;
    function GetObjectCount: QWord;
    procedure SetCurrentLocation(AValue: QWord);

  public

    constructor Create;
    destructor Destroy; override;

    procedure AddSearchDirectory(APath: String);
    procedure AddInput(APath: String);
    procedure AddGroup(APath: String);
    procedure AddDLL(APath: String);

    function Align(AAlignment: DWord): QWord;

    procedure WriteByte(AValue: Byte);
    procedure WriteWord(AValue: Word);
    procedure WriteDWord(AValue: DWord);
    procedure WriteQWord(AValue: QWord);

    function FindSymbol(AName: String): TLinkerObjectSymbol;
    procedure NewSymbol(AName: String; AValue: QWord);
    procedure AddSymbol(ASymbol: TLinkerObjectSymbol);
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

uses COFF64;

{ TLinkerSymbol }

function TLinkerSymbol.GetIsAbsolute: Boolean;
begin
  Result := True;
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

constructor TLinkerSection.Create(AOwner: TLinker; ACapacity: QWord; AName: String);
begin
  FObjectSections := TObjectList.Create(False);
  FCapacity := ACapacity;
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
  while Size + AObjectSection.Size > FCapacity do
  begin
    FOwner.FMemoryManager.ExpandMemory(1);
    FCapacity += FOwner.FMemoryManager.PageSize;
  end;
  CopyMemory(Pointer(FOwner.CurrentLocation), AObjectSection.Data, AObjectSection.Size);
  FOwner.CurrentLocation := FOwner.CurrentLocation + AObjectSection.Size;
end;

procedure TLinkerSection.WriteByte(AValue: Byte);
begin
  if Size + 1 > FCapacity then
  begin
    FOwner.FMemoryManager.ExpandMemory(1);
    FCapacity += FOwner.FMemoryManager.PageSize;
  end;
  Pbyte(FOwner.CurrentLocation)^ := AValue;
  FOwner.CurrentLocation := FOwner.CurrentLocation + 1;
end;

procedure TLinkerSection.WriteWord(AValue: Word);
begin
  if Size + 2 > FCapacity then
  begin
    FOwner.FMemoryManager.ExpandMemory(1);
    FCapacity += FOwner.FMemoryManager.PageSize;
  end;
  PWord(FOwner.CurrentLocation)^ := AValue;
  FOwner.CurrentLocation := FOwner.CurrentLocation + 2;
end;

procedure TLinkerSection.WriteDWord(AValue: DWord);
begin
  if Size + 4 > FCapacity then
  begin
    FOwner.FMemoryManager.ExpandMemory(1);
    FCapacity += FOwner.FMemoryManager.PageSize;
  end;
  PDWORD(FOwner.CurrentLocation)^ := AValue;
  FOwner.CurrentLocation := FOwner.CurrentLocation + 4;
end;

procedure TLinkerSection.WriteQWord(AValue: QWord);
begin
  if Size + 8 > FCapacity then
  begin
    FOwner.FMemoryManager.ExpandMemory(1);
    FCapacity += FOwner.FMemoryManager.PageSize;
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
  FCurrentIndex := -1;
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
    end
  else
    while FCurrentIndex < FOwner.SectionCount do
    begin
      if (Pos(Q, FOwner.Section[FCurrentIndex].Name) = 1) then
      begin
        Result := True;
        Break;
      end;
    end;
end;

{ TLinker }

procedure TLinker.SetCurrentLocation(AValue: QWord);
begin
  if FCurrentLocation = AValue then Exit;
  FCurrentLocation := AValue;
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
  FSearchDirectories := TStringList.Create;
  FInputs := TStringList.Create;
  FGroups := TStringList.Create;
  FDLLs := TStringList.Create;
  FObjects := TObjectList.Create(True);
  FSymbols := TObjectList.Create(False);
  FSections := TObjectList.Create(True);
end;

destructor TLinker.Destroy;
var
  I: Integer;
begin
  FSearchDirectories.Free;
  FInputs.Free;
  FGroups.Free;
  FDLLs.Free;
  FObjects.Free;
  for I := 0 to FSymbols.Count - 1 do
    if FSymbols[I] is TLinkerSymbol then
      FSymbols[I].Free;
  FSymbols.Free;
  FSections.Free;
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

procedure TLinker.AddDLL(APath: String);
begin
  FDLLs.Add(APath);
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
  for I := 0 to FObjects.Count - 1 do
  begin
    with TLinkerObject(FObjects[I]) do
      for J := 0 to SymbolCount - 1 do
        if Symbol[J].Name = AName then
        begin
          Result := Symbol[J];
          Exit;
        end;
  end;
  for I := 0 to FSymbols.Count - 1 do
  begin
    if TLinkerObjectSymbol(FSymbols[J]).Name = AName then
    begin
      Result := TLinkerObjectSymbol(FSymbols[J]);
      Exit;
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

procedure TLinker.ProvideSymbol(AName: String; AValue: QWord);
begin
  if FindSymbol(AName) <> nil then
    Exit;
  FSymbols.Add(TLinkerSymbol.Create(AName, AValue));
end;

procedure TLinker.NewSection(AName: String);
begin
  //Todo: Check douplicated section name
  FCurrentSection := TLinkerSection.Create(Self, FMemoryManager.Allocated - (FCurrentLocation - QWord(FMemoryManager.Mem)), AName);
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
begin

end;

procedure TLinker.Execute;
begin

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
end;

end.
