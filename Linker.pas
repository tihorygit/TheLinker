unit Linker;

{$mode objfpc}{$H+}

interface

uses
  JwaWindows, Classes, SysUtils;

type

  TLinkerObject = class;
  TLinkerObjectSection = class;

  TLinkerSection = class;

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
    FSize: QWord;
    function GetObjectSection(Index: QWord): TLinkerObjectSection;
    function GetObjectSectionCount: QWord;
  public
    procedure AddObjectSection(AObjectSection: TLinkerObjectSection);

    property ObjectSectionCount: QWord read GetObjectSectionCount;
    property ObjectSection[Index: QWord]: TLinkerObjectSection read GetObjectSection;
    property Size: QWord read FSize;
  end;

  TLinker = class
  private
    FCurrentLocation: QWord;
    FEntryPoint: String;
    FOutputFormat: TLinkerOutputFormat;
    FMemoryManager: TLinkerMemoryManager;

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

    procedure NewSymbol(AName: String; AValue: QWord);
    procedure ProvideSymbol(AName: String; AValue: QWord);

    procedure NewSection(AName: String);

    procedure Link(AFileName: String; ASectionName: String);

    procedure MakeExecutable;
    procedure Execute;

    property EntryPoint: String read FEntryPoint write FEntryPoint;
    property OutputFormat: TLinkerOutputFormat read FOutputFormat write FOutputFormat;
    property CurrentLocation: QWord read FCurrentLocation write SetCurrentLocation;
  end;

implementation

{ TLinkerSection }

function TLinkerSection.GetObjectSection(Index: QWord): TLinkerObjectSection;
begin

end;

function TLinkerSection.GetObjectSectionCount: QWord;
begin

end;

procedure TLinkerSection.AddObjectSection(AObjectSection: TLinkerObjectSection);
begin

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

constructor TLinker.Create;
begin

end;

destructor TLinker.Destroy;
begin
  inherited Destroy;
end;

procedure TLinker.AddSearchDirectory(APath: String);
begin

end;

procedure TLinker.AddInput(APath: String);
begin

end;

procedure TLinker.AddGroup(APath: String);
begin

end;

procedure TLinker.AddDLL(APath: String);
begin

end;

function TLinker.Align(AAlignment: DWord): QWord;
begin

end;

procedure TLinker.WriteByte(AValue: Byte);
begin

end;

procedure TLinker.WriteWord(AValue: Word);
begin

end;

procedure TLinker.WriteDWord(AValue: DWord);
begin

end;

procedure TLinker.WriteQWord(AValue: QWord);
begin

end;

procedure TLinker.NewSymbol(AName: String; AValue: QWord);
begin

end;

procedure TLinker.ProvideSymbol(AName: String; AValue: QWord);
begin

end;

procedure TLinker.NewSection(AName: String);
begin

end;

procedure TLinker.Link(AFileName: String; ASectionName: String);
begin

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
