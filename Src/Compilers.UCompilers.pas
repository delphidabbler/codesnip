
{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2005-2021, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Provides a class that maintains a list of all supported compilers and creates
 * a global singleton instance of the list. Also provides a class that can
 * detect some compilers that are registered on the local machine and another
 * that can persist the compilers list to the application's storage.
}


unit Compilers.UCompilers;


interface


uses
  // Project
  Compilers.UGlobals, UBaseObjects;


type

  {
  TPersistCompilers:
    Class that can save and load a ICompilers object to and from persistent
    storage.
  }
  TPersistCompilers = class(TInterfacedObject,
    IPersistCompilers
  )
  public
    { IPersistCompilers methods }
    procedure Save(const Compilers: ICompilers);
      {Saves a list of compilers to storage.
        @param Compilers [in] List of compilers to save.
      }
    procedure Load(const Compilers: ICompilers);
      {Loads a list of compilers from persistent storage.
        @param Compilers [in] List of compilers to load.
      }
  end;

  {
  TCompilersFactory:
    Factory class that create ICompilers instances.
  }
  TCompilersFactory = class(TNoConstructObject)
  public
    class function CreateCompilers: ICompilers;
      {Creates a new Compilers object without loading configuration from
      persistent storage.
        @return Required object instance.
      }
    class function CreateAndLoadCompilers: ICompilers;
      {Creates a new Compilers object, loading configuration from persistent
      storage.
        @return Required object instance.
      }
  end;


implementation


uses
  // Delphi
  Generics.Collections, SysUtils,
  // Project
  Compilers.UBDS, Compilers.UDelphi, Compilers.UFreePascal,
  Compilers.USearchDirs, IntfCommon, UConsts, UExceptions, UIStringList,
  USettings;


type

  {
  TCompilerFactory:
    Factory class that creates ICompiler instances for a supported compiler.
  }
  TCompilerFactory = class(TNoConstructObject)
  public
    class function CreateCompiler(const CompID: TCompilerID): ICompiler;
      {Creates a compiler object for a supported compiler.
        @param CompID [in] Id of required compiler.
        @return Instance of object representing compiler.
        @except EBug raised if CompID is not recognised.
      }
  end;

  {
  TCompilers:
    Implements a list of objects representing all supported compilers. The class
    supports assignment.
  }
  TCompilers = class(TInterfacedObject,
    ICompilers, IAssignable
  )
  strict private
    var
      fCompilers: TList<ICompiler>; // List of compiler objects
  public
    constructor Create;
      {Object constructor. Creates object containing compiler instances for all
      supported compilers.
      }
    destructor Destroy; override;
      {Object destructor. Tears down object.
      }
    { IAssignable method }
    procedure Assign(const Src: IInterface);
      {Assigns properties of a given object to this object.
        @param Src [in] Object whose properties are to be copied.
        @except EBug raised if Src is incompatible with this object.
      }
    { ICompilers methods }
    function GetCompiler(CompID: TCompilerID): ICompiler;
      {Read accessor for Compilers property. Returns compiler object with
      specified id.
        @param CompID [in] Id of required compiler.
        @return Selected compiler object.
      }
    function GetCount: Integer;
      {Read access method for Count property.
        @return Number of compilers in list.
      }
    function GetAvailableCount: Integer;
      {Read access method for AvailableCount property
        @return Number of installed compilers available to program.
      }
    function HaveDisplayable: Boolean;
      {Checks if any compilers are displayable.
        @return True if at least one compiler is displayable, False otherwise.
      }
    function GetEnumerator: TEnumerator<ICompiler>;
      {Creates an enumerator for this object.
        @return Reference to new enumerator.
      }
  end;

{ TCompilerFactory }

class function TCompilerFactory.CreateCompiler(
  const CompID: TCompilerID): ICompiler;
  {Creates a compiler object for a supported compiler.
    @param CompID [in] Id of required compiler.
    @return Instance of object representing compiler.
    @except EBug raised if CompID is not recognised.
  }
begin
  if CompID in cClassicDelphiCompilers then
    Result := TDelphiCompiler.Create(CompID)
  else if CompID in cBDSCompilers then
    Result := TBDSCompiler.Create(CompID)
  else if CompID in cFreePascalCompilers then
    Result := TFreePascalCompiler.Create
  else
    raise EBug.Create(ClassName + '.CreateCompiler: CompID not known');
end;

{ TCompilersFactory }

class function TCompilersFactory.CreateAndLoadCompilers: ICompilers;
  {Creates a new Compilers object, loading configuration from persistent
  storage.
    @return Required object instance.
  }
var
  Persister: IPersistCompilers; // object used to read details from storage
begin
  // Haven't created object: do it and read from persistent storage
  Result := CreateCompilers;
  Persister := TPersistCompilers.Create;
  Persister.Load(Result);
end;

class function TCompilersFactory.CreateCompilers: ICompilers;
  {Creates a new Compilers object without loading configuration from persistent
  storage.
    @return Required object instance.
  }
begin
  Result := TCompilers.Create;
end;

{ TCompilers }

procedure TCompilers.Assign(const Src: IInterface);
  {Assigns properties of a given object to this object.
    @param Src [in] Object whose properties are to be copied.
    @except EBug raised if Src is incompatible with this object.
  }
var
  SrcCompilers: ICompilers; // ICompilers interface to object being assigned
  SrcCompiler: ICompiler;   // references each compiler being assigned
begin
  // Get ICompilers interface of given object: raise exception or error
  if not Supports(Src, ICompilers, SrcCompilers) then
    raise EBug.Create(ClassName + '.Assign: Src is wrong type');
  // Make a copy (clone) of each compiler in list
  fCompilers.Clear;
  for SrcCompiler in SrcCompilers do
    fCompilers.Add((SrcCompiler as IClonable).Clone as ICompiler);
end;

constructor TCompilers.Create;
  {Object constructor. Creates object containing compiler instances for all
  supported compilers.
  }
var
  CompID: TCompilerID;  // loops thru all supported compilers
begin
  inherited;
  // Create list to store compilers and create and store each compiler in it
  fCompilers := TList<ICompiler>.Create;
  for CompID := Low(TCompilerID) to High(TCompilerID) do
    fCompilers.Add(TCompilerFactory.CreateCompiler(CompID));
end;

destructor TCompilers.Destroy;
  {Object destructor. Tears down object.
  }
begin
  fCompilers.Free;
  inherited;
end;

function TCompilers.GetAvailableCount: Integer;
  {Read access method for AvailableCount property
    @return Number of installed compilers available to program.
  }
var
  Compiler: ICompiler;  // loops thru all compilers
begin
  Result := 0;
  for Compiler in fCompilers do
    if Compiler.IsAvailable then
      Inc(Result);
end;

function TCompilers.GetCompiler(CompID: TCompilerID): ICompiler;
  {Read accessor for Compilers property. Returns compiler object with
  specified id.
    @param CompID [in] Id of required compiler.
    @return Selected compiler object.
  }
begin
  Result := fCompilers[Ord(CompID)];
end;

function TCompilers.GetCount: Integer;
  {Read access method for Count property.
    @return Number of compilers in list.
  }
begin
  Result := fCompilers.Count;
end;

function TCompilers.GetEnumerator: TEnumerator<ICompiler>;
  {Creates an enumerator for this object.
    @return Reference to new enumerator.
  }
begin
  Result := fCompilers.GetEnumerator;
end;

function TCompilers.HaveDisplayable: Boolean;
var
  Compiler: ICompiler;  // each compiler
begin
  for Compiler in fCompilers do
    if Compiler.GetDisplayable then
      Exit(True);
  Result := False;
end;

{ TPersistCompilers }

procedure TPersistCompilers.Load(const Compilers: ICompilers);
  {Loads a list of compilers from persistent storage.
    @param Compilers [in] List of compilers to load.
  }
var
  Compiler: ICompiler;                  // refers to each compiler
  Prefixes: TCompLogPrefixes;           // compiler log prefixes from storage
  PrefixID: TCompLogPrefixID;           // loops thru all compiler log prefixes
  Storage: ISettingsSection;            // accesses persistent storage
  ExePath: string;                      // value of ExePath in storage file
  SearchDirNames: IStringList;          // list of search directory names
begin
  // Loop thru each supported compiler
  for Compiler in Compilers do
  begin
    // Load values for compiler from persistent storage
    Storage := Settings.ReadSection(
      ssCompilerInfo, Compiler.GetIDString
    );

    // Get compiler path (if any) and store in compiler object if so
    ExePath := Storage.GetString('ExePath');
    if ExePath <> '' then
      Compiler.SetExecFile(ExePath);

    // Get compiler visibility in UI
    Compiler.SetDisplayable(Storage.GetBoolean('Displayable', True));

    // Load compiler log prefixes (format PrefixX)
    for PrefixID := Low(TCompLogPrefixID) to High(TCompLogPrefixID) do
      Prefixes[PrefixID] := Storage.GetString(
        Format('Prefix%d', [Ord(PrefixID)])
      );
    Compiler.SetLogFilePrefixes(Prefixes);

    // Load command line switches (empty entry => use default)
    if Storage.ItemExists('Switches') then
      Compiler.SetSwitches(Storage.GetString('Switches'));

    // Load namespaces to search for RTL units, if required
    // (empty entry => use default)
    if Compiler.RequiresRTLNamespaces and Storage.ItemExists('Namespaces') then
      Compiler.SetRTLNamespaces(Storage.GetString('Namespaces'));

    // Load search directories
    SearchDirNames := Storage.GetStrings('SearchDirCount', 'SearchDir%d');
    Compiler.SetSearchDirs(TSearchDirs.Create(SearchDirNames.ToArray));
  end;
end;

procedure TPersistCompilers.Save(const Compilers: ICompilers);
  {Saves a list of compilers to storage.
    @param Compilers [in] List of compilers to save.
  }
var
  Compiler: ICompiler;          // refers to each compiler
  Prefixes: TCompLogPrefixes;   // compiler log prefixes from storage
  PrefixID: TCompLogPrefixID;   // loops thru all compiler log prefixes
  Storage: ISettingsSection;    // object used to access persistent storage
  SearchDirNames: IStringList;  // list of search directory names
begin
  for Compiler in Compilers do
  begin
    // Store required values in persistent storage
    // get new empty storage object
    Storage := Settings.EmptySection(ssCompilerInfo, Compiler.GetIDString);
    // add required data to storage object
    Storage.SetString('ExePath', Compiler.GetExecFile);
    Storage.SetBoolean('Displayable', Compiler.GetDisplayable);
    Prefixes := Compiler.GetLogFilePrefixes;
    for PrefixID := Low(TCompLogPrefixID) to High(TCompLogPrefixID) do
      Storage.SetString(
        Format('Prefix%d', [Ord(PrefixID)]),
        DOUBLEQUOTE + Prefixes[PrefixID] + DOUBLEQUOTE
      );
    if Compiler.GetSwitches <> Compiler.GetDefaultSwitches then
      Storage.SetString('Switches', Compiler.GetSwitches);
    if Compiler.RequiresRTLNamespaces
      and (Compiler.GetRTLNamespaces <> Compiler.GetDefaultRTLNamespaces) then
      Storage.SetString('Namespaces', Compiler.GetRTLNamespaces);
    SearchDirNames := TIStringList.Create(Compiler.GetSearchDirs.ToStrings);
    Storage.SetStrings('SearchDirCount', 'SearchDir%d', SearchDirNames);
    // save the data
    Storage.Save;
  end;
end;

end.

