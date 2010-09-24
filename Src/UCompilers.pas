{
 * UCompilers.pas
 *
 * Provides a class that maintains a list of all supported compilers and creates
 * a global singleton instance of the list. Also provides a class that can
 * detect some compilers that are registered on the local machine and another
 * that can persist the compilers list to the application's storage.
 *
 * $Rev$
 * $Date$
 *
 * ***** BEGIN LICENSE BLOCK *****
 *
 * Version: MPL 1.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with the
 * License. You may obtain a copy of the License at http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
 * the specific language governing rights and limitations under the License.
 *
 * The Original Code is UCompilers.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2005-2009 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
}


unit UCompilers;


interface


uses
  // Project
  IntfCompilers, UBaseObjects;


type

  {
  TPersistCompilers:
    Class that can save and load a ICompilers object to and from persistent
    storage.
  }
  TPersistCompilers = class(TInterfacedObject,
    IPersistCompilers
  )
  protected
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
  SysUtils, Classes,
  // Project
  IntfCommon, UBDSCompiler, UExceptions, UDelphiCompiler,
  UFreePascalCompiler, USettings;


type

  {
  TCompilerFactory:
    Factory class that create ICompiler instances for a supported compiler.
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
    supprts cloning and assignment.
  }
  TCompilers = class(TInterfacedObject,
    ICompilers, IClonable, IAssignable
  )
  strict private
    var
      fCompilers: TInterfaceList;
        {List of compiler objects}
    type
      {
      TEnumerator:
        Implements enumerator for ICompilers object.
      }
      TEnumerator = class(TInterfacedObject,
        ICompilersEnum
      )
      strict private
        fCompilers: TInterfaceList;
          {List of compilers to be enumerated}
        fIndex: Integer;
          {Index of current item in enumeration}
      public
        constructor Create(const Compilers: TInterfaceList);
          {Class constructor. Initialises enumeration.
            @param Compilers [in] List of compilers to be enumerated.
          }
        { ICompilersEnum methods }
        function GetCurrent: ICompiler;
          {Gets reference to current compiler.
            @return Reference to compiler.
          }
        function MoveNext: Boolean;
          {Moves to next item in enumeration.
            @return True if there is a next item, False if beyond last item.
          }
        property Current: ICompiler
          read GetCurrent;
          {Reference to current compiler}
      end;
  protected // do not make strict
    { IAssignable method }
    procedure Assign(const Src: IInterface);
      {Assigns properties of a given object to this object.
        @param Src [in] Object whose properties are to be copied.
        @except EBug raised if Src is incompatible with this object.
      }
    { IClonable method }
    function Clone: IInterface;
      {Creates a new instance of the object that is an extact copy and returns
      it.
        @return Cloned object.
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
    function GetEnumerator: ICompilersEnum;
      {Creates an enumerator for this object.
        @return Reference to new enumerator.
      }
  public
    constructor Create;
      {Class constructor. Creates object containing compiler instances for all
      supported compilers.
      }
    destructor Destroy; override;
      {Class destructor. Tears down object.
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
    fCompilers.Add((SrcCompiler as IClonable).Clone);
end;

function TCompilers.Clone: IInterface;
  {Creates a new instance of the object that is an extact copy and returns it.
    @return Cloned object.
  }
var
  Obj: TCompilers;      // cloned object
  Compiler: ICompiler;  // loops thru all compilers
begin
  // Create cloned object
  Obj := TCompilers.Create;
  // Clear any previous entries
  Obj.fCompilers.Clear;
  // Store a cloned copies of all compilers in list
  for Compiler in (Self as ICompilers) do
    Obj.fCompilers.Add((Compiler as IClonable).Clone);
  Result := Obj;
end;

constructor TCompilers.Create;
  {Class constructor. Creates object containing compiler instances for all
  supported compilers.
  }
var
  CompID: TCompilerID;  // loops thru all supported compilers
begin
  inherited;
  // Create list to store compilers and create and store each compiler in it
  fCompilers := TInterfaceList.Create;
  for CompID := Low(TCompilerID) to High(TCompilerID) do
    fCompilers.Add(TCompilerFactory.CreateCompiler(CompID));
end;

destructor TCompilers.Destroy;
  {Class destructor. Tears down object.
  }
begin
  FreeAndNil(fCompilers);
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
  for Compiler in (Self as ICompilers) do
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
  Result := fCompilers[Ord(CompID)] as ICompiler;
end;

function TCompilers.GetCount: Integer;
  {Read access method for Count property.
    @return Number of compilers in list.
  }
begin
  Result := fCompilers.Count;
end;

function TCompilers.GetEnumerator: ICompilersEnum;
  {Creates an enumerator for this object.
    @return Reference to new enumerator.
  }
begin
  Result := TEnumerator.Create(fCompilers);
end;

constructor TCompilers.TEnumerator.Create(const Compilers: TInterfaceList);
  {Class constructor. Initialises enumeration.
    @param Compilers [in] List of compilers to be enumerated.
  }
begin
  inherited Create;
  fCompilers := Compilers;
  fIndex := -1;
end;

function TCompilers.TEnumerator.GetCurrent: ICompiler;
  {Gets reference to current compiler.
    @return Reference to compiler.
  }
begin
  Result := fCompilers[fIndex] as ICompiler;
end;

function TCompilers.TEnumerator.MoveNext: Boolean;
  {Moves to next item in enumeration.
    @return True if there is a next item, False if beyond last item.
  }
begin
  Result := fIndex < Pred(fCompilers.Count);
  if Result then
    Inc(fIndex);
end;

{ TPersistCompilers }

procedure TPersistCompilers.Load(const Compilers: ICompilers);
  {Loads a list of compilers from persistent storage.
    @param Compilers [in] List of compilers to load.
  }
var
  Compiler: ICompiler;        // refers to each compiler
  Prefixes: TCompLogPrefixes; // compiler log prefixes from storage
  PrefixID: TCompLogPrefixID; // loops thru all compiler log prefixes
  Storage: ISettingsSection;  // object used to access persistent storage
  ExePath: string;            // value of ExePath value in storage file
begin
  // Loop thru each supported compiler
  for Compiler in Compilers do
  begin
    // Load values for compiler from persistent storage
    Storage := Settings.ReadSection(
      ssCompilerInfo, Compiler.GetIDString
    );

    // Get compiler path (if any) and store in compiler object if so
    ExePath := Storage.ItemValues['ExePath'];
    if ExePath <> '' then
      Compiler.SetExecFile(ExePath);

    // Load compiler log prefixes (format PrefixX)
    for PrefixID := Low(TCompLogPrefixID) to High(TCompLogPrefixID) do
      Prefixes[PrefixID] :=
        Storage.ItemValues[Format('Prefix%d', [Ord(PrefixID)])];
    Compiler.SetLogFilePrefixes(Prefixes);

    // Load command line switches (empty entry => use default)
    if Storage.ItemExists('Switches') then
      Compiler.SetSwitches(Storage.ItemValues['Switches']);
  end;
end;

procedure TPersistCompilers.Save(const Compilers: ICompilers);
  {Saves a list of compilers to storage.
    @param Compilers [in] List of compilers to save.
  }
var
  Compiler: ICompiler;        // refers to each compiler
  Prefixes: TCompLogPrefixes; // compiler log prefixes from storage
  PrefixID: TCompLogPrefixID; // loops thru all compiler log prefixes
  Storage: ISettingsSection;  // object used to access persistent storage
begin
  for Compiler in Compilers do
  begin
    // Store required values in persistent storage
    // get new empty storage object
    Storage := Settings.EmptySection(ssCompilerInfo, Compiler.GetIDString);
    // add required data to storage object
    Storage.ItemValues['ExePath'] := Compiler.GetExecFile;
    Prefixes := Compiler.GetLogFilePrefixes;
    for PrefixID := Low(TCompLogPrefixID) to High(TCompLogPrefixID) do
      Storage.ItemValues[Format('Prefix%d', [Ord(PrefixID)])] :=
        '"' + Prefixes[PrefixID] + '"';
    if Compiler.GetSwitches <> Compiler.GetDefaultSwitches then
      Storage.ItemValues['Switches'] := Compiler.GetSwitches;
    // save the data
    Storage.Save;
  end;
end;

end.

