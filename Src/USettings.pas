{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2006-2023, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Implements class that can store application settings in application wide and
 * per user persistent storage.
}


unit USettings;


interface


uses
  // Delphi
  SysUtils {in interface for inlining to work},
  // Project
  UIStringList;


type
  ///  <summary>Interface supported by objects that encapsulate a named settings
  ///  section that contains named values.</summary>
  ISettingsSection = interface(IInterface)
    ['{20D32E19-4780-4D72-A96E-C0A1D044C8FB}']

    ///  <summary>Returns the number of named values in the section.</summary>
    function GetItemCount: Integer;

    ///  <summary>Checks whether a value with a given name exists in the
    ///  section.</summary>
    function ItemExists(const Name: string): Boolean;

    ///  <summary>Deletes the value with the given name from the section.
    ///  </summary>
    procedure DeleteItem(const Name: string);

    ///  <summary>Deletes all the values from the section.</summary>
    procedure ClearItems;

    ///  <summary>Saves the data and all its values to persistent storage.
    ///  </summary>
    procedure Save;

    ///  <summary>Loads the section and all its values from persistent storage.
    ///  </summary>
    procedure Load;

    ///  <summary>Gets a named Boolean value from settings.</summary>
    ///  <param name="Name">string [in] Name of value.</param>
    ///  <param name="Default">Boolean [in] Value to return if named item not
    ///  present in storage.</param>
    ///  <returns>Boolean. The required value.</returns>
    function GetBoolean(const Name: string; const Default: Boolean = False):
      Boolean;

    ///  <summary>Records a named Boolean value in settings.</summary>
    ///  <param name="Name">string [in] Name of value.</param>
    ///  <param name="Value">Boolean [in] Value to be recored.</param>
    procedure SetBoolean(const Name: string; const Value: Boolean);

    ///  <summary>Gets a named integer value from settings.</summary>
    ///  <param name="Name">string [in] Name of value.</param>
    ///  <param name="Default">Integer [in] Value to return if named item not
    ///  present in storage.</param>
    ///  <returns>Integer. The required value.</returns>
    function GetInteger(const Name: string; const Default: Integer = 0):
      Integer;

    ///  <summary>Records a named integer value in settings.</summary>
    ///  <param name="Name">string [in] Name of value.</param>
    ///  <param name="Value">Integer [in] Value to be recored.</param>
    procedure SetInteger(const Name: string; const Value: Integer);

    ///  <summary>Gets a named string value from settings.</summary>
    ///  <param name="Name">string [in] Name of value.</param>
    ///  <param name="Default">string [in] Value to return if named item not
    ///  present in storage.</param>
    ///  <returns>string. The required value.</returns>
    function GetString(const Name: string; const Default: string = ''):
      string;

    ///  <summary>Records a named string value in settings.</summary>
    ///  <param name="Name">string [in] Name of value.</param>
    ///  <param name="Value">string [in] Value to be recored.</param>
    procedure SetString(const Name, Value: string);

    ///  <summary>Gets a floating point value from settings.</summary>
    ///  <param name="Name">string [in] Name of value.</param>
    ///  <param name="Default">Double [in] Value to return if named item not
    ///  present in storage.</param>
    ///  <returns>Double. The required value.</returns>
    function GetFloat(const Name: string; const Default: Double = 0.0): Double;

    ///  <summary>Records a named floating point value in settings.</summary>
    ///  <param name="Name">string [in] Name of value.</param>
    ///  <param name="Value">Double [in] Value to be recored.</param>
    procedure SetFloat(const Name: string; const Value: Double);

    ///  <summary>Gets a named date time value from settings.</summary>
    ///  <param name="Name">string [in] Name of value.</param>
    ///  <param name="Default">TDateTime [in] Value to return if named item not
    ///  present in storage.</param>
    ///  <returns>TDateTime. The required value.</returns>
    ///  <remarks>The value is stored in YYYY-MM-DD hh:mm:ss format, regardless
    ///  of locale.</remarks>
    function GetDateTime(const Name: string; const Default: TDateTime = 0.0):
      TDateTime;

    ///  <summary>Records a named date time value in settings.</summary>
    ///  <param name="Name">string [in] Name of value.</param>
    ///  <param name="Value">TDateTime [in] Value to be recored.</param>
    ///  <remarks>The value must be stored in YYYY-MM-DD hh:mm:ss format
    ///  regardless of locale.</remarks>
    procedure SetDateTime(const Name: string; const Value: TDateTime);

    ///  <summary>Gets a named byte array from settings.</summary>
    ///  <param name="Name">string [in] Name of value.</param>
    ///  <returns>TBytes. The required array of bytes.</returns>
    ///  <remarks>The value is stored as a comma separated sequence of decimal
    ///  values in the range 0..255.</remarks>
    function GetBytes(const Name: string): TBytes;

    ///  <summary>Stores a named byte array in settings.</summary>
    ///  <param name="Name">string [in] Name of value.</param>
    ///  <param name="Value">TBytes [in] Byte array to be recorded.</param>
    ///  <remarks>The bytes must be stored as comma separated decimal values.
    ///  </remarks>
    procedure SetBytes(const Name: string; const Value: TBytes);

    ///  <summary>Gets a list of related string values from the section.
    ///  </summary>
    ///  <param name="CountName">string [in] Name of an integer value that
    ///  specifies the number of related value in the list.</param>
    ///  <param name="ItemFmt">string [in] A format string that acts as a
    ///  template for the names of the related string values. ItemFmt must
    ///  contain a single %d specifier that represents the number of the value
    ///  in the list.</param>
    ///  <returns>IStringList [in] The list of strings from the list.</returns>
    function GetStrings(const CountName, ItemFmt: string): IStringList;

    ///  <summary>Stores a string list in the section as a set of values with
    ///  related names.</summary>
    ///  <param name="CountName">string [in] Name of an integer value that is
    ///  used to record the number of related value in the list.</param>
    ///  <param name="ItemFmt">string [in] A format string that acts as a
    ///  template for the names of the related string values. ItemFmt must
    ///  contain a single %d specifier that represents the number of the value
    ///  in the list.</param>
    ///  <param name="Value">IStringList [in] List of strings to be stored.
    ///  </param>
    procedure SetStrings(const CountName, ItemFmt: string;
      Value: IStringList);

    ///  <summary>Number of named values in the section.</summary>
    property ItemCount: Integer read GetItemCount;
  end;

type

  ///  <summary>
  ///  <para>Enumeration of recognised sections within persistent storage.
  ///  </para>
  ///  <para>-ssFindText - info about last text search</para>
  ///  <para>-ssFindCompiler - info about last compiler search</para>
  ///  <para>-ssFindXRefs - info about last XRef search</para>
  ///  <para>-ssCompilerInfo - info about each supported compiler</para>
  ///  <para>-ssPreferences - info about program preferences</para>
  ///  <para>-ssUnits - list of default units</para>
  ///  <para>-ssDuplicateSnippet - persistent settings from Duplicate Snippets
  ///  dlg</para>
  ///  <para>-ssFavourites - persistent settings from Favourites dlg</para>
  ///  <para>-ssWindowState - info about the size and state of various
  ///  windows</para>
  ///  <para>-ssCompilers - info about all compilers</para>
  ///  <para>-ssCollections - info about all snippet collections</para>
  ///  <para>-ssCollection - info about a specific snippet collection</para>
  ///  </summary>
  TSettingsSectionId = (
    ssFindText, ssFindCompiler, ssFindXRefs, ssCompilerInfo,
    ssPreferences, ssUnits, ssDuplicateSnippet,
    ssFavourites, ssWindowState, ssCompilers, ssCollections,
    ssCollection
  );

type
  ///  <summary>Interface that defines top level operations on a settings
  ///  object.</summary>
  ISettings = interface(IInterface)
    ['{6ADBEE7E-83A0-423A-AA7D-86C87D23C1C8}']

    ///  <summary>Reads a given section, and optional sub-section, and all its
    ///  values from persistent storage.</summary>
    ///  <param name="Section">TSettingsSectionId [in] Id of section to be read.
    ///  </param>
    ///  <param name="SubSection">string [in] Optional. Name of subsection.
    ///  </param>
    ///  <returns>ISettingsSection. Reference to object encapsulating the
    ///  section.</returns>
    ///  <remarks>Clients can modify the values in the returned section and save
    ///  it, overwriting the existing section.</remarks>
    function ReadSection(const Section: TSettingsSectionId;
      const SubSection: string = ''): ISettingsSection;

    ///  <summary>Creates a new empty section and optional sub-section.
    ///  </summary>
    ///  <param name="Section">TSettingsSectionId [in] Id of section to be
    ///  created.</param>
    ///  <param name="SubSection">string [in] Optional. Name of subsection.
    ///  </param>
    ///  <returns>ISettingsSection. Reference to object encapsulating the
    ///  section.</returns>
    ///  <remarks>Clients can add values to the returned section and save it,
    ///  overwriting the existing section.</remarks>
    function EmptySection(const Section: TSettingsSectionId;
      const SubSection: string = ''): ISettingsSection;
  end;


///  <summary>Returns reference to Settings singleton object.</summary>
function Settings: ISettings;


implementation


uses
  // Delphi
  Classes,
  IniFiles,
  IOUtils,
  // Project
  UAppInfo,
  UHexUtils,
  UIOUtils,
  UStrUtils,
  UUtils;


var
  // Private global variable: stores reference to settings singleton object
  pvtSettings: ISettings = nil;

type
  ///  <summary>Base class for all settings classes that use setting ini file
  /// for persisent storage.</summary>
  ///  <remarks>Implements core ini file functionality.</remarks>
  TIniSettingsBase = class(TInterfacedObject)
  strict protected
    ///  <summary>Creates and returns a TIniFile instance onto the settings ini
    ///  file.</summary>
    ///  <remarks>The caller is responsible for freeing the returned instance.
    ///  </remarks>
    function CreateIniFile: TIniFile;
  public
    ///  <summary>Constructs new object instance.</summary>
    constructor Create;
  end;

type
  ///  <summary>Class that manages access to ini-file based persistant storage.
  ///  </summary>
  TIniSettings = class(TIniSettingsBase, ISettings)
  strict private
    ///  <summary>Creates and returns an object representing an ini file
    ///  section or sub-section.</summary>
    ///  <param name="SectionID">TSettingsSectionId [in] Id of required section.
    ///  </param>
    ///  <param name="SubSection">string [in] Name of any subsection or the
    ///  empty section if no subsection is required.</param>
    function CreateSection(const SectionID: TSettingsSectionId;
      const SubSection: string): ISettingsSection;
  strict protected
    ///  <summary>Returns the name of a given section in the ini file.</summary>
    ///  <param name="Id">TSettingsSectionId [in] Id of required section.
    ///  </param>
    ///  <param name="SubSection">string [in] Name of any subsection or the
    ///  empty section if no subsection is required.</param>
    ///  <returns>string. Name of required section.</returns>
    function SectionName(const Id: TSettingsSectionId;
      const SubSection: string = ''): string;
  public

    ///  <summary>Reads a given section, and optional sub-section, and all its
    ///  values from persistent storage.</summary>
    ///  <param name="Section">TSettingsSectionId [in] Id of section to be read.
    ///  </param>
    ///  <param name="SubSection">string [in] Optional. Name of subsection.
    ///  </param>
    ///  <returns>ISettingsSection. Reference to object encapsulating the
    ///  section.</returns>
    ///  <remarks>
    ///  <para>Clients can modify the values in the returned section and save
    ///  it, overwriting the existing section.</para>
    ///  <para>Method of ISettingsSection.</para>
    ///  </remarks>
    function ReadSection(const Section: TSettingsSectionId;
      const SubSection: string = ''): ISettingsSection;

    ///  <summary>Creates a new empty section and optional sub-section.
    ///  </summary>
    ///  <param name="Section">TSettingsSectionId [in] Id of section to be
    ///  created.</param>
    ///  <param name="SubSection">string [in] Optional. Name of subsection.
    ///  </param>
    ///  <returns>ISettingsSection. Reference to object encapsulating the
    ///  section.</returns>
    ///  <remarks>
    ///  <para>Clients can add values to the returned section and save it,
    ///  overwriting the existing section.</para>
    ///  <para>Method of ISettingsSection.</para>
    ///  </remarks>
    function EmptySection(const Section: TSettingsSectionId;
      const SubSection: string = ''): ISettingsSection;
  end;

type
  ///  <summary>Encapsulates a data section in settings ini files.</summary>
  ///  <remarks>Data items can be read and updated and setting can updated.
  ///  </remarks>
  TIniSettingsSection = class(TIniSettingsBase, ISettingsSection)
  strict private
    var
      ///  <summary>Name of section.</summary>
      fSectionName: string;
      ///  <summary>Stores section's data as name=value pairs.</summary>
      fValues: TStringList;

    ///  <summary>Parses the given string representing a date in the config file
    ///  into a TDateTime value that is returned.</summary>
    function ParseConfigDate(const S: string): TDateTime;

    ///  <summary>Gets a named string value from settings.</summary>
    ///  <param name="Name">string [in] Value name.</param>
    ///  <returns>string. The required value.</returns>
    ///  <remarks>Returns the empty string if there is no value with the given
    ///  name.</remarks>
    function GetItemValue(const Name: string): string;

    ///  <summary>Records a named value in settings.</summary>
    ///  <param name="Name">string [in] Name of value.</param>
    ///  <param name="Value">string [in] Value to be recored.</param>
    procedure SetItemValue(const Name, Value: string);

  public

    ///  <summary>Construct a new object instance that encapsulates an empty
    ///  section.</summary>
    ///  <param name="Section">string [in] Name of section in ini file.</param>
    constructor Create(const Section: string);

    ///  <summary>Destroys object instance.</summary>
    destructor Destroy; override;

    ///  <summary>Returns the number of named values in the section.</summary>
    ///  <remarks>Method of ISettingsSection.</remarks>
    function GetItemCount: Integer;

    ///  <summary>Checks whether a value with a given name exists in the
    ///  section.</summary>
    ///  <remarks>Method of ISettingsSection.</remarks>
    function ItemExists(const Name: string): Boolean;

    ///  <summary>Deletes the value with the given name from the section.
    ///  </summary>
    ///  <remarks>Method of ISettingsSection.</remarks>
    procedure DeleteItem(const Name: string);

    ///  <summary>Deletes all the values from the section.</summary>
    ///  <remarks>Method of ISettingsSection.</remarks>
    procedure ClearItems;

    ///  <summary>Saves the data and all its values to persistent storage.
    ///  </summary>
    ///  <remarks>Method of ISettingsSection.</remarks>
    procedure Save;

    ///  <summary>Loads the section and all its values from persistent storage.
    ///  </summary>
    ///  <remarks>Method of ISettingsSection.</remarks>
    procedure Load;

    ///  <summary>Gets a named Boolean value from settings.</summary>
    ///  <param name="Name">string [in] Name of value.</param>
    ///  <param name="Default">Boolean [in] Value to return if named item not
    ///  present in storage.</param>
    ///  <returns>Boolean. The required value.</returns>
    ///  <remarks>Method of ISettingsSection.</remarks>
    function GetBoolean(const Name: string; const Default: Boolean = False):
      Boolean;

    ///  <summary>Records a named Boolean value in settings.</summary>
    ///  <param name="Name">string [in] Name of value.</param>
    ///  <param name="Value">Boolean [in] Value to be recored.</param>
    ///  <remarks>Method of ISettingsSection.</remarks>
    procedure SetBoolean(const Name: string; const Value: Boolean);

    ///  <summary>Gets a named integer value from settings.</summary>
    ///  <param name="Name">string [in] Name of value.</param>
    ///  <param name="Default">Integer [in] Value to return if named item not
    ///  present in storage.</param>
    ///  <returns>Integer. The required value.</returns>
    ///  <remarks>Method of ISettingsSection.</remarks>
    function GetInteger(const Name: string; const Default: Integer = 0):
      Integer;

    ///  <summary>Records a named integer value in settings.</summary>
    ///  <param name="Name">string [in] Name of value.</param>
    ///  <param name="Value">Integer [in] Value to be recored.</param>
    ///  <remarks>Method of ISettingsSection.</remarks>
    procedure SetInteger(const Name: string; const Value: Integer);

    ///  <summary>Gets a named string value from settings.</summary>
    ///  <param name="Name">string [in] Name of value.</param>
    ///  <param name="Default">string [in] Value to return if named item not
    ///  present in storage.</param>
    ///  <returns>string. The required value.</returns>
    ///  <remarks>Method of ISettingsSection.</remarks>
    function GetString(const Name: string; const Default: string = ''):
      string;

    ///  <summary>Records a named string value in settings.</summary>
    ///  <param name="Name">string [in] Name of value.</param>
    ///  <param name="Value">string [in] Value to be recored.</param>
    ///  <remarks>Method of ISettingsSection.</remarks>
    procedure SetString(const Name, Value: string);

    ///  <summary>Gets a floating point value from settings.</summary>
    ///  <param name="Name">string [in] Name of value.</param>
    ///  <param name="Default">Double [in] Value to return if named item not
    ///  present in storage.</param>
    ///  <returns>Double. The required value.</returns>
    ///  <remarks>Method of ISettingsSection.</remarks>
    function GetFloat(const Name: string; const Default: Double = 0.0): Double;

    ///  <summary>Records a named floating point value in settings.</summary>
    ///  <param name="Name">string [in] Name of value.</param>
    ///  <param name="Value">Double [in] Value to be recored.</param>
    ///  <remarks>Method of ISettingsSection.</remarks>
    procedure SetFloat(const Name: string; const Value: Double);

    ///  <summary>Gets a named date time value from settings.</summary>
    ///  <param name="Name">string [in] Name of value.</param>
    ///  <param name="Default">TDateTime [in] Value to return if named item not
    ///  present in storage.</param>
    ///  <returns>TDateTime. The required value.</returns>
    ///  <remarks>
    ///  <para>The value is stored in YYYY-MM-DD hh:mm:ss format, regardless of
    ///  locale.</para>
    ///  <para>Method of ISettingsSection.</para>
    ///  </remarks>
    function GetDateTime(const Name: string; const Default: TDateTime = 0.0):
      TDateTime;

    ///  <summary>Records a named date time value in settings.</summary>
    ///  <param name="Name">string [in] Name of value.</param>
    ///  <param name="Value">TDateTime [in] Value to be recorded.</param>
    ///  <remarks>
    ///  <para>The value must be stored in YYYY-MM-DD hh:mm:ss format
    ///  regardless of locale.</para>
    ///  <para>Method of ISettingsSection.</para>
    ///  </remarks>
    procedure SetDateTime(const Name: string; const Value: TDateTime);

    ///  <summary>Gets a named byte array from settings.</summary>
    ///  <param name="Name">string [in] Name of value.</param>
    ///  <returns>TBytes. The required array of bytes.</returns>
    ///  <remarks>
    ///  <para>The value is stored as a comma separated sequence of decimal
    ///  values in the range 0..255.</para>
    ///  <para>Method of ISettingsSection.</para>
    ///  </remarks>
    function GetBytes(const Name: string): TBytes;

    ///  <summary>Stores a named byte array in settings.</summary>
    ///  <param name="Name">string [in] Name of value.</param>
    ///  <param name="Value">TBytes [in] Byte array to be recorded.</param>
    ///  <remarks>
    ///  <para>The bytes must be stored as comma separated decimal values.
    ///  </para>
    ///  <para>Method of ISettingsSection.</para>
    ///  </remarks>
    procedure SetBytes(const Name: string; const Value: TBytes);

    ///  <summary>Gets a list of related string values from the section.
    ///  </summary>
    ///  <param name="CountName">string [in] Name of an integer value that
    ///  specifies the number of related value in the list.</param>
    ///  <param name="ItemFmt">string [in] A format string that acts as a
    ///  template for the names of the related string values. ItemFmt must
    ///  contain a single %d specifier that represents the number of the value
    ///  in the list.</param>
    ///  <returns>IStringList [in] The list of strings from the list.</returns>
    ///  <remarks>Method of ISettingsSection.</remarks>
    function GetStrings(const CountName, ItemFmt: string): IStringList;

    ///  <summary>Stores a string list in the section as a set of values with
    ///  related names.</summary>
    ///  <param name="CountName">string [in] Name of an integer value that is
    ///  used to record the number of related value in the list.</param>
    ///  <param name="ItemFmt">string [in] A format string that acts as a
    ///  template for the names of the related string values. ItemFmt must
    ///  contain a single %d specifier that represents the number of the value
    ///  in the list.</param>
    ///  <param name="Value">IStringList [in] List of strings to be stored.
    ///  </param>
    ///  <remarks>Method of ISettingsSection.</remarks>
    procedure SetStrings(const CountName, ItemFmt: string;
      Value: IStringList);

  end;

function Settings: ISettings;
begin
  if not Assigned(pvtSettings) then
    pvtSettings := TIniSettings.Create as ISettings;
  Result := pvtSettings;
end;

{ TIniSettingsBase }

constructor TIniSettingsBase.Create;
begin
  inherited;
  // Ensure storage directories exist
  TDirectory.CreateDirectory(TAppInfo.UserAppDir);
  TDirectory.CreateDirectory(TAppInfo.CommonAppDir);
end;

function TIniSettingsBase.CreateIniFile: TIniFile;
var
  FileName: string; // name if ini file
begin
  FileName := TAppInfo.UserConfigFileName;
  if not TFile.Exists(FileName, False) then
    // create empty Unicode text file with BOM to force Win API to write Unicode
    TFileIO.WriteAllText(FileName, '', TEncoding.Unicode, True);
  Result := TIniFile.Create(FileName);
end;

{ TIniSettings }

function TIniSettings.CreateSection(const SectionID: TSettingsSectionId;
  const SubSection: string): ISettingsSection;
begin
  Result := TIniSettingsSection.Create(SectionName(SectionID, SubSection));
end;

function TIniSettings.EmptySection(const Section: TSettingsSectionId;
  const SubSection: string): ISettingsSection;
begin
  Result := CreateSection(Section, SubSection);
end;

function TIniSettings.ReadSection(const Section: TSettingsSectionId;
  const SubSection: string = ''): ISettingsSection;
begin
  Result := CreateSection(Section, SubSection);
  Result.Load;
end;

function TIniSettings.SectionName(const Id: TSettingsSectionId;
  const SubSection: string): string;
const
  // Map of section ids to names
  cSectionNames: array[TSettingsSectionId] of string = (
    'FindText',         // ssFindText
    'FindCompiler',     // ssFindCompiler
    'FindXRefs',        // ssFindXRefs
    'Cmp',              // ssCompilerInfo
    'Prefs',            // ssPreferences
    'UnitList',         // ssUnits
    'DuplicateSnippet', // ssDuplicateSnippet
    'Favourites',       // ssFavourites
    'WindowState',      // ssWindowState
    'Compilers',        // ssCompilers
    'Collections',      // ssCollections
    'Collection'        // ssCollection
  );
begin
  Result := cSectionNames[Id];
  if SubSection <> '' then
    Result := Result + ':' + SubSection;
end;

{ TIniSettingsSection }

procedure TIniSettingsSection.ClearItems;
begin
  fValues.Clear;
end;

constructor TIniSettingsSection.Create(const Section: string);
begin
  inherited Create;
  fValues := TStringList.Create;
  fSectionName := Section;
end;

procedure TIniSettingsSection.DeleteItem(const Name: string);
var
  Idx: Integer; // index of name in list
begin
  Idx := fValues.IndexOfName(Name);
  if Idx >= 0 then
    fValues.Delete(Idx);
end;

destructor TIniSettingsSection.Destroy;
begin
  FreeAndNil(fValues);
  inherited;
end;

function TIniSettingsSection.GetBoolean(const Name: string;
  const Default: Boolean): Boolean;
var
  ValStr: string;
begin
  ValStr := GetItemValue(Name);
  if ValStr = '' then
    Exit(Default);
  Result := not StrMatchText(ValStr, ['0', 'false', 'no', 'n']);
end;

function TIniSettingsSection.GetBytes(const Name: string): TBytes;
var
  ValStr: string;
  ValSL: TStrings;
  B: Byte;
  Idx: Integer;
begin
  ValStr := StrTrim(GetItemValue(Name));
  if ValStr = '' then
  begin
    SetLength(Result, 0);
    Exit;
  end;
  ValSL := TStringList.Create;
  try
    StrExplode(ValStr, ',', ValSL, False, True);
    SetLength(Result, ValSL.Count);
    for Idx := 0 to Pred(ValSL.Count) do
    begin
      if TryStrToByte(ValSL[Idx], B) then
        Result[Idx] := B
      else
        Result[Idx] := 0;
    end;
  finally
    ValSL.Free;
  end;
end;

function TIniSettingsSection.GetDateTime(const Name: string;
  const Default: TDateTime): TDateTime;
var
  ValStr: string;
begin
  ValStr := GetItemValue(Name);
  if ValStr = '' then
    Exit(Default);
  Result := ParseConfigDate(ValStr);
end;

function TIniSettingsSection.GetFloat(const Name: string;
  const Default: Double): Double;
begin
  Result := StrToFloatDef(GetItemValue(Name), Default);
end;

function TIniSettingsSection.GetInteger(const Name: string;
  const Default: Integer): Integer;
begin
  Result := StrToIntDef(GetItemValue(Name), Default);
end;

function TIniSettingsSection.GetItemCount: Integer;
begin
  Result := fValues.Count;
end;

function TIniSettingsSection.GetItemValue(const Name: string): string;
begin
  Result := fValues.Values[Name];
end;

function TIniSettingsSection.GetString(const Name, Default: string): string;
begin
  Result := GetItemValue(Name);
  if Result = '' then
    Result := Default;
end;

function TIniSettingsSection.GetStrings(const CountName,
  ItemFmt: string): IStringList;
var
  Idx: Integer; // loops thru string list items
begin
  Result := TIStringList.Create;
  for Idx := 0 to Pred(StrToIntDef(GetItemValue(CountName), 0)) do
    Result.Add(GetItemValue(Format(ItemFmt, [Idx])));
end;

function TIniSettingsSection.ItemExists(const Name: string): Boolean;
begin
  Result := fValues.IndexOfName(Name) >= 0;
end;

procedure TIniSettingsSection.Load;
var
  Ini: TIniFile;
begin
  // Read all values from section in app's ini file to data item storage
  Ini := CreateIniFile;
  try
    Ini.ReadSectionValues(fSectionName, fValues);
  finally
    Ini.Free;
  end;
end;

function TIniSettingsSection.ParseConfigDate(const S: string): TDateTime;
begin
  Result := EncodeDate(
    StrToInt(StrSlice(S, 1, 4)),
    StrToInt(StrSlice(S, 6, 2)),
    StrToInt(StrSlice(S, 9, 2))
  )
  +
  EncodeTime(
    StrToInt(StrSlice(S, 12, 2)),
    StrToInt(StrSlice(S, 15, 2)),
    StrToInt(StrSlice(S, 18, 2)),
    0
  );
end;

procedure TIniSettingsSection.Save;
var
  Idx: Integer; // loops thru all data items in section
  Ini: TIniFile;
begin
  // Open application's ini file
  Ini := CreateIniFile;
  try
    // Delete any existing section with same name
    Ini.EraseSection(fSectionName);
    // Write all data items to ini file section
    for Idx := 0 to Pred(fValues.Count) do
      Ini.WriteString(
        fSectionName, fValues.Names[Idx], fValues.ValueFromIndex[Idx]
      );
  finally
    Ini.Free;
  end;
end;

procedure TIniSettingsSection.SetBoolean(const Name: string;
  const Value: Boolean);
const
  // do not localise these strings
  BoolStrs: array[Boolean] of string = ('False', 'True');
begin
  SetItemValue(Name, BoolStrs[Value]);
end;

procedure TIniSettingsSection.SetBytes(const Name: string;
  const Value: TBytes);
var
  SL: TStrings;
  B: Byte;
begin
  SL := TStringList.Create;
  try
    for B in Value do
    begin
      SL.Add(IntToStr(B));
    end;
    SetItemValue(Name, StrJoin(SL, ','));
  finally
    SL.Free;
  end;
end;

procedure TIniSettingsSection.SetDateTime(const Name: string;
  const Value: TDateTime);
begin
  SetItemValue(Name, FormatDateTime('yyyy"-"mm"-"dd" "hh":"nn":"ss', Value));
end;

procedure TIniSettingsSection.SetFloat(const Name: string; const Value: Double);
begin
  SetItemValue(Name, FloatToStr(Value));
end;

procedure TIniSettingsSection.SetInteger(const Name: string;
  const Value: Integer);
begin
  SetItemValue(Name, IntToStr(Value));
end;

procedure TIniSettingsSection.SetItemValue(const Name, Value: string);
var
  Idx: Integer; // index of name in list
begin
  if Value <> '' then
    fValues.Values[Name] := Value
  else
  begin
    // Value is '': can't simply do fValues.Values[Name] := Value since this
    // would delete name from list and we want it there
    Idx := fValues.IndexOfName(Name);
    if Idx = -1 then
      fValues.Add(Name + '=')
    else
      fValues[Idx] := Name + '=';
  end;
end;

procedure TIniSettingsSection.SetString(const Name, Value: string);
begin
  SetItemValue(Name, Value);
end;

procedure TIniSettingsSection.SetStrings(const CountName, ItemFmt: string;
  Value: IStringList);
var
  Idx: Integer; // loops through string list items
begin
  Assert(Assigned(Value), ClassName + '.SetStrings: Value is nil');
  SetItemValue(CountName, IntToStr(Value.Count));
  for Idx := 0 to Pred(Value.Count) do
    SetItemValue(Format(ItemFmt, [Idx]), Value[Idx]);
end;

end.


