{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2006-2013, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
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

  {
  ISettingsSection:
    Interface supported by objects that encapsulate a named data section in
    application's persistent storage. Allows data items in section to be read
    and written and section itelsf to be read or written to storage.
  }
  ISettingsSection = interface(IInterface)
    ['{20D32E19-4780-4D72-A96E-C0A1D044C8FB}']
    function GetItemCount: Integer;
      {Gets number of data items in section.
        @return Number of data items.
      }
    function ItemExists(const Name: string): Boolean;
      {Checks if a specified item in this list exists.
        @param Name [in] Name of item.
        @return Whether Name exists in section.
      }
    procedure DeleteItem(const Name: string);
      {Deletes a named item from the list. If the item is not in the list no
      action is taken.
        @param Name [in] Name of item to delete.
      }
    procedure ClearItems;
      {Clears all data items from a section.
      }
    procedure Save;
      {Saves section with all its data items to application's persistent
      storage.
      }
    procedure Load;
      {Loads section and all its data items from application's persistent
      storage.
      }
    ///  <summary>Gets a named Boolean value from settings.</summary>
    ///  <param name="Name">string [in] Name of item.</param>
    ///  <param name="Default">Boolean [in] Value to return if named item not
    ///  present in storage.</param>
    ///  <returns>Boolean. The required value.</returns>
    function GetBoolean(const Name: string; const Default: Boolean = False):
      Boolean;
    ///  <summary>Records a named Boolean value in settings.</summary>
    ///  <param name="Name">string [in] Name of item.</param>
    ///  <param name="Value">Boolean [in] Value to be recored.</param>
    procedure SetBoolean(const Name: string; const Value: Boolean);
    ///  <summary>Gets a named integer value from settings.</summary>
    ///  <param name="Name">string [in] Name of item.</param>
    ///  <param name="Default">Integer [in] Value to return if named item not
    ///  present in storage.</param>
    ///  <returns>Integer. The required value.</returns>
    function GetInteger(const Name: string; const Default: Integer = 0):
      Integer;
    ///  <summary>Records a named integer value in settings.</summary>
    ///  <param name="Name">string [in] Name of item.</param>
    ///  <param name="Value">Integer [in] Value to be recored.</param>
    procedure SetInteger(const Name: string; const Value: Integer);
    ///  <summary>Gets a named string value from settings.</summary>
    ///  <param name="Name">string [in] Name of item.</param>
    ///  <param name="Default">string [in] Value to return if named item not
    ///  present in storage.</param>
    ///  <returns>string. The required value.</returns>
    function GetString(const Name: string; const Default: string = ''):
      string;
    ///  <summary>Records a named string value in settings.</summary>
    ///  <param name="Name">string [in] Name of item.</param>
    ///  <param name="Value">string [in] Value to be recored.</param>
    procedure SetString(const Name, Value: string);
    ///  <summary>Gets a floating point value from settings.</summary>
    ///  <param name="Name">string [in] Name of item.</param>
    ///  <param name="Default">Double [in] Value to return if named item not
    ///  present in storage.</param>
    ///  <returns>Double. The required value.</returns>
    function GetFloat(const Name: string; const Default: Double = 0.0): Double;
    ///  <summary>Records a named floating point value in settings.</summary>
    ///  <param name="Name">string [in] Name of item.</param>
    ///  <param name="Value">Double [in] Value to be recored.</param>
    procedure SetFloat(const Name: string; const Value: Double);
    ///  <summary>Gets a named date time value from settings.</summary>
    ///  <param name="Name">string [in] Name of item.</param>
    ///  <param name="Default">TDateTime [in] Value to return if named item not
    ///  present in storage.</param>
    ///  <returns>TDateTime. The required value.</returns>
    ///  <remarks>The value is stored in YYYY-MM-DD hh:mm:ss format, regardless
    ///  of locale.</remarks>
    function GetDateTime(const Name: string; const Default: TDateTime = 0.0):
      TDateTime;
    ///  <summary>Records a named date time value in settings.</summary>
    ///  <param name="Name">string [in] Name of item.</param>
    ///  <param name="Value">TDateTime [in] Value to be recored.</param>
    ///  <remarks>The value must be stored in YYYY-MM-DD hh:mm:ss format
    ///  regardless of locale.</remarks>
    procedure SetDateTime(const Name: string; const Value: TDateTime);
    ///  <summary>Gets a named encrypted string value from settings and
    ///  unencrypts it.</summary>
    ///  <param name="Name">string [in] Name of item.</param>
    ///  <returns>string. The unencrypted string.</returns>
    function GetEncryptedString(const Name: string): string;
    ///  <summary>Encrypts and records a named string value in settings.
    ///  </summary>
    ///  <param name="Name">string [in] Name of item.</param>
    ///  <param name="Value">string [in] Value to be encrypted and recored.
    ///  </param>
    procedure SetEncryptedString(const Name, Value: string);
    function GetStrings(const CountName, ItemFmt: string): IStringList;
      {Reads a string list from storage. There must be a value containing number
      of elements and correct number of further elements containing each entry
      in string list.
        @param CountName [in] Name of value containing number of elements in
          string list.
        @param ItemFmt [in] Format string that provides a template names of
          values of list items. ItemFmt must contain a single %d specifier that
          is replaced by the item number.
        @return Required string list.
      }
    procedure SetStrings(const CountName, ItemFmt: string;
      Value: IStringList);
      {Stores a string list in storage.
        @param CountName [in] Name of value that stores number of elements in
          string list.
        @param ItemFmt [in] Format string that provides a template of the names
          of the string list entries. It must contain just one %d specifier that
          is replaced by the item number.
        @param Value [in] String list to be stored.
      }
    property ItemCount: Integer read GetItemCount;
      {Number of data items in section represented by object}
  end;

  {
  TSettingsStorageId:
    Identifies a persisent storage used to persist settings.
  }
  TSettingsStorageId = (
    ssUser,     // storage for per-user settings
    ssCommon    // storage for common (application level) settings
  );

  {
  TSettingsSectionId:
    Ids of valid sections within application's persistent storage.
  }
  TSettingsSectionId = (
    ssFindText,           // info about last text search
    ssFindCompiler,       // info about last compiler search
    ssFindXRefs,          // info about last XRef search
    ssCompilerInfo,       // info about each supported compiler
    ssApplication,        // info about the application
    ssPreferences,        // info about program preferences
    ssUserInfo,           // info about user
    ssProxyServer,        // info about any proxy server
    ssUnits,              // list of default units
    ssDuplicateSnippet,   // persistent settings from Duplicate Snippets dlg
    ssFavourites,         // persistent settings from Favourites dlg
    ssWindowState,        // info about the size and state of various windows
    ssDatabase,           // database customisation info
    ssUpdateChecks        // info about update checks
  );

  {
  ISettings:
    Interface of object that manages program's persistant storage and returns
    information about, and contents of, sections within storage.
  }
  ISettings = interface(IInterface)
    ['{6ADBEE7E-83A0-423A-AA7D-86C87D23C1C8}']
    function ReadSection(const Section: TSettingsSectionId;
      const SubSection: string = ''): ISettingsSection;
      {Reads a named section from persistent storage.
        @param Section [in] Id of section to be written to.
        @param SubSection [in] Name of sub section ('' if not supplied).
        @return Object that encapsulates section that was.
      }
    function EmptySection(const Section: TSettingsSectionId;
      const SubSection: string = ''): ISettingsSection;
      {Creates new empty section object to which data items can be written.
        @param Section [in] Id of section to be written to.
        @param SubSection [in] Name of sub section ('' if not supplied).
        @return Object that encapsulates new empty section.
      }
  end;


function Settings: ISettings;
  {Returns reference to Settings singleton.
    @return Reference to singleton.
  }


implementation


uses
  // Delphi
  Classes, IniFiles, IOUtils,
  // Project
  UAppInfo, UEncryptor, UExceptions, UHexUtils, UIOUtils, UStrUtils;


var
  // Private global variable: stores reference to settings singleton object
  pvtSettings: ISettings = nil;


type

  {
  TSettingsBase:
    Base class for all settings classes, regardless of storage medium.
  }
  TSettingsBase = class(TInterfacedObject)
  strict protected
    function SectionStorage(
      const Section: TSettingsSectionId): TSettingsStorageId;
      {Determines which storage a section is stored in.
        @param Section [in] Id of section.
        @return Id of required storage.
      }
  end;

  {
  TIniSettingsBase:
    Base class for all settings classes that use ini files for persisent
    storage. Implements core ini file functionality.
  }
  TIniSettingsBase = class(TSettingsBase)
  strict protected
    function StorageName(const Storage: TSettingsStorageId): string;
      {Maps storage id to name of storage.
        @param Storage [in] Storage id.
        @return Required storage name.
      }
    function CreateIniFile(const Storage: TSettingsStorageId): TIniFile;
      {Creates a TIniFile instance onto required storage ini file.
        @param Id [in] Id of storage for which object is required.
        @return TIniFile instance.
      }
  public
    constructor Create;
      {Class constructor. Sets up object.
      }
  end;

  {
  TIniSettings:
    Class that manages program's persistant storage in ini files and returns
    names of ini sections for access from other code.
  }
  TIniSettings = class(TIniSettingsBase,
    ISettings
  )
  strict private
    function CreateSection(const SectionID: TSettingsSectionId;
      const SubSection: string): ISettingsSection;
      {Creates object representing a section of the ini file.
        @param SectionID [in] Id of required section.
        @param SubSection [in] Name of any subsection ('' if none).
        @return Instance of object that represents section.
      }
  strict protected
    function SectionName(const Id: TSettingsSectionId;
      const SubSection: string = ''): string;
      {Gets name of a specified section in ini file.
        @param Id [in] Id of required section.
        @param SubSection [in] Name of any subsection ('' if none).
        @return Name of required section.
      }
  public
    { ISettings methods }
    function ReadSection(const Section: TSettingsSectionId;
      const SubSection: string = ''): ISettingsSection;
      {Reads a named section from persistent storage.
        @param Section [in] Id of section to be written to.
        @param SubSection [in] Name of sub section ('' if not supplied).
        @return Object that encapsulates section that was.
      }
    function EmptySection(const Section: TSettingsSectionId;
      const SubSection: string = ''): ISettingsSection;
      {Creates new empty section object to which data items can be written.
        @param Section [in] Id of section to be written to.
        @param SubSection [in] Name of sub section ('' if not supplied).
        @return Object that encapsulates new empty section.
      }
  end;

  {
  TIniSettingsSection:
    Encapsulates a named data section in one of application's ini files.
    Allows data items in section to be read and written and section to be read
    or written to ini file.
  }
  TIniSettingsSection = class(TIniSettingsBase,
    ISettingsSection
  )
  strict private
    fSectionName: string;         // Name of section
    fStorage: TSettingsStorageId; // Id of storage to be used
    fValues: TStringList;         // Stores section's data as name=value pairs
    function ParseConfigDate(const S: string): TDateTime;
    function GetItemValue(const Name: string): string;
      {Gets value of a named data item in section.
        @param Name [in] Name of data item.
        @return Value of data item.
      }
    procedure SetItemValue(const Name, Value: string);
      {Sets value of named data item in section.
        @param Name [in] Name of data item.
        @param Value [in] Value of data item.
      }
  public
    constructor Create(const Section: string;
      const Storage: TSettingsStorageId);
      {Class constructor. Creates object encapsulating empty section.
        @param Section [in] Name of section.
        @param Storage [in] Identifies storage to be used.
      }
    destructor Destroy; override;
      {Class destructor. Tears down object.
      }
    { ISettingsSection methods }
    function GetItemCount: Integer;
      {Gets number of data items in section.
        @return Number of data items.
      }
    function ItemExists(const Name: string): Boolean;
      {Checks if a specified item in this list exists.
        @param Name [in] Name of item.
        @return Whether Name exists in section.
      }
    procedure DeleteItem(const Name: string);
      {Deletes a named item from the list. If the item is not in the list no
      action is taken.
        @param Name [in] Name of item to delete.
      }
    procedure ClearItems;
      {Clears all data items from a section.
      }
    procedure Save;
      {Saves section with all its data items to application's persistent
      storage.
      }
    procedure Load;
      {Loads section and all its data items from application's persistent
      storage.
      }
    function GetBoolean(const Name: string; const Default: Boolean = False):
      Boolean;
    procedure SetBoolean(const Name: string; const Value: Boolean); inline;
    function GetInteger(const Name: string; const Default: Integer = 0):
      Integer; inline;
    procedure SetInteger(const Name: string; const Value: Integer); inline;
    function GetString(const Name: string; const Default: string = ''):
      string; inline;
    procedure SetString(const Name, Value: string); inline;
    function GetFloat(const Name: string; const Default: Double = 0.0): Double;
      inline;
    procedure SetFloat(const Name: string; const Value: Double); inline;
    function GetDateTime(const Name: string; const Default: TDateTime = 0.0):
      TDateTime;
    procedure SetDateTime(const Name: string; const Value: TDateTime); inline;
    ///  <summary>Gets a named encrypted string value from settings and
    ///  unencrypts it.</summary>
    ///  <param name="Name">string [in] Name of item.</param>
    ///  <returns>string. The unencrypted string.</returns>
    function GetEncryptedString(const Name: string): string;
    ///  <summary>Encrypts and records a named string value in settings.
    ///  </summary>
    ///  <param name="Name">string [in] Name of item.</param>
    ///  <param name="Value">string [in] Value to be encrypted and recored.
    ///  </param>
    procedure SetEncryptedString(const Name, Value: string);
    function GetStrings(const CountName, ItemFmt: string): IStringList;
      {Reads a string list from storage. There must be a value containing number
      of elements and correct number of further elements containing each entry
      in string list.
        @param CountName [in] Name of value containing number of elements in
          string list.
        @param ItemFmt [in] Format string that provides a template names of
          values of list items. ItemFmt must contain a single %d specifier that
          is replaced by the item number.
        @return Required string list.
      }
    procedure SetStrings(const CountName, ItemFmt: string;
      Value: IStringList);
      {Stores a string list in storage.
        @param CountName [in] Name of value that stores number of elements in
          string list.
        @param ItemFmt [in] Format string that provides a template of the names
          of the string list entries. It must contain just one %d specifier that
          is replaced by the item number.
        @param Value [in] String list to be stored.
      }
  end;

function Settings: ISettings;
  {Returns reference to Settings singleton.
    @return Reference to singleton.
  }
begin
  if not Assigned(pvtSettings) then
    pvtSettings := TIniSettings.Create as ISettings;
  Result := pvtSettings;
end;

{ TSettingsBase }

function TSettingsBase.SectionStorage(
  const Section: TSettingsSectionId): TSettingsStorageId;
  {Determines which storage a section is stored in.
    @param Section [in] Id of section.
    @return Id of required storage.
  }
const
  // Map of known sections onto storage that contains them
  cSectionStorageMap: array[TSettingsSectionId] of TSettingsStorageId = (
    ssUser,     // ssFindText
    ssUser,     // ssFindCompiler
    ssUser,     // ssFindXRefs
    ssUser,     // ssCompilerInfo
    ssCommon,   // ssApplication
    ssUser,     // ssPreferences
    ssUser,     // ssUserInfo
    ssUser,     // ssProxyServer
    ssUser,     // ssUnits
    ssUser,     // ssDuplicateSnippet
    ssUser,     // ssFavourites
    ssUser,     // ssWindowState
    ssUser,     // ssDatabase
    ssUser      // ssUpdateChecks
  );
begin
  Result := cSectionStorageMap[Section];
end;

{ TIniSettingsBase }

constructor TIniSettingsBase.Create;
  {Class constructor. Sets up object.
  }
begin
  inherited;
  // Ensure storage directories exist
  TDirectory.CreateDirectory(TAppInfo.UserAppDir);
  TDirectory.CreateDirectory(TAppInfo.CommonAppDir);
end;

function TIniSettingsBase.CreateIniFile(
  const Storage: TSettingsStorageId): TIniFile;
  {Creates a TIniFile instance onto required storage ini file.
    @param Id [in] Id of storage for which object is required.
    @return TIniFile instance.
  }
var
  FileName: string; // name if ini file
begin
  FileName := StorageName(Storage);
  if not TFile.Exists(FileName) then
    // create empty Unicode text file with BOM to force Win API to write Unicode
    TFileIO.WriteAllText(FileName, '', TEncoding.Unicode, True);
  Result := TIniFile.Create(FileName);
end;

function TIniSettingsBase.StorageName(
  const Storage: TSettingsStorageId): string;
  {Maps storage id to name of storage.
    @param Storage [in] Storage id.
    @return Required storage name.
  }
begin
  case Storage of
    ssUser:
      Result := TAppInfo.UserAppDir + '\User.config';
    ssCommon:
      Result := TAppInfo.CommonAppDir + '\Common.config';
    else
      raise EBug.Create(ClassName + '.StorageName: unknown storage type');
  end;
end;

{ TIniSettings }

function TIniSettings.CreateSection(const SectionID: TSettingsSectionId;
  const SubSection: string): ISettingsSection;
  {Creates object representing a section of the ini file.
    @param SectionID [in] Id of required section.
    @param SubSection [in] Name of any subsection ('' if none).
    @return Instance of object that represents section.
  }
begin
  Result := TIniSettingsSection.Create(
    SectionName(SectionID, SubSection),
    SectionStorage(SectionID)
  );
end;

function TIniSettings.EmptySection(const Section: TSettingsSectionId;
  const SubSection: string): ISettingsSection;
  {Creates new empty section object to which data items can be written.
    @param Section [in] Id of section to be written to.
    @param SubSection [in] Name of sub section ('' if not supplied).
    @return Object that encapsulates new empty section.
  }
begin
  Result := CreateSection(Section, SubSection);
end;

function TIniSettings.ReadSection(const Section: TSettingsSectionId;
  const SubSection: string = ''): ISettingsSection;
  {Reads a named section from persistent storage.
    @param Section [in] Id of section to be written to.
    @param SubSection [in] Name of sub section ('' if not supplied).
    @return Object that encapsulates section that was.
  }
begin
  Result := CreateSection(Section, SubSection);
  Result.Load;
end;

function TIniSettings.SectionName(const Id: TSettingsSectionId;
  const SubSection: string): string;
  {Gets name of a specified section in ini file.
    @param Id [in] Id of required section.
    @param SubSection [in] Name of any subsection ('' if none).
    @return Name of required section.
  }
const
  // Map of section ids to names
  cSectionNames: array[TSettingsSectionId] of string = (
    'FindText',         // ssFindText
    'FindCompiler',     // ssFindCompiler
    'FindXRefs',        // ssFindXRefs
    'Cmp',              // ssCompilerInfo
    'Application',      // ssApplication
    'Prefs',            // ssPreferences
    'UserInfo',         // ssUserInfo
    'ProxyServer',      // ssProxyServer
    'UnitList',         // ssUnits
    'DuplicateSnippet', // ssDuplicateSnippet
    'Favourites',       // ssFavourites
    'WindowState',      // ssWindowState
    'Database',         // ssDatabase
    'UpdateChecks'      // ssUpdateChecks
  );
begin
  Result := cSectionNames[Id];
  if SubSection <> '' then
    Result := Result + ':' + SubSection;
end;

{ TIniSettingsSection }

procedure TIniSettingsSection.ClearItems;
  {Clears all data items from a section.
  }
begin
  fValues.Clear;
end;

constructor TIniSettingsSection.Create(const Section: string;
  const Storage: TSettingsStorageId);
  {Class constructor. Creates object encapsulating empty section.
    @param Section [in] Name of section.
    @param Storage [in] Identifies storage to be used.
  }
begin
  inherited Create;
  fValues := TStringList.Create;
  fSectionName := Section;
  fStorage := Storage;
end;

procedure TIniSettingsSection.DeleteItem(const Name: string);
  {Deletes a named item from the list. If the item is not in the list no
  action is taken.
    @param Name [in] Name of item to delete.
  }
var
  Idx: Integer; // index of name in list
begin
  Idx := fValues.IndexOfName(Name);
  if Idx >= 0 then
    fValues.Delete(Idx);
end;

destructor TIniSettingsSection.Destroy;
  {Class destructor. Tears down object.
  }
begin
  FreeAndNil(fValues);
  inherited;
end;

function TIniSettingsSection.GetEncryptedString(const Name: string): string;
var
  EncryptedBytes: TBytes; // encrypted value as array of bytes
begin
  // NOTE:
  // See SetEncryptedString for details of how encrypted values are stored.
  if not TryHexToBytes(GetItemValue(Name), EncryptedBytes) then
    Exit('');
  Result := TEncoding.UTF8.GetString(TEncryptor.Decrypt(EncryptedBytes));
end;

function TIniSettingsSection.GetBoolean(const Name: string;
  const Default: Boolean): Boolean;
var
  ValStr: string;
begin
  ValStr := StrToLower(GetItemValue(Name));
  if ValStr = '' then
    Exit(Default);
  if (ValStr = '0') or (ValStr = 'false') or (ValStr = 'no')
    or (ValStr = 'n') then
    Exit(False);
  Result := True;
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
  {Gets number of data items in section.
    @return Number of data items.
  }
begin
  Result := fValues.Count;
end;

function TIniSettingsSection.GetItemValue(const Name: string): string;
  {Gets value of a named data item in section.
    @param Name [in] Name of data item.
    @return Value of data item.
  }
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
  {Reads a string list from storage. There must be a value containing number of
  elements and correct number of further elements containing each entry in
  string list.
    @param CountName [in] Name of value containing number of elements in string
      list.
    @param ItemFmt [in] Format string that provides a template names of values
      of list items. ItemFmt must contain a single %d specifier that is replaced
      by the item number.
    @return Required string list.
  }
var
  Idx: Integer; // loops thru string list items
begin
  Result := TIStringList.Create;
  for Idx := 0 to Pred(StrToIntDef(GetItemValue(CountName), 0)) do
    Result.Add(GetItemValue(Format(ItemFmt, [Idx])));
end;

function TIniSettingsSection.ItemExists(const Name: string): Boolean;
  {Checks if a specified item in this list exists.
    @param Name [in] Name of item.
    @return Whether Name exists in section.
  }
begin
  Result := fValues.IndexOfName(Name) >= 0;
end;

procedure TIniSettingsSection.Load;
  {Loads section and all its data items from application's persistent
  storage.
  }
begin
  // Read all values from section in app's ini file to data item storage
  with CreateIniFile(fStorage) do
    try
      ReadSectionValues(fSectionName, fValues);
    finally
      Free;
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
  {Saves section with all its data items to application's persistent
  storage.
  }
var
  Idx: Integer; // loops thru all data items in section
begin
  // Open application's ini file
  with CreateIniFile(fStorage) do
    try
      // Delete any existing section with same name
      EraseSection(fSectionName);
      // Write all data items to ini file section
      for Idx := 0 to Pred(fValues.Count) do
        WriteString(
          fSectionName, fValues.Names[Idx], fValues.ValueFromIndex[Idx]
        );
    finally
      Free;
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

procedure TIniSettingsSection.SetDateTime(const Name: string;
  const Value: TDateTime);
begin
  SetItemValue(Name, FormatDateTime('yyyy"-"mm"-"dd" "hh":"nn":"ss', Value));
end;

procedure TIniSettingsSection.SetEncryptedString(const Name, Value: string);
begin
  // NOTE:
  // Encrypted values are stored as follows:
  // 1: Unicode Value string is converted to an array of UTF-8 encoded bytes
  // 2: The UTF-8 byte array is encrypted into another array of bytes
  // 3: The encrypted byte array is converted to hexadecimal
  // 4: The hexadecimal character string is stored in storage
  SetItemValue(
    Name, BytesToHex(TEncryptor.Encrypt(TEncoding.UTF8.GetBytes(Value)))
  );
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
  {Sets value of named data item in section.
    @param Name [in] Name of data item.
    @param Value [in] Value of data item.
  }
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
  {Stores a string list in storage.
    @param CountName [in] Name of value that stores number of elements in string
      list.
    @param ItemFmt [in] Format string that provides a template of the names of
      the string list entries. It must contain just one %d specifier that is
      replaced by the item number.
    @param Value [in] String list to be stored.
  }
var
  Idx: Integer; // loops through string list items
begin
  Assert(Assigned(Value), ClassName + '.SetStrings: Value is nil');
  SetItemValue(CountName, IntToStr(Value.Count));
  for Idx := 0 to Pred(Value.Count) do
    SetItemValue(Format(ItemFmt, [Idx]), Value[Idx]);
end;

end.

