{
 * USettings.pas
 *
 * Implements class that can store application settings in application wide and
 * per user persistent storage.
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
 * The Original Code is USettings.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2006-2010 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
}


unit USettings;


interface


uses
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
    function GetSectionName: string;
      {Gets name of section this object represents.
        @return Name of section.
      }
    function GetItemCount: Integer;
      {Gets number of data items in section.
        @return Number of data items.
      }
    function GetItemName(Idx: Integer): string;
      {Gets name of a data item in section by index.
        @param Idx [in] Index of data item.
        @return Name of data item.
      }
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
    function GetItemValueByIdx(Idx: Integer): string;
      {Gets value a data item by index.
        @param Idx [in] Index of data item.
        @return Value of data item.
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
    function GetEncryptedItemValue(const Name: string): string; overload;
      {Gets an encrypted value by name and unencrypts it.
        @param Name [in] Name of value.
        @return Required unencrypted value.
      }
    function GetEncryptedItemValue(const Idx: Integer): string; overload;
      {Gets an encrypted value by index and unencrypts it.
        @param Idx [in] Index of value.
        @return Required unencrypted value.
      }
    procedure SetEncryptedItemValue(const Name, Value: string);
      overload;
      {Encrypts and sets a named value.
        @param Name [in] Name of value.
        @param Value [in] Unencryped value to be encrypted.
      }
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
    property SectionName: string read GetSectionName;
      {Name of section represented by this object}
    property ItemCount: Integer read GetItemCount;
      {Number of data items in section represented by object}
    property ItemNames[Idx: Integer]: string read GetItemName;
      {List of names of data items in section represented by object}
    property ItemValues[const Name: string]: string
      read GetItemValue write SetItemValue;
      {List of data item values in section represented by object, indexed by
      data item name}
    property ItemValuesByIdx[Idx: Integer]: string read GetItemValueByIdx;
      {List of data item values in section represented by object, indexed by
      position in array}
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
    ssMainWindow,         // info about main window
    ssFindText,           // info about last text search
    ssFindCompiler,       // info about last compiler search
    ssFindXRefs,          // info about last XRef search
    ssCompilerInfo,       // info about each supported compiler
    ssApplication,        // info about the application
    ssPreferences,        // info about program preferences
    ssUserInfo,           // info about user
    ssProxyServer         // info about any proxy server
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
  SysUtils, Classes, IniFiles,
  // 3rd party
  UAppInfo, UEncryptor, UExceptions, UHexUtils, UUtils;


var
  // Private global variable: stores reference to settings singleton object
  pvtSettings: ISettings;


type

  {
  TSettingsBase:
    Base class for all settings classes, regardless of storage medium.
  }
  TSettingsBase = class(TInterfacedObject)
  protected
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
  protected
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
  private
    function CreateSection(const SectionID: TSettingsSectionId;
      const SubSection: string): ISettingsSection;
      {Creates object representing a section of the ini file.
        @param SectionID [in] Id of required section.
        @param SubSection [in] Name of any subsection ('' if none).
        @return Instance of object that represents section.
      }
  protected
    function SectionName(const Id: TSettingsSectionId;
      const SubSection: string = ''): string;
      {Gets name of a specified section in ini file.
        @param Id [in] Id of required section.
        @param SubSection [in] Name of any subsection ('' if none).
        @return Name of required section.
      }
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
  private
    fSectionName: string;         // Name of section
    fStorage: TSettingsStorageId; // Id of storage to be used
    fValues: TStringList;         // Stores section's data as name=value pairs
  protected
    { ISettingsSection methods }
    function GetSectionName: string;
      {Gets name of section this object represents.
        @return Name of section.
      }
    function GetItemCount: Integer;
      {Gets number of data items in section.
        @return Number of data items.
      }
    function GetItemName(Idx: Integer): string;
      {Gets name of a data item in section by index.
        @param Idx [in] Index of data item.
        @return Name of data item.
      }
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
    function GetItemValueByIdx(Idx: Integer): string;
      {Gets value a data item by index.
        @param Idx [in] Index of data item.
        @return Value of data item.
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
    function GetEncryptedItemValue(const Name: string): string; overload;
      {Gets an encrypted value by name and unencrypts it.
        @param Name [in] Name of value.
        @return Required unencrypted value.
      }
    function GetEncryptedItemValue(const Idx: Integer): string; overload;
      {Gets an encrypted value by index and unencrypts it.
        @param Idx [in] Index of value.
        @return Required unencrypted value.
      }
    procedure SetEncryptedItemValue(const Name, Value: string);
      overload;
      {Encrypts and sets a named value.
        @param Name [in] Name of value.
        @param Value [in] Unencryped value to be encrypted.
      }
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
  end;

function Settings: ISettings;
  {Returns reference to Settings singleton.
    @return Reference to singleton.
  }
begin
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
    ssUser,     // ssMainWindow
    ssUser,     // ssFindText
    ssUser,     // ssFindCompiler
    ssUser,     // ssFindXRefs
    ssUser,     // ssCompilerInfo
    ssCommon,   // ssApplication
    ssUser,     // ssPreferences
    ssUser,     // ssUserInfo
    ssUser      // ssProxyServer
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
  EnsureFolders(TAppInfo.UserAppDir);
  EnsureFolders(TAppInfo.CommonAppDir);
end;

function TIniSettingsBase.CreateIniFile(
  const Storage: TSettingsStorageId): TIniFile;
  {Creates a TIniFile instance onto required storage ini file.
    @param Id [in] Id of storage for which object is required.
    @return TIniFile instance.
  }
begin
  Result := TIniFile.Create(StorageName(Storage));
end;

function TIniSettingsBase.StorageName(
  const Storage: TSettingsStorageId): string;
  {Maps storage id to name of storage.
    @param Storage [in] Storage id.
    @return Required storage name.
  }
begin
  // We only support one storage: ssUser
  case Storage of
    ssUser:
      Result := TAppInfo.UserAppDir + '\User.3.ini';
    ssCommon:
      Result := TAppInfo.CommonAppDir + '\Common.ini';
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
    'MainWindow',       // ssMainWindow
    'FindText',         // ssFindText
    'FindCompiler',     // ssFindCompiler
    'FindXRefs',        // ssFindXRefs
    'Cmp',              // ssCompilerInfo
    'Application',      // ssApplication
    'Prefs',            // ssPreferences
    'UserInfo',         // ssUserInfo
    'ProxyServer'       // ssProxyServer
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

function TIniSettingsSection.GetEncryptedItemValue(const Name: string): string;
  {Gets an encrypted value by name and unencrypts it.
    @param Name [in] Name of value.
    @return Required unencrypted value.
  }
var
  EncryptedBytes: TBytes; // encrypted value as array of bytes
begin
  // NOTE:
  // See SetEncryptedItemValue for details of how encrypted values are stored.
  if not TryHexToBytes(GetItemValue(Name), EncryptedBytes) then
    Exit('');
  Result := TEncoding.UTF8.GetString(TEncryptor.Decrypt(EncryptedBytes));
end;

function TIniSettingsSection.GetEncryptedItemValue(const Idx: Integer): string;
  {Gets an encrypted value by index and unencrypts it.
    @param Idx [in] Index of value.
    @return Required unencrypted value.
  }
begin
  // NOTE:
  // See SetEncryptedItemValue for details of how encrypted values are stored.
  Result := GetEncryptedItemValue(GetItemName(Idx));
end;

function TIniSettingsSection.GetItemCount: Integer;
  {Gets number of data items in section.
    @return Number of data items.
  }
begin
  Result := fValues.Count;
end;

function TIniSettingsSection.GetItemName(Idx: Integer): string;
  {Gets name of a data item in section by index.
    @param Idx [in] Index of data item.
    @return Name of data item.
  }
begin
  Result := fValues.Names[Idx];
end;

function TIniSettingsSection.GetItemValue(const Name: string): string;
  {Gets value of a named data item in section.
    @param Name [in] Name of data item.
    @return Value of data item.
  }
begin
  Result := fValues.Values[Name];
end;

function TIniSettingsSection.GetItemValueByIdx(Idx: Integer): string;
  {Gets value a data item by index.
    @param Idx [in] Index of data item.
    @return Value of data item.
  }
begin
  Result := fValues.ValueFromIndex[Idx];
end;

function TIniSettingsSection.GetSectionName: string;
  {Gets name of section this object represents.
    @return Name of section.
  }
begin
  Result := fSectionName;
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

procedure TIniSettingsSection.SetEncryptedItemValue(const Name, Value: string);
  {Encrypts and sets a named value.
    @param Name [in] Name of value.
    @param Value [in] Unencryped value to be encrypted.
  }
begin
  // NOTE:
  // Encrypted values are stored as follows:
  // 1: Unicode Value is converted to an array of UTF-8 encoded bytes
  // 2: The UTF-8 byte array is encrypted as another array bytes
  // 3: The encrypted byte array is converted to hexadecimal
  // 4: The hexadecimal character string is stored in storage
  SetItemValue(
    Name, BytesToHex(TEncryptor.Encrypt(TEncoding.UTF8.GetBytes(Value)))
  );
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

initialization

// Initialise settings singletion
pvtSettings := TIniSettings.Create as ISettings;

finalization

// Free the singleton
pvtSettings := nil;

end.

