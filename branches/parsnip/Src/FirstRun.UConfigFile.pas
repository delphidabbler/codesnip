{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2007-2015, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * Implements class that manages the updating of older config files to the
 * current format.
}


unit FirstRun.UConfigFile;


interface


type
  ///  <summary>Class that manages the updating of older config files to the
  ///  current format. Missing files will also be created.</summary>
  TConfigFileUpdater = class abstract(TObject)
  strict private
    var
      ///  <summary>Name of config file.</summary>
      fCfgFileName: string;
    ///  <summary>Copies an ANSI format config file from SrcFileName, converted
    ///  to UTF-16LE encoded Unicode.</summary>
    procedure CopyANSIFile(const SrcFileName: string);
    ///  <summary>Copies a Unicode config file from SrcFileName.</summary>
    procedure CopyUnicodeFile(const SrcFileName: string);
  strict protected
    ///  <summary>Returns current config file version.</summary>
    ///  <remarks>Descendant classes must override to provide value.</remarks>
    class function GetFileVersion: Integer; virtual; abstract;
    ///  <summary>Name of config file.</summary>
    ///  <remarks>For use in descandant classes.</remarks>
    property CfgFileName: string read fCfgFileName;
  public
    ///  <summary>Constructs object to operate on given config file.</summary>
    ///  <remarks>Given file may or may not exist.</remarks>
    constructor Create(const CfgFileName: string);
    ///  <summary>Creates a new empty UTF-16LE encoded config file.</summary>
    ///  <remarks>Used to ensure the config file writing routines write in
    ///  Unicode. This works because ini access routines call Windows API
    ///  WritePrivateProfileString which only writes Unicode if the file it is
    ///  writing to is detected as Unicode. If not it writes ANSI text.
    ///  </remarks>
    procedure CreateNewFile;
    ///  <summary>Copies config file from SrcFileName. File created is always
    ///  Uincode and is converted if source file is ANSI, as specified by IsAnsi
    ///  parameter.</summary>
    procedure CopyFile(const SrcFileName: string; const IsAnsi: Boolean);
    ///  <summary>Gets file version number of config file.</summary>
    function FileVer: Integer;
    ///  <summary>Checks if program version in given config file is same as
    ///  current program version.</summary>
    class function IsCurrentProgramVer(const CfgFileName: string): Boolean;
      overload;
    ///  <summary>Checks if program version in config file is same as current
    ///  program version.</summary>
    function IsCurrentProgramVer: Boolean; overload;
    ///  <summary>Stamps config file with current and file version.</summary>
    procedure Stamp; virtual;
  end;

type
  ///  <summary>Config file updater class for use with per-user config file.
  ///  </summary>
  ///  <remarks>Provides current user config file version along with additional
  ///  functionality to TConfigFileUpdater that is relevant only to per-user
  ///  config files.</remarks>
  TUserConfigFileUpdater = class(TConfigFileUpdater)
  strict private
    const
      ///  <summary>Current user config file version.</summary>
      FileVersion = 16;
  strict protected
    ///  <summary>Returns current user config file version.</summary>
    class function GetFileVersion: Integer; override;
  public
    ///  <summary>Updates config file currently in original (pre v1.9) format to
    ///  current format.</summary>
    procedure UpdateFromOriginal;
    ///  <summary>Deletes any highlighter preferences.</summary>
    procedure DeleteHighligherPrefs;
    ///  <summary>Checks a proxy password is present in file.</summary>
    function HasProxyPassword: Boolean;
    ///  <summary>Deletes proxy password entry.</summary>
    procedure DeleteProxyPassword;
    ///  <summary>Updates Prefs:CodeGen section from format prior to version 9
    ///  to version 9 and later format.</summary>
    procedure UpdateCodeGenEntries;
    ///  <summary>Deletes unused key that determines detail pane index.
    ///  </summary>
    procedure DeleteDetailsPaneIndex;
    ///  <summary>Deletes unused keys from Prefs:Display section.</summary>
    procedure DeleteRedundantDisplayOptions;
    ///  <summary>Checks if config file contains any page structure
    ///  customisation information.</summary>
    function HasPageStructureInfo: Boolean;
    ///  <summary>Deletes all page structure customisation information from
    ///  Prefs:SnippetPageStructure section.</summary>
    procedure DeletePageStructureInfo;
    ///  <summary>Renames MainWindow section used prior to version 11 as
    ///  WindowState:MainForm.</summary>
    procedure RenameMainWindowSection;
    ///  <summary>Renames OverviewTab key in WindowState:MainForm as
    ///  OverviewGrouping.</summary>
    ///  <remarks>Must be run after RenameMainWindowSection if that method is
    ///  called.</remarks>
    procedure RenameMainWindowSectionOverviewGrouping;
    ///  <summary>Renames compiler identifiers used for Delphi 2005, 2006 and
    ///  2009 used to name value in FindCompiler section and in section names in
    ///  Cmp:XXX section names.</summary>
    procedure RenameCompilerIdentifiers;
    ///  <summary>Renames Prefs:PrintingSection as Printing and renames some
    ///  values within the section</summary>
    procedure RenamePrintingSectionAndValues;
    ///  <summary>Replaces any -NS switch in [Cmp:XXXX] sections' Switches value
    ///  with an equivalent entry in new Namespaces value, only if compiler XXX
    ///  is Delphi XE2 or later.</summary>
    procedure UpdateNamespaces;
    ///  <summary>Updates value names in FindXRefs section.</summary>
    procedure UpdateFindXRefs;
    ///  <summary>Adds Prefs:CodeGen section along with default data.</summary>
    procedure CreateDefaultCodeGenEntries;
    ///  <summary>Checks if a CodeSnip 4 style custom database directory has
    ///  been set created.</summary>
    function HasV4CustomDatabaseDirectory: Boolean;
    ///  <summary>Resets database directory to the default.</summary>
    ///  <remarks>This is done to ensure that an old version 4 database in a
    ///  custom location is not accidentally overwritten by a version 5
    ///  database. It is up to user to move the database to a new location.
    ///  </remarks>
    procedure ResetCustomDatabaseDirectory;
    ///  <summary>Stamps config file with current program and file versions.
    ///  </summary>
    ///  <remarks>Note that the user config file has program version written to
    ///  a different section to common config file, hence need for overridden
    ///  methods.</remarks>
    procedure Stamp; override;
  end;

type
  ///  <summary>Config file updater class for use with common config file.
  ///  </summary>
  ///  <remarks>Provides current common config file version.</remarks>
  TCommonConfigFileUpdater = class(TConfigFileUpdater)
  strict private
    const
      ///  <summary>Current common config file version.</summary>
      FileVersion = 7;
  strict protected
    ///  <summary>Returns current common config file version.</summary>
    class function GetFileVersion: Integer; override;
  public
    ///  <summary>Deletes any program registration information.</summary>
    procedure DeleteRedundantRegistrationInfo;
    ///  <summary>Stamps config file with current program and file versions.
    ///  </summary>
    ///  <remarks>Note that the user config file has program version written to
    ///  a different section to common config file, hence need for overridden
    ///  methods.</remarks>
    procedure Stamp; override;
  end;


implementation


uses
  // Delphi
  SysUtils, Types, IOUtils,
  // Project
  FirstRun.UIniFile, UAppInfo, UIOUtils, UIStringList, UStrUtils;


{ TConfigFileUpdater }

procedure TConfigFileUpdater.CopyANSIFile(const SrcFileName: string);
var
  Lines: TArray<string>;  // lines of text read from ANSI .ini file
begin
  if StrSameText(SrcFileName, fCfgFileName) then
    Exit;
  // reads an ANSI file and converts contents to Unicode, using system default
  // encoding
  Lines := TFileIO.ReadAllLines(SrcFileName, TEncoding.Default);
  // writes string array to UTF-16LE file
  ForceDirectories(ExtractFileDir(fCfgFileName));
  TFileIO.WriteAllLines(fCfgFileName, Lines, TEncoding.Unicode, True);
end;

procedure TConfigFileUpdater.CopyFile(const SrcFileName: string;
  const IsAnsi: Boolean);
begin
  if SrcFileName <> '' then
  begin
    if IsAnsi then
      CopyANSIFile(SrcFileName)
    else
      CopyUnicodeFile(SrcFileName);
  end;
end;

procedure TConfigFileUpdater.CopyUnicodeFile(const SrcFileName: string);
begin
  if StrSameText(SrcFileName, fCfgFileName) then
    Exit;
  ForceDirectories(ExtractFileDir(fCfgFileName));
  TFileIO.CopyFile(SrcFileName, fCfgFileName);
end;

constructor TConfigFileUpdater.Create(const CfgFileName: string);
begin
  inherited Create;
  fCfgFileName := CfgFileName;
end;

procedure TConfigFileUpdater.CreateNewFile;
begin
  ForceDirectories(ExtractFileDir(fCfgFileName));
  TFileIO.WriteAllText(fCfgFileName, '', TEncoding.Unicode, True);
end;

function TConfigFileUpdater.FileVer: Integer;
begin
  Result := GetIniInt('IniFile', 'Version', 1, fCfgFileName);
end;

function TConfigFileUpdater.IsCurrentProgramVer: Boolean;
begin
  Result := IsCurrentProgramVer(fCfgFileName);
end;

class function TConfigFileUpdater.IsCurrentProgramVer(
  const CfgFileName: string): Boolean;
var
  CfgProgVer: string;  // program version from config file
begin
  CfgProgVer := GetIniString('IniFile', 'ProgramVersion', '', CfgFileName);
  Result := CfgProgVer = TAppInfo.ProgramReleaseVersion;
end;

procedure TConfigFileUpdater.Stamp;
begin
  if not TFile.Exists(fCfgFileName, False) then
    CreateNewFile;
  SetIniInt('IniFile', 'Version', GetFileVersion, fCfgFileName);
end;

{ TUserConfigFileUpdater }

procedure TUserConfigFileUpdater.CreateDefaultCodeGenEntries;
begin
  if not TFile.Exists(CfgFileName, False) then
    CreateNewFile;
  SetIniInt('Prefs:CodeGen', 'EmitWarnDirs', 0, CfgFileName);
  SetIniInt('Prefs:CodeGen', 'WarningCount', 8, CfgFileName);
  // We don't set warning state: it defaults to required "off" value
  SetIniString(
    'Prefs:CodeGen', 'Warning0.Symbol', 'UNSAFE_TYPE', CfgFileName
  );
  SetIniString('Prefs:CodeGen', 'Warning0.MinCompiler', '15.00', CfgFileName);
  SetIniString(
    'Prefs:CodeGen', 'Warning1.Symbol', 'UNSAFE_CAST', CfgFileName
  );
  SetIniString('Prefs:CodeGen', 'Warning1.MinCompiler', '15.00', CfgFileName);
  SetIniString(
    'Prefs:CodeGen', 'Warning2.Symbol', 'UNSAFE_CODE', CfgFileName
  );
  SetIniString('Prefs:CodeGen', 'Warning2.MinCompiler', '15.00', CfgFileName);
  SetIniString(
    'Prefs:CodeGen', 'Warning3.Symbol', 'SYMBOL_PLATFORM', CfgFileName
  );
  SetIniString('Prefs:CodeGen','Warning3.MinCompiler', '14.00', CfgFileName);
  SetIniString(
    'Prefs:CodeGen', 'Warning4.Symbol', 'SYMBOL_DEPRECATED', CfgFileName
  );
  SetIniString('Prefs:CodeGen', 'Warning4.MinCompiler', '14.00', CfgFileName);
  SetIniString(
    'Prefs:CodeGen', 'Warning5.Symbol', 'SYMBOL_LIBRARY', CfgFileName
  );
  SetIniString('Prefs:CodeGen', 'Warning5.MinCompiler', '14.00', CfgFileName);
  SetIniString(
    'Prefs:CodeGen', 'Warning6.Symbol', 'IMPLICIT_STRING_CAST', CfgFileName
  );
  SetIniString('Prefs:CodeGen', 'Warning6.MinCompiler', '20.00', CfgFileName);
  SetIniString(
    'Prefs:CodeGen', 'Warning7.Symbol', 'EXPLICIT_STRING_CAST', CfgFileName
  );
  SetIniString('Prefs:CodeGen', 'Warning7.MinCompiler', '20.00', CfgFileName);
end;

procedure TUserConfigFileUpdater.DeleteDetailsPaneIndex;
begin
  if not TFile.Exists(CfgFileName, False) then
    Exit;
  DeleteIniKey('MainWindow', 'DetailTab', CfgFileName);
end;

procedure TUserConfigFileUpdater.DeleteHighligherPrefs;
begin
  if not TFile.Exists(CfgFileName, False) then
    Exit;
  DeleteIniSection('Prefs:Hiliter', CfgFileName);
end;

procedure TUserConfigFileUpdater.DeletePageStructureInfo;
var
  I: Integer;
begin
  if not TFile.Exists(CfgFileName, False) then
    Exit;
  for I := 0 to 5 do
    DeleteIniKey(
      'Prefs:SnippetPageStructure', Format('PageKind%d', [I]), CfgFileName
    );
end;

procedure TUserConfigFileUpdater.DeleteProxyPassword;
begin
  if not TFile.Exists(CfgFileName, False) then
    Exit;
  SetIniString('ProxyServer', 'Password', '', CfgFileName);
end;

procedure TUserConfigFileUpdater.DeleteRedundantDisplayOptions;
var
  I: Integer;
  ColourCount: Integer;
begin
  if not TFile.Exists(CfgFileName, False) then
    Exit;
  DeleteIniKey('Prefs:Display', 'MainDBHeadingColour', CfgFileName);
  DeleteIniKey('Prefs:Display', 'UserDBHeadingColour', CfgFileName);
  ColourCount := GetIniInt(
    'Prefs:Display', 'MainDBHeadingCustomColourCount', 0, CfgFileName
  );
  for I := 0 to Pred(ColourCount) do
    DeleteIniKey(
      'Prefs:Display', Format('MainDBHeadingCustomColour%d', [I]), CfgFileName
    );
  DeleteIniKey(
    'Prefs:Display', 'MainDBHeadingCustomColourCount', CfgFileName
  );
  ColourCount := GetIniInt(
    'Prefs:Display', 'UserDBHeadingCustomColourCount', 0, CfgFileName
  );
  for I := 0 to Pred(ColourCount) do
    DeleteIniKey(
      'Prefs:Display', Format('UserDBHeadingCustomColour%d', [I]), CfgFileName
    );
  DeleteIniKey(
    'Prefs:Display', 'UserDBHeadingCustomColourCount', CfgFileName
  );
end;

class function TUserConfigFileUpdater.GetFileVersion: Integer;
begin
  Result := FileVersion;
end;

function TUserConfigFileUpdater.HasPageStructureInfo: Boolean;
begin
  Result := IniKeyExists(
    'Prefs:SnippetPageStructure', 'PageKind0', CfgFileName
  );
end;

function TUserConfigFileUpdater.HasProxyPassword: Boolean;
begin
  Result := GetIniString('ProxyServer', 'Password', '', CfgFileName) <> '';
end;

function TUserConfigFileUpdater.HasV4CustomDatabaseDirectory: Boolean;
begin
  Result := GetIniString('Database', 'UserDataDir', '', CfgFileName) <> '';
end;

procedure TUserConfigFileUpdater.RenameCompilerIdentifiers;
type
  TNameChangeInfo = record
    OldName: string;
    NewName: string;
  end;
const
  NameChanges: array[1..3] of TNameChangeInfo = (
    (OldName: 'D2005w32'; NewName: 'D2005'),
    (OldName: 'D2006w32'; NewName: 'D2006'),
    (OldName: 'D2009w32'; NewName: 'D2009')
  );
var
  NameChangeInfo: TNameChangeInfo;
begin
  if not TFile.Exists(CfgFileName, False) then
    Exit;
  for NameChangeInfo in NameChanges do
  begin
    RenameIniSection(
      Format('Cmp:%s', [NameChangeInfo.OldName]),
      Format('Cmp:%s', [NameChangeInfo.NewName]),
      CfgFileName
    );
    RenameIniKey(
      'FindCompiler',
      NameChangeInfo.OldName,
      NameChangeInfo.NewName,
      CfgFileName
    );
  end;
end;

procedure TUserConfigFileUpdater.RenameMainWindowSection;
begin
  if not TFile.Exists(CfgFileName, False) then
    Exit;
  RenameIniSection('MainWindow', 'WindowState:MainForm', CfgFileName);
end;

procedure TUserConfigFileUpdater.RenameMainWindowSectionOverviewGrouping;
begin
  if not TFile.Exists(CfgFileName, False) then
    Exit;
  RenameIniKey(
    'WindowState:MainForm', 'OverviewTab', 'OverviewGrouping', CfgFileName
  );
end;

procedure TUserConfigFileUpdater.RenamePrintingSectionAndValues;
begin
  if not TFile.Exists(CfgFileName, False) then
    Exit;
  RenameIniSection('Prefs:Printing', 'Printing', CfgFileName);
  RenameIniKey('Printing', 'UseColor', 'UseColour', CfgFileName);
  RenameIniKey('Printing', 'SyntaxPrint', 'SyntaxHighlight', CfgFileName);
end;

procedure TUserConfigFileUpdater.ResetCustomDatabaseDirectory;
begin
  if not TFile.Exists(CfgFileName, False) then
    Exit;
  DeleteIniKey('Database', 'UserDataDir', CfgFileName)
end;

procedure TUserConfigFileUpdater.Stamp;
begin
  inherited;  // creates ini file if it doesn't exist
  SetIniString(
    'IniFile', 'ProgramVersion', TAppInfo.ProgramReleaseVersion, CfgFileName
  );
end;

procedure TUserConfigFileUpdater.UpdateCodeGenEntries;
begin
  // Key that determines if warnings are emitted changes from SwitchOffWarnings
  // to EmitWarnDirs.
  if not TFile.Exists(CfgFileName, False) then
    CreateNewFile;
  if not RenameIniKey(
    'Prefs:CodeGen', 'SwitchOffWarnings', 'EmitWarnDirs', CfgFileName
  ) then
    SetIniInt('Prefs:CodeGen', 'EmitWarnDirs', 0, CfgFileName);
end;

procedure TUserConfigFileUpdater.UpdateFindXRefs;
begin
  // From file ver 14, "IncludeRoutine" in FindXRefs was renamed as
  // "IncludeSnippet"
  if not TFile.Exists(CfgFileName, False) then
    CreateNewFile;
  if not RenameIniKey(
    'FindXRefs', 'IncludeRoutine', 'IncludeSnippet', CfgFileName
  ) then
    SetIniInt('FindXRefs', 'IncludeSnippet', 1, CfgFileName);
end;

procedure TUserConfigFileUpdater.UpdateFromOriginal;
var
  I: Integer; // loops thru all highlight elements
begin
  if not TFile.Exists(CfgFileName, False) then
    CreateNewFile
  else
  begin
    // Delete unwanted sections in existing config file:
    // - Application section: now in common config file
    // - Source code output format: format lost when updating from CodeSnip pre
    //   1.7 since section was SourceOutput, but format preserved from v1.7
    //   since current Prefs:SourceCode section used
    // - Highlighting style is deliberately lost since CodeSnip v3 & v4 have
    //   different default style and main display uses that style, therefore
    //   section's HiliteOutput (pre v1.7.5) and Prefs:Hiliter (v1.7.5 and
    //   later) deleted.
    DeleteIniSection('Application', CfgFileName);
    DeleteIniSection('SourceOutput', CfgFileName);
    DeleteIniSection('HiliteOutput', CfgFileName);
    for I := 0 to 11 do
      DeleteIniSection('HiliteOutput:Elem' + IntToStr(I), CfgFileName);
    DeleteHighligherPrefs;
  end;
  // Main window's overview tabs changed at v3: so we reset to 0 (default)
  SetIniInt('MainWindow', 'OverviewTab', 0, CfgFileName);
end;

procedure TUserConfigFileUpdater.UpdateNamespaces;

  // Extracts any namespace information from compiler's switches and moves it
  // into Namespaces value.
  procedure UpdateForCompiler(const CompilerID: string);
  var
    Section: string;
    Switches: IStringList;
    SwitchIdx: Integer;
    Switch: string;
    Namespaces: IStringList;
  begin
    Section := Format('Cmp:%s', [CompilerID]);
    Namespaces := TIStringList.Create;
    Switches := TIStringList.Create(
      GetIniString(Section, 'Switches', '', CfgFileName), ',', False, True
    );
    if not IniKeyExists(Section, 'Switches', CfgFileName) then
      Exit;
    for SwitchIdx := Pred(Switches.Count) downto 0 do
    begin
      Switch := Switches[SwitchIdx];
      if StrStartsStr('-NS', Switch) then
      begin
        Namespaces.Add(
          StrSliceRight(Switch, Length(Switch) - Length('-NS')),
          ';',
          False,
          True
        );
        Switches.Delete(SwitchIdx);
      end;
    end;
    SetIniString(
      Section, 'Switches', Switches.GetText(',', False), CfgFileName
    );
    if not Namespaces.IsEmpty then
      SetIniString(
        Section, 'Namespaces', Namespaces.GetText(' ', False), CfgFileName
      );
  end;

begin
  if not TFile.Exists(CfgFileName, False) then
    Exit;
  UpdateForCompiler('DXE2');
  UpdateForCompiler('DXE3');
  UpdateForCompiler('DXE4');
  UpdateForCompiler('DXE5');
  UpdateForCompiler('DXE6');
  UpdateForCompiler('DXE7');
  UpdateForCompiler('DXE8');
  UpdateForCompiler('D10S');
end;

{ TCommonConfigFileUpdater }

procedure TCommonConfigFileUpdater.DeleteRedundantRegistrationInfo;
begin
  if not TFile.Exists(CfgFileName) then
    Exit;
  DeleteIniKey('Application', 'RegCode', CfgFileName);
  DeleteIniKey('Application', 'RegName', CfgFileName);
end;

class function TCommonConfigFileUpdater.GetFileVersion: Integer;
begin
  Result := FileVersion;
end;

procedure TCommonConfigFileUpdater.Stamp;
begin
  inherited;  // creates ini file if it doesn't exist
  SetIniString(
    'Application', 'Version', TAppInfo.ProgramReleaseVersion, CfgFileName
  );
end;

end.

