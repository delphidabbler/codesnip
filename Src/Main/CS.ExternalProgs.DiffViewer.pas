{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2013, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * Implements a class to manage specification and execution of a user defined
 * file diff viewer application.
}



unit CS.ExternalProgs.DiffViewer;


interface


type
  TDiffViewer = class(TObject)
  strict private
    type
      TTestFiles = record
      public
        type
          TTestFileID = (tfiLeft, tfiRight);
      public
        class function Name(FileID: TTestFileID): string; static;
        class procedure Create; static;
        class procedure Delete; static;
      end;
    var
      fExePath: string;
      fParams: string;
      fChanged: Boolean;
    procedure SetExePath(const AExePath: string);
    procedure SetParams(const AParams: string);
    class function ParamStr(const Tplt, FileName1, FileName2: string): string;
    procedure LoadProps;
    procedure SaveProps;
    class function InternalExecute(const AExePath, AParams, AFileName1,
      AFileName2: string): Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    function Execute(const AFileName1, AFileName2: string): Boolean;
    function Test(const AExePath, AParams: string): Boolean;
    property ExePath: string read fExePath write SetExePath;
    property Params: string read fParams write SetParams;
  end;


implementation


uses
  // Delphi
  SysUtils,
  IOUtils,
  Windows,
  ShellAPI,
  // Project
  UConsts,
  UIOUtils,
  USettings,
  UStrUtils,
  USystemInfo;


{ TDiffViewer }

constructor TDiffViewer.Create;
begin
  inherited Create;
  LoadProps;
end;

destructor TDiffViewer.Destroy;
begin
  if fChanged then
    SaveProps;
  TTestFiles.Delete;
  inherited;
end;

function TDiffViewer.Execute(const AFileName1, AFileName2: string): Boolean;
begin
  Result := InternalExecute(fExePath, fParams, AFileName1, AFileName2);
end;

class function TDiffViewer.InternalExecute(const AExePath, AParams, AFileName1,
  AFileName2: string): Boolean;
begin
  if StrIsBlank(AFileName1) or StrIsBlank(AFileName2) then
    Exit(False);
  if not TFile.Exists(AFileName1, False)
    or not TFile.Exists(AFileName2, False) then
    Exit(False);
  if (AExePath = EmptyStr) or (AParams = EmptyStr) then
    Exit(False);
  if not TFile.Exists(AExePath, False) then
    Exit(False);
  Result := ShellExecute(
    0,
    'open',
    PChar(StrQuoteSpaced(AExePath)),
    PChar(ParamStr(AParams, AFileName1, AFileName2)),
    PChar(TSystemFolders.Temp),
    SW_SHOWNORMAL
  ) > 32;
end;

procedure TDiffViewer.LoadProps;
var
  Storage: ISettingsSection;
begin
  Storage := Settings.ReadSection(ssExternalApps);
  fExePath := Storage.GetString('DiffViewerPath');
  fParams := Storage.GetString('DiffViewerParams');
end;

class function TDiffViewer.ParamStr(const Tplt, FileName1, FileName2: string):
  string;
begin
  Result := StrReplace(StrReplace(Tplt, '%1', FileName1), '%2', FileName2);
end;

procedure TDiffViewer.SaveProps;
var
  Storage: ISettingsSection;
begin
  Storage := Settings.EmptySection(ssExternalApps);
  Storage.SetString('DiffViewerPath', fExePath);
  Storage.SetString('DiffViewerParams', StrQuote(fParams));
  Storage.Save;
end;

procedure TDiffViewer.SetExePath(const AExePath: string);
begin
  if StrSameText(AExePath, fExePath) then
    Exit;
  fExePath := StrTrim(AExePath);
  fChanged := True;
end;

procedure TDiffViewer.SetParams(const AParams: string);
begin
  if StrSameText(AParams, fParams) then
    Exit;
  fParams := StrTrim(AParams);
  fChanged := True;
end;

function TDiffViewer.Test(const AExePath, AParams: string): Boolean;
begin
  TTestFiles.Create;
  Result := InternalExecute(
    AExePath, AParams, TTestFiles.Name(tfiLeft), TTestFiles.Name(tfiRight)
  );
end;

{ TDiffViewer.TTestFiles }

class procedure TDiffViewer.TTestFiles.Create;
resourcestring
  sLeftContents = 'One' + EOL + 'Three' + EOL + 'Five' + EOL + 'Seven';
  sRightContents = 'One' + EOL + 'Five' + EOL + 'Six' + EOL + 'Seven';
const
  Contents: array[TTestFileID] of string = (sLeftContents, sRightContents);
var
  FileID: TTestFileID;
begin
  for FileID := Low(TTestFileID) to High(TTestFileID) do
    TFileIO.WriteAllText(Name(FileID), Contents[FileID], TEncoding.Default);
end;

class procedure TDiffViewer.TTestFiles.Delete;
var
  FileID: TTestFileID;
begin
  for FileID := Low(TTestFileID) to High(TTestFileID) do
    TFile.Delete(Name(FileID));
end;

class function TDiffViewer.TTestFiles.Name(FileID: TTestFileID): string;
const
  Stubs: array[TTestFileID] of string = ('Left', 'Right');
begin
    Result := IncludeTrailingPathDelimiter(TSystemFolders.Temp) +
      'CodeSnip-Diff-Test-' + Stubs[FileID] + '.txt';
end;

end.
