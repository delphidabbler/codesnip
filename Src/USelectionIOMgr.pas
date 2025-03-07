{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2012-2021, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Provides a static object that manages loading and saving of stored
 * database selections.
}


unit USelectionIOMgr;


interface


uses
  // Project
  UBaseObjects, USearch;


type

  TSelectionIOMgr = class sealed(TNoConstructObject)
  strict private
    class procedure CanOpenDialogClose(Sender: TObject;
      var CanClose: Boolean);
    class procedure CanSaveDialogClose(Sender: TObject;
      var CanClose: Boolean);
    class function GetLoadFileName(out FileName: string): Boolean;
    class function GetSaveFileName(out FileName: string): Boolean;
  public
    class procedure SaveCurrentSelection;
    class function CanSaveCurrentSelection: Boolean;
    class function LoadSelectionSearch(out Search: ISearch): Boolean;
  end;


implementation


uses
  // Delphi
  SysUtils,
  Dialogs,
  // Project
  DB.SnippetIDs,
  DB.USnippet,
  UConsts,
  UMessageBox,
  UOpenDialogEx,
  UOpenDialogHelper,
  UQuery,
  USaveDialogEx,
  USnippetIDListIOHandler;

const
  ///  <summary>Watermark for selection files. Uses characters that will be
  ///  interpreted wrongly if the file is not in UTF8 format.</summary>
  SelectionFileWatermark = #$25BA + ' CodeSnip Selections v2 ' + #$25C4;

{ TSelectionIOMgr }

class procedure TSelectionIOMgr.CanOpenDialogClose(Sender: TObject;
  var CanClose: Boolean);
var
  Dlg: TOpenDialogEx; // dialog box instance triggering this event
  FileSpec: string;   // full path to entered or selected file name
resourcestring
  // Error messages
  sFileDoesNotExist = '"%s" does not exist.';
begin
  Dlg := Sender as TOpenDialogEx;
  FileSpec := FileOpenEditedFileName(Dlg);
  CanClose := FileExists(FileSpec);
  if not CanClose then
    TMessageBox.Error(Dlg, Format(sFileDoesNotExist, [FileSpec]));
end;

class function TSelectionIOMgr.CanSaveCurrentSelection: Boolean;
begin
  Result := Query.IsSearchActive and not Query.Selection.IsEmpty;
end;

class procedure TSelectionIOMgr.CanSaveDialogClose(Sender: TObject;
  var CanClose: Boolean);
var
  Dlg: TSaveDialogEx; // dialog box instance triggering this event
  FileSpec: string;   // full path to entered or selected file name

  function QueryOverwrite: Boolean;
  resourcestring
    // Text of query displayed in dialog box
    sQueryMsg = '%s already exists.' + EOL + 'Do you want to replace it?';
  begin
    Result := TMessageBox.Confirm(Dlg, Format(sQueryMsg, [FileSpec]));
  end;

begin
  Dlg := Sender as TSaveDialogEx;
  FileSpec := FileOpenEditedFileNameWithExt(Dlg);
  CanClose := not FileExists(FileSpec) or QueryOverwrite;
end;

class function TSelectionIOMgr.GetLoadFileName(out FileName: string): Boolean;
resourcestring
  sDlgTitle = 'Load Selection'; // dialogue box title
  sFilter = 'CodeSnip selection files (*.cssel)|*.cssel|'
    + 'All files (*.*)|*.*';    // file filter
var
  OpenDlg: TOpenDialogEx; // load selection dialogue box
begin
  OpenDlg := TOpenDialogEx.Create(nil);
  try
    OpenDlg.OnCanClose := CanOpenDialogClose;
    OpenDlg.Filter := sFilter;
    OpenDlg.FilterIndex := 1;
    OpenDlg.InitialDir := '';
    // we don't include ofFileMustExist in Options below since we handle
    // non-existant files ourselves
    OpenDlg.Options := [ofHideReadOnly, ofEnableSizing];
    OpenDlg.OptionsEx := [];
    OpenDlg.Title := sDlgTitle;
    OpenDlg.HelpKeyword := 'LoadSelectionDlg';
    Result := OpenDlg.Execute;
    if Result then
      FileName := OpenDlg.FileName;
  finally
    OpenDlg.Free;
  end;
end;

class function TSelectionIOMgr.GetSaveFileName(out FileName: string): Boolean;
resourcestring
  sDlgTitle = 'Save Selection'; // dialogue box caption
  sFilter = 'CodeSnip selection files (*.cssel)|*.cssel|'
    + 'All files (*.*)|*.*';    // file filter
var
  SaveDlg: TSaveDialogEx; // save selection dialogue box
begin
  SaveDlg := TSaveDialogEx.Create(nil);
  try
    SaveDlg.Title := sDlgTitle;
    SaveDlg.Options := [ofShowHelp, ofNoTestFileCreate, ofEnableSizing];
    SaveDlg.Filter := sFilter;
    SaveDlg.FilterIndex := 1;
    SaveDlg.HelpKeyword := 'SaveSelectionDlg';
    SaveDlg.OnCanClose := CanSaveDialogClose;
    Result := SaveDlg.Execute;
    if Result then
      FileName := FileOpenFileNameWithExt(SaveDlg);
  finally
    SaveDlg.Free;
  end;
end;

class function TSelectionIOMgr.LoadSelectionSearch(out Search: ISearch):
  Boolean;
var
  FileName: string;
  Reader: TSnippetIDListFileReader;
  SnippetIDs: ISnippetIDList;
  Filter: ISelectionSearchFilter;
begin
  if not GetLoadFileName(FileName) then
    Exit(False);
  Reader := TSnippetIDListFileReader.Create(SelectionFileWatermark);
  try
    SnippetIDs := Reader.ReadFile(FileName);
  finally
    Reader.Free;
  end;
  Filter := TSearchFilterFactory.CreateStoredSelectionSearchFilter(
    SnippetIDs
  );
  Search := TSearchFactory.CreateSearch(Filter);
  Result := True;
end;

class procedure TSelectionIOMgr.SaveCurrentSelection;
var
  FileName: string;
  Writer: TSnippetIDListFileWriter;
  SnippetIDs: ISnippetIDList;
  Snippet: TSnippet;
begin
  if not GetSaveFileName(FileName) then
    Exit;
  Writer := TSnippetIDListFileWriter.Create(SelectionFileWatermark);
  try
    SnippetIDs := TSnippetIDList.Create;
    for Snippet in Query.Selection do
      SnippetIDs.Add(Snippet.ID);
    Writer.WriteFile(FileName, SnippetIDs);
  finally
    Writer.Free;
  end;
end;

end.

