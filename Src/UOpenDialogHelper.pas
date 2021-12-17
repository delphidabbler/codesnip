{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2008-2020, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Helper routines for use when working with standard windows open and save file
 * dialog boxes.
}


unit UOpenDialogHelper;


interface


uses
  // Delphi
  Dialogs;


function FileOpenEditedFileName(const Dlg: TOpenDialog): string;
  {Gets full path to the file that is currently entered in a file open dialog
  box.
    @param Dlg [in] Dialog box for which file name is required.
    @return Required file path.
  }

function FileOpenFileNameWithExt(Dlg: TOpenDialog): string;
  {Ensures a file name entered in a dialog box has a default extension if none
  provided.
    @param Dlg [in] Dialog box containing required file name and extension info.
    @return File name, with default extension if (1) file name has no extension,
      (2) current filter is not *.* (3) dialog has a filter string.
  }

function FilterIndexToExt(const Dlg: TOpenDialog): string;
  {Extracts extension associated with a standard file open dialog's selected
  file type from its "|" delimited Filter property.
    @param Dlg [in] Dialog box for which current extension is to be extracted.
    @return Extension specified by dialog's FilterIndex property with a
      prepended '.'.
  }

function ExtToFilterIndex(const FilterStr, Ext: string;
  const DefValue: Integer): Integer;
  {Calculates index of a file extension in a "|" delimited file filter string as
  used in standard file dialog boxes.
    @param FilterStr [in] List of file types and extensions. Has format
      "file desc 1|ext 1|file desc 2|ext 2 etc...".
    @param Ext [in] Extension to be found.
    @param DefValue [in] Default 1 based index to use if Ext is not in
      FilterStr.
    @return 1 based index of extension in filter string or -1 if extension not
      in list.
  }

function FileOpenEditedFileNameWithExt(const Dlg: TOpenDialog): string;
  {Gets full path to the file that is currently entered in a file open dialog
  box, with default extension added if necessary.
    @param Dlg [in] Dialog box for which file name is required.
    @return Required file path.
  }


implementation


uses
  // Delphi
  SysUtils, Classes, Windows, Dlgs, CommDlg,
  // Project
  UStrUtils, UUtils;


function FilterIndexToExt(const Dlg: TOpenDialog): string;
  {Extracts extension associated with a standard file open dialog's selected
  file type from its "|" delimited Filter property.
    @param Dlg [in] Dialog box for which current extension is to be extracted.
    @return Extension specified by dialog's FilterIndex property with a
      prepended '.'.
  }
var
  FilterParts: TStringList; // stores filter split into component parts
begin
  FilterParts := TStringList.Create;
  try
    // Split filter string into parts (divided by | chars):
    // even number indexes are descriptions and odd indexes are extensions
    StrExplode(Dlg.Filter, '|', FilterParts);
    Result := ExtractFileExt(FilterParts[2 * (Dlg.FilterIndex - 1) + 1]);
  finally
    FreeAndNil(FilterParts);
  end;
end;

function ExtToFilterIndex(const FilterStr, Ext: string;
  const DefValue: Integer): Integer;
  {Calculates index of a file extension in a "|" delimited file filter string as
  used in standard file dialog boxes.
    @param FilterStr [in] List of file types and extensions. Has format
      "file desc 1|ext 1|file desc 2|ext 2 etc...".
    @param Ext [in] Extension to be found.
    @param DefValue [in] Default 1 based index to use if Ext is not in
      FilterStr.
    @return 1 based index of extension in filter string or -1 if extension not
      in list.
  }
var
  FilterParts: TStringList; // stores filter split into component parts
  Extensions: TStringList;  // list of extensions in filter string
  Idx: Integer;             // loops thru extensions in filter string
begin
  Extensions := nil;
  FilterParts := TStringList.Create;
  try
    // Split filter string into parts (divided by | chars):
    // even number indexes are descriptions and odd indexes are extensions
    StrExplode(FilterStr, '|', FilterParts);
    // Record only extensions (every 2nd entry starting at index 1)
    Extensions := TStringList.Create;
    Idx := 1;
    while Idx < FilterParts.Count do
    begin
      Extensions.Add(ExtractFileExt(FilterParts[Idx]));
      Inc(Idx, 2);
    end;
    // Check if required extension in list
    Result := Extensions.IndexOf(Ext);
    if Result >= 0 then
      // extension in list, increment by 1 since filter indexes are 1 based
      Inc(Result)
    else
      Result := DefValue;
  finally
    FreeAndNil(Extensions);
    FreeAndNil(FilterParts);
  end;
end;

function FileOpenFileNameWithExt(Dlg: TOpenDialog): string;
  {Ensures a file name entered in a dialog box has a default extension if none
  provided.
    @param Dlg [in] Dialog box containing required file name and extension info.
    @return File name, with default extension if (1) file name has no extension,
      (2) current filter is not *.* (3) dialog has a filter string.
  }
var
  DefaultExt: string; // default extension for current filter
begin
  Result := Dlg.FileName;
  if (ExtractFileExt(Dlg.FileName) = '') and (Dlg.Filter <> '') then
  begin
    DefaultExt := FilterIndexToExt(Dlg);
    if not StrContainsStr('*', DefaultExt) then
      Result := Result + DefaultExt;
  end;
end;

function FileOpenEditText(const Dlg: TOpenDialog): string;
  {Gets text from file edit control in a file open dialog box.
    @param Dlg [in] Reference to dialog box for which text is required.
    @return Required text.
  }
var
  DlgItemWnd: HWND;                 // handle of edit control window in dlg box
  Text: array[0..MAX_PATH] of Char; // buffer to receive edit control text
begin
  Result := '';
  // File name entered by user or selected in dialog is displayed either in
  // edit control edt1 or in combo box cmb13. We try to get text first from
  // edt1 and if that fails, from cmb13. If we fail to find both we bail out.
  DlgItemWnd := GetDlgItem(GetParent(Dlg.Handle), edt1);
  if DlgItemWnd = 0 then
    DlgItemWnd := GetDlgItem(GetParent(Dlg.Handle), cmb13);
  if DlgItemWnd = 0 then
    Exit;
  // Get text from found control
  GetWindowText(DlgItemWnd, Text, SizeOf(Text));
  Result := Text;
end;

function FileOpenFolderPath(const Dlg: TOpenDialog): string;
  {Gets name of currently selected folder in a file open dialog box.
    @param Dlg [in] Reference to dialog box for which folder is required.
    @return Path to folder.
  }
var
  Folder: array[0..MAX_PATH] of Char; // receives path to folder
begin
  SendMessage(
    GetParent(Dlg.Handle), CDM_GETFOLDERPATH, SizeOf(Folder), Integer(@Folder)
  );
  Result := Folder;
end;

function FileOpenEditedFileName(const Dlg: TOpenDialog): string;
  {Gets full path to the file that is currently entered in a file open dialog
  box.
    @param Dlg [in] Dialog box for which file name is required.
    @return Required file path.
  }
begin
  Result := FileOpenEditText(Dlg);
  if Result = '' then
    Exit;
  if IsBaseFileName(Result) then
    Result := IncludeTrailingPathDelimiter(FileOpenFolderPath(Dlg)) + Result;
end;

function FileOpenEditedFileNameWithExt(const Dlg: TOpenDialog): string;
  {Gets full path to the file that is currently entered in a file open dialog
  box, with default extension added if necessary.
    @param Dlg [in] Dialog box containing required file name and extension info.
    @return File name, with default extension if (1) file name has no extension,
      (2) current filter is not *.* (3) dialog has a filter string.
  }
var
  DefaultExt: string; // default extension for current filter
begin
  Result := FileOpenEditedFileName(Dlg);
  if Result = '' then
    Exit;
  if (ExtractFileExt(Result) = '') and (Dlg.Filter <> '') then
  begin
    DefaultExt := FilterIndexToExt(Dlg);
    if not StrContainsStr('*', DefaultExt) then
      Result := Result + DefaultExt;
  end;
end;

end.
