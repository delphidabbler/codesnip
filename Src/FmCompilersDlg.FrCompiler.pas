{
 * FmCompilersDlg.FrCompiler.pas
 *
 * Implements a frame used to edit executable file name of compiler being edited
 * in TCompilersDlg.
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
 * The Original Code is FmCompilersDlg.FrCompiler.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2011 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
}


unit FmCompilersDlg.FrCompiler;


interface


uses
  // Delphi
  StdCtrls, Controls, Classes,
  // Project
  FmCompilersDlg.FrBase;


type
  ///  <summary>
  ///  Frame used to edit executable file name of compiler being edited in
  ///  TCompilersDlg.
  ///  </summary>
  TCompilersDlgCompilerFrame = class(TCompilersDlgBaseFrame)
    lblCompilerPath: TLabel;
    edCompilerPath: TEdit;
    btnBrowse: TButton;
    btnClear: TButton;
    ///  <summary>Displays file open dialog box and places entered file name in
    ///  compiler file name edit control.</summary>
    procedure btnBrowseClick(Sender: TObject);
    ///  <summary>Clears compiler file name edit control.</summary>
    procedure btnClearClick(Sender: TObject);
    ///  <summary>Triggers OnChange event if contents of edit control changed.
    ///  </summary>
    procedure edCompilerPathExit(Sender: TObject);
  strict private
    ///  <summary>Gets compiler path from edit control.</summary>
    function GetCompilerPath: string;
    ///  <summary>Handles open dialog box's OnCanClose event. Prevents dialog
    ///  from closing if selected file does not exist or is not a suitable
    ///  executable file.</summary>
    ///  <param name="Sender">TObject [in] Reference to dialog box that
    ///  triggered event. Must be of type TOpenDialogEx.</param>
    ///  <param name="CanClose">Boolean [in/out] Flag that determines if dialog
    ///  can close.</param>
    procedure CanOpenDialogClose(Sender: TObject;
      var CanClose: Boolean);
    ///  <summary>Checks if given file name is a valid executable.</summary>
    ///  <param name="FileName">string [in] File name to check.</param>
    ///  <param name="ErrMsg">string [out] Error message if file name is not
    ///  valid. Undefined otherwise.</param>
    ///  <returns>Boolean. True if FileName is valid, False if not.</returns>
    function ValidateFileName(const FileName: string; out ErrMsg: string):
      Boolean;
  strict protected
    ///  <summary>Initialises frame to display details of current compiler.
    ///  </summary>
    procedure Initialise; override;
  public
    ///  <summary>Arranges controls in frame.</summary>
    procedure ArrangeControls; override;
    ///  <summary>Updates current compiler object with edited information.
    ///  </summary>
    procedure UpdateCompiler; override;
  end;


implementation


uses
  // Delphi
  Sysutils, Dialogs,
  // Project
  UCtrlArranger, UExeFileType, UMessageBox, UOpenDialogEx, UOpenDialogHelper,
  UStrUtils;


{$R *.dfm}

{ TCompilersDlgCompilerFrame }

procedure TCompilersDlgCompilerFrame.ArrangeControls;
begin
  TCtrlArranger.SetLabelHeights(Self);
  TCtrlArranger.AlignVCentres(
    TCtrlArranger.BottomOf(lblCompilerPath, 4), [edCompilerPath, btnBrowse]
  );
  btnClear.Top := TCtrlArranger.BottomOf([edCompilerPath, btnBrowse], 8);
end;

procedure TCompilersDlgCompilerFrame.btnBrowseClick(Sender: TObject);
var
  OpenDlg: TOpenDialogEx; // self-aligning enhanced open dialog box
resourcestring
  sFilter = 'Executable files (*.exe)|*.exe|' // file filter
    + 'All files (*.*)|*.*';
  sTitle = 'Select Compiler';                 // dialog box title
begin
  // Create and initialise
  OpenDlg := TOpenDialogEx.Create(Self);
  try
    OpenDlg.OnCanClose := CanOpenDialogClose;
    OpenDlg.Filter := sFilter;
    OpenDlg.FilterIndex := 1;
    OpenDlg.InitialDir := '';
    // we don't include ofFileMustExist in Options below since we handle
    // non-existant files ourselves
    // we don't include ofShowHelp since the dialog box automatically displays
    // help if HelpKeyword property is set.
    OpenDlg.Options := [ofHideReadOnly, ofEnableSizing];
    OpenDlg.OptionsEx := [];
    OpenDlg.Title := sTitle;
    OpenDlg.HelpKeyword := 'SelectCompilerDlg';
    // if we have a compiler path use it as default if it exists
    if FileExists(GetCompilerPath) then
      OpenDlg.FileName := GetCompilerPath;
    if OpenDlg.Execute then
    begin
      // User OKd: use entered file name
      edCompilerPath.Text := OpenDlg.FileName;
      DoChange;
    end;
  finally
    OpenDlg.Free;
  end;
end;

procedure TCompilersDlgCompilerFrame.CanOpenDialogClose(Sender: TObject;
  var CanClose: Boolean);
var
  Dlg: TOpenDialogEx; // dialog box instance triggering this event
  ErrMsg: string;     // error message to be displayed on error
begin
  Dlg := Sender as TOpenDialogEx;
  CanClose := ValidateFileName(FileOpenEditedFileName(Dlg), ErrMsg);
  if not CanClose then
    TMessageBox.Error(Dlg, ErrMsg);
end;

procedure TCompilersDlgCompilerFrame.edCompilerPathExit(Sender: TObject);
begin
  if Compiler.GetExecFile <> GetCompilerPath then
    DoChange;
end;

function TCompilersDlgCompilerFrame.GetCompilerPath: string;
begin
  Result := StrTrim(edCompilerPath.Text);
end;

procedure TCompilersDlgCompilerFrame.Initialise;
begin
  edCompilerPath.Text := Compiler.GetExecFile;
end;

procedure TCompilersDlgCompilerFrame.UpdateCompiler;
begin
  Compiler.SetExecFile(GetCompilerPath);
end;

function TCompilersDlgCompilerFrame.ValidateFileName(const FileName: string;
  out ErrMsg: string): Boolean;
resourcestring
  // Error messages
  sFileDoesNotExist = 'File does not exist.';
  sFileNotExe = 'File is not executable.';
begin
  if not FileExists(FileName) then
  begin
    ErrMsg := sFileDoesNotExist;
    Exit(False);
  end;
  if ExeFileType(FileName) <> fkExe32 then
  begin
    ErrMsg := sFileNotExe;
    Exit(False);
  end;
  Result := True;
end;

procedure TCompilersDlgCompilerFrame.btnClearClick(Sender: TObject);
begin
  edCompilerPath.Text := '';
  DoChange;
end;

end.

