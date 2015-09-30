{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2011-2014, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * Implements a frame used to edit executable file name of compiler being edited
 * in TCompilersDlg.
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
    chkShowInMain: TCheckBox;
    ///  <summary>Displays file open dialogue box and places entered file name
    ///  in compiler file name edit control.</summary>
    procedure btnBrowseClick(Sender: TObject);
    ///  <summary>Clears compiler file name edit control.</summary>
    procedure btnClearClick(Sender: TObject);
    ///  <summary>Triggers OnChange event if contents of edit control changed.
    ///  </summary>
    procedure edCompilerPathExit(Sender: TObject);
  strict private
    ///  <summary>Gets compiler path from edit control.</summary>
    function GetCompilerPath: string;
    ///  <summary>Handles open dialogue box's OnCanClose event. Prevents
    ///  dialogue from closing if selected file does not exist or is not a
    ///  suitable executable file.</summary>
    ///  <param name="Sender">TObject [in] Reference to dialogue box that
    ///  triggered event. Must be of type TOpenDialogEx.</param>
    ///  <param name="CanClose">Boolean [in/out] Flag that determines if
    ///  dialogue can close.</param>
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
  Compilers.UGlobals,
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
  chkShowInMain.Top := TCtrlArranger.BottomOf(btnClear, 24);
end;

procedure TCompilersDlgCompilerFrame.btnBrowseClick(Sender: TObject);
var
  OpenDlg: TOpenDialogEx; // self-aligning enhanced open dialogue box
resourcestring
  sFilter = 'Executable files (*.exe)|*.exe|' // file filter
    + 'All files (*.*)|*.*';
  sTitle = 'Select Compiler';                 // dialogue box title
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
    // we don't include ofShowHelp since the dialogue box automatically displays
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
  Dlg: TOpenDialogEx; // dialogue box instance triggering this event
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
  chkShowInMain.Checked := Compiler.GetDisplayable;
end;

procedure TCompilersDlgCompilerFrame.UpdateCompiler;
begin
  Compiler.SetExecFile(GetCompilerPath);
  Compiler.SetDisplayable(chkShowInMain.Checked);
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

