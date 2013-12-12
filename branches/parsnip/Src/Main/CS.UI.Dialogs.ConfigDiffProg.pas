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
 * Implements a dialogue box that permits the user to specify a file diff viewer
 * application for use by CodeSnip.
}


unit CS.UI.Dialogs.ConfigDiffProg;


interface


uses
  // Delphi
  Classes,
  ActnList,
  StdCtrls,
  Controls,
  ExtCtrls,
  // Project
  CS.ExternalProgs.DiffViewer,
  FmGenericOKDlg,
  UBaseObjects;


type
  TConfigDiffProgDlg = class(TGenericOKDlg, INoPublicConstruct)
    lblProgFile: TLabel;
    edProgFile: TEdit;
    btnBrowse: TButton;
    lblParams: TLabel;
    edParams: TEdit;
    btnTest: TButton;
    btnClear: TButton;
    alDlg: TActionList;
    actClear: TAction;
    actTest: TAction;
    lblParamsHelp: TLabel;
    procedure btnBrowseClick(Sender: TObject);
    procedure actTestUpdate(Sender: TObject);
    procedure actClearUpdate(Sender: TObject);
    procedure btnClearClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure actTestExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure actClearExecute(Sender: TObject);
  strict private
    var
      fDiffViewer: TDiffViewer;
    function GetProgFile: string;
    function GetParams: string;
    function ValidateExeFileName(const FileName: string; out ErrMsg: string):
      Boolean;
    function IsValidExeFileName(const FileName: string): Boolean;
    procedure CanOpenDialogClose(Sender: TObject; var CanClose: Boolean);
  strict protected
    procedure ArrangeForm; override;
    procedure InitForm; override;
  public
    class function Execute(AOwner: TComponent): Boolean;
  end;


implementation


uses
  // Delphi
  SysUtils, // for inlining
  Dialogs,
  IOUtils,
  UCtrlArranger,
  UExeFileType,
  UMessageBox,
  UOpenDialogEx,
  UOpenDialogHelper,
  UStrUtils,
  USystemInfo;

{$R *.dfm}

{ TConfigDiffProgDlg}

procedure TConfigDiffProgDlg.actClearExecute(Sender: TObject);
begin
  edProgFile.Clear;
  edParams.Clear;
end;

procedure TConfigDiffProgDlg.actClearUpdate(Sender: TObject);
begin
  actClear.Enabled := not(StrIsBlank(GetProgFile) and StrIsBlank(GetParams));
end;

procedure TConfigDiffProgDlg.actTestExecute(Sender: TObject);
begin
  fDiffViewer.Test(GetProgFile, GetParams);
end;

procedure TConfigDiffProgDlg.actTestUpdate(Sender: TObject);
begin
  actTest.Enabled := not StrIsBlank(GetProgFile)
    and IsValidExeFileName(StrTrim(edProgFile.Text))
    and not StrIsBlank(GetParams);
end;

procedure TConfigDiffProgDlg.ArrangeForm;
begin
  TCtrlArranger.AlignLefts(
    [lblProgFile, edProgFile, lblParams, edParams, lblParamsHelp, btnTest],
    0
  );
  TCtrlArranger.MoveToRightOf(btnTest, btnClear, 12);
  TCtrlArranger.AlignRights([btnBrowse], pnlBody.ClientWidth);
  TCtrlArranger.StretchRightTo(edProgFile, btnBrowse.Left - 8);
  edParams.Width := edProgFile.Width;
  lblProgFile.Top := 0;
  TCtrlArranger.AlignVCentres(
    TCtrlArranger.BottomOf(lblProgFile, 6), [edProgFile, btnBrowse]
  );
  TCtrlArranger.MoveBelow([edProgFile, btnBrowse], lblParams, 12);
  TCtrlArranger.MoveBelow(lblParams, edParams, 6);
  TCtrlArranger.MoveBelow(edParams, lblParamsHelp, 6);
  TCtrlArranger.AlignVCentres(
    TCtrlArranger.BottomOf(lblParamsHelp, 18), [btnTest, btnClear]
  );
  pnlBody.ClientHeight := TCtrlArranger.TotalControlHeight(pnlBody) + 8;
  inherited;
end;

procedure TConfigDiffProgDlg.btnBrowseClick(Sender: TObject);
var
  OpenDlg: TOpenDialogEx; // self-aligning enhanced open dialogue box
resourcestring
  sFilter = 'Executable files (*.exe)|*.exe|' // file filter
    + 'All files (*.*)|*.*';
  sTitle = 'Select Diff Program';             // dialogue box title
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
    if TFile.Exists(GetProgFile, False) then
      OpenDlg.FileName := GetProgFile;
    if OpenDlg.Execute then
      edProgFile.Text := StrTrim(OpenDlg.FileName);
  finally
    OpenDlg.Free;
  end;
end;

procedure TConfigDiffProgDlg.CanOpenDialogClose(Sender: TObject;
  var CanClose: Boolean);
var
  Dlg: TOpenDialogEx; // dialogue box instance triggering this event
  ErrMsg: string;     // error message to be displayed on error
begin
  Dlg := Sender as TOpenDialogEx;
  CanClose := ValidateExeFileName(FileOpenEditedFileName(Dlg), ErrMsg);
  if not CanClose then
    TMessageBox.Error(Dlg, ErrMsg);
end;

class function TConfigDiffProgDlg.Execute(AOwner: TComponent): Boolean;
var
  Dlg: TConfigDiffProgDlg;
begin
  Dlg := TConfigDiffProgDlg.InternalCreate(AOwner);
  try
    Result := Dlg.ShowModal = mrOK;
  finally
    Dlg.Free;
  end;
end;

procedure TConfigDiffProgDlg.FormCreate(Sender: TObject);
begin
  inherited;
  fDiffViewer := TDiffViewer.Create;
end;

procedure TConfigDiffProgDlg.FormDestroy(Sender: TObject);
begin
  fDiffViewer.Free;
  inherited;
end;

function TConfigDiffProgDlg.GetParams: string;
begin
  Result := StrTrim(edParams.Text);
end;

function TConfigDiffProgDlg.GetProgFile: string;
begin
  Result := StrTrim(edProgFile.Text);
end;

procedure TConfigDiffProgDlg.InitForm;
begin
  inherited;
  edProgFile.Text := fDiffViewer.ExePath;
  edParams.Text := fDiffViewer.Params;
end;

function TConfigDiffProgDlg.IsValidExeFileName(const FileName: string): Boolean;
var
  DummyErrMsg: string;
begin
  Result := ValidateExeFileName(FileName, DummyErrMsg);
end;

function TConfigDiffProgDlg.ValidateExeFileName(const FileName: string;
  out ErrMsg: string): Boolean;
resourcestring
  // Error messages
  sFileDoesNotExist = 'Diff program file does not exist.';
  sFileNotExe = 'Diff program file is not a valid executable.';
var
  PermittedExeFileKinds: TExeFileKinds;
begin
  if not TFile.Exists(FileName, False) then
  begin
    ErrMsg := sFileDoesNotExist;
    Exit(False);
  end;
  {$IFDEF WIN64}
  PermittedExeFileKinds := [fkExe32, fkExe64];
  {$ELSE}
  if TOSInfo.IsWow64 then
    PermittedExeFileKinds := [fkExe32, fkExe64]
  else
    PermittedExeFileKinds := [fkExe32];
  {$ENDIF}
  if not (ExeFileType(FileName) in PermittedExeFileKinds) then
  begin
    ErrMsg := sFileNotExe;
    Exit(False);
  end;
  Result := True;
end;

procedure TConfigDiffProgDlg.btnClearClick(Sender: TObject);
begin
  edProgFile.Clear;
  edParams.Clear;
end;

procedure TConfigDiffProgDlg.btnOKClick(Sender: TObject);
var
  ErrMsg: string;
begin
  ModalResult := mrNone;
  if not ValidateExeFileName(GetProgFile, ErrMsg) then
  begin
    TMessageBox.Error(Self, ErrMsg);
    Exit;
  end;
  ModalResult := mrOK;
  fDiffViewer.ExePath := GetProgFile;
  fDiffViewer.Params := GetParams;
end;

end.

