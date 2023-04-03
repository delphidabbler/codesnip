{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2022, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Implements a dialogue box that is optionally displayed at start-up when
 * Delphi compiler(s) are installed but not registered with CodeSnip. Allows the
 * user to specify which, if any, compiler(s) are to be registered.
}


unit FmRegisterCompilersDlg;

interface

uses
  // Delphi
  Forms,
  Classes,
  StdCtrls,
  Controls,
  CheckLst,
  ExtCtrls,
  SysUtils,
  // Project
  FmGenericOKDlg,
  FrBrowserBase,
  FrFixedHTMLDlg,
  FrHTMLDlg,
  Compilers.UCompilers,
  Compilers.UGlobals,
  UBaseObjects,
  UCSSBuilder;

type
  TRegisterCompilersDlg = class(TGenericOKDlg, INoPublicConstruct)
    lblDesc: TLabel;
    clbCompilers: TCheckListBox;
    frmNotes: TFixedHTMLDlgFrame;
    chkDontShowAgain: TCheckBox;
    procedure FormDestroy(Sender: TObject);
    procedure alMainUpdate(Action: TBasicAction; var Handled: Boolean);
  strict private
    var
      fCandidateCompilers: TCompilerList;
      fSelectedCompilers: TCompilerList;
    function CountTickedCompilers: Integer;
    procedure StoreSelectedCompilers;
    procedure SetCaptions;
    procedure PopulateList;
  strict protected
    procedure CustomiseControls; override;
    procedure ArrangeControls; override;
    procedure InitialiseControls; override;
    ///  <summary>Handles HTML frame's OnBuildCSS event. Adds additional CSS
    ///  required by HTML in this form.</summary>
    ///  <param name="Sender">TObject [in] Reference to object triggering event.
    ///  </param>
    ///  <param name="CSSBuilder">TCSSBuilder [in] Object used to construct the
    ///  CSS.</param>
    procedure BuildCSS(Sender: TObject; const CSSBuilder: TCSSBuilder);
  public
    class function Execute(const AOwner: TComponent;
      const CandidateCompilers: TCompilerList;
      const SelectedCompilers: TCompilerList): Boolean;
  end;

implementation

uses
  Compilers.USettings,
  UBox,
  UCSSUtils,
  UCtrlArranger;

{$R *.dfm}

{ TRegisterCompilersDlg }

procedure TRegisterCompilersDlg.alMainUpdate(Action: TBasicAction;
  var Handled: Boolean);
begin
  inherited;
  btnOK.Enabled := chkDontShowAgain.Checked or (CountTickedCompilers > 0);
end;

procedure TRegisterCompilersDlg.ArrangeControls;
begin
  TCtrlArranger.SetLabelHeight(lblDesc);
  clbCompilers.Top := TCtrlArranger.BottomOf(lblDesc, 8);
  frmNotes.Top := TCtrlArranger.BottomOf(clbCompilers, 0);
  frmNotes.Height := frmNotes.DocHeight;
  chkDontShowAgain.Top := TCtrlArranger.BottomOf(frmNotes, 8);
  TCtrlArranger.AlignLefts(
    [lblDesc, clbCompilers, frmNotes, chkDontShowAgain], 0
  );
  pnlBody.ClientHeight := TCtrlArranger.TotalControlHeight(pnlBody) + 8;
  pnlBody.ClientWidth := TCtrlArranger.TotalControlWidth(pnlBody);
  inherited;
end;

procedure TRegisterCompilersDlg.BuildCSS(Sender: TObject;
  const CSSBuilder: TCSSBuilder);
var
  Selector: TCSSSelector;
begin
  Selector := CSSBuilder.AddSelector('ol');
  Selector.AddProperty(TCSS.MarginProp(cssLeft, 24));
  Selector.AddProperty(TCSS.MarginProp(cssTop, 6));
  Selector.AddProperty(TCSS.MarginProp(cssBottom, 0));
  Selector.AddProperty(TCSS.PaddingProp(0));

  Selector := CSSBuilder.AddSelector('ol li');
  Selector.AddProperty(TCSS.MarginProp(cssTop, 6));

  Selector := CSSBuilder.AddSelector('ol li ul');
  Selector.AddProperty(TCSS.MarginProp(cssLeft, 16));
  Selector.AddProperty(TCSS.MarginProp(cssTop, 6));
  Selector.AddProperty(TCSS.MarginProp(cssBottom, 0));
  Selector.AddProperty(TCSS.PaddingProp(0));

  Selector := CSSBuilder.AddSelector('ol li ul li');
  Selector.AddProperty(TCSS.MarginProp(cssTop, 0));
end;

function TRegisterCompilersDlg.CountTickedCompilers: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to Pred(clbCompilers.Count) do
    if clbCompilers.Checked[I] then
      Inc(Result);
end;

procedure TRegisterCompilersDlg.CustomiseControls;
begin
  inherited;
  frmNotes.OnBuildCSS := BuildCSS;
  frmNotes.Initialise('dlg-register-compilers.html');
end;

class function TRegisterCompilersDlg.Execute(const AOwner: TComponent;
  const CandidateCompilers: TCompilerList;
  const SelectedCompilers: TCompilerList): Boolean;
var
  Dlg: TRegisterCompilersDlg;
begin
  Assert(Assigned(CandidateCompilers),
    ClassName + '.Execute: AvailCompilers list is nil');
  Assert(Assigned(SelectedCompilers),
    ClassName + '.Execute: CompilersToReg list is nil');
  Assert(CandidateCompilers.Count > 0,
    ClassName + '.Execute: AvailCompilers list must not be empty');

  Dlg := InternalCreate(AOwner);
  try
    Dlg.fCandidateCompilers := CandidateCompilers;  // reference to list
    Dlg.fSelectedCompilers := SelectedCompilers; // reference to list
    Result := Dlg.ShowModal = mrOK;
    if Result then
    begin
      Dlg.StoreSelectedCompilers;
      TCompilerSettings.PermitStartupDetection :=
        not Dlg.chkDontShowAgain.Checked;
    end;
  finally
    Dlg.Free;
  end;
end;

procedure TRegisterCompilersDlg.FormDestroy(Sender: TObject);
var
  Idx: Integer;
begin
  // Free TBox<> objects associated with compiler list items
  for Idx := Pred(clbCompilers.Count) downto 0 do
    clbCompilers.Items.Objects[Idx].Free;
  inherited;
end;

procedure TRegisterCompilersDlg.InitialiseControls;
begin
  inherited;
  SetCaptions;
  PopulateList;
end;

procedure TRegisterCompilersDlg.PopulateList;
var
  Compiler: ICompiler;
begin
  clbCompilers.Items.BeginUpdate;
  try
    clbCompilers.Clear;
    for Compiler in fCandidateCompilers do
    begin
      clbCompilers.Items.AddObject(
        Compiler.GetName,
        TBox<ICompiler>.Create(Compiler)
      );
    end;
  finally
    clbCompilers.Items.EndUpdate;
  end;
  Assert(clbCompilers.Count > 0,
    ClassName + '.PopulateList: compiler list empty');
  clbCompilers.ItemIndex := 0;
end;

procedure TRegisterCompilersDlg.SetCaptions;
resourcestring
  sFormCaptionS = 'Unregistered Delphi Installations Detected';
  sFormCaptionP = 'Unregistered Delphi Installation Detected';
  sDescS = '&Unregistered compiler. Tick to register:';
  sDescP = '&Unregistered compilers. Tick the ones to be registered:';
begin
  // Set form and description captions
  if fCandidateCompilers.Count = 1 then
  begin
    Caption := sFormCaptionS;
    lblDesc.Caption := sDescS;
  end
  else
  begin
    // fCandidateCompilers.Count > 1 because fCandidateCompilers.Count > 0 assured by
    // assertions in Execute method
    Caption := sFormCaptionP;
    lblDesc.Caption := sDescP;
  end;
end;

procedure TRegisterCompilersDlg.StoreSelectedCompilers;
var
  I: Integer;
begin
  fSelectedCompilers.Clear;
  for I := 0 to Pred(clbCompilers.Count) do
    if clbCompilers.Checked[I] then
    begin
      fSelectedCompilers.Add(
        TBox<ICompiler>(clbCompilers.Items.Objects[I]).Value
      );
    end;
end;

end.

