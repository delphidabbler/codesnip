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
 * Implements a dialogue box where the user can specify a name under which to
 * save a user defined syntax highlighter theme.
}


unit CS.UI.Dialogs.HiliteThemeName;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls,

  // 3rd party
  Collections.Base,
  Collections.Sets,

  // Project
  CS.SourceCode.Hiliter.Themes,
  FmGenericOKDlg,
  UBaseObjects;

type
  THiliteThemeNameDlg = class(TGenericOKDlg, INoPublicConstruct)
    lblThemeName: TLabel;
    cbThemeName: TComboBox;
    lblError: TLabel;
    procedure btnOKClick(Sender: TObject);
    procedure cbThemeNameChange(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  strict private
    var
      fThemesList: TSyntaxHiliteThemes;
      fChosenThemeName: string;
      fBlockedThemeNames: THashSet<string>;
    procedure PopulateThemesCombo;
  strict protected
    procedure ConfigForm; override;
    procedure ArrangeForm; override;
    procedure InitForm; override;
  public
    class function Execute(const AOwner: TComponent;
      const AThemesList: TSyntaxHiliteThemes; out AThemeName: string): Boolean;
  end;


implementation

uses
  // Project
  UColours,
  UComparers,
  UConsts,
  UContainers,
  UCtrlArranger,
  UMessageBox,
  UStrUtils;

{$R *.dfm}

{ THiliteThemeNameDlg }

procedure THiliteThemeNameDlg.ArrangeForm;
begin
  TCtrlArranger.AlignLefts([lblThemeName, cbThemeName, lblError], 0);
  lblThemeName.Top := 0;
  TCtrlArranger.MoveBelow(lblThemeName, cbThemeName, 6);
  TCtrlArranger.MoveBelow(cbThemeName, lblError, 8);
  pnlBody.ClientWidth := TCtrlArranger.TotalControlWidth(pnlBody);
  pnlBody.ClientHeight := TCtrlArranger.TotalControlHeight(pnlBody);
  inherited;
end;

procedure THiliteThemeNameDlg.btnOKClick(Sender: TObject);
resourcestring
  sQuestion = 'The theme name you have chosen already exists.'
    + EOL2
    + 'OK to overwrite it?';
begin
  ModalResult := mrOK;
  if Assigned(fThemesList.FindThemeByFriendlyName(fChosenThemeName))
    and not TMessageBox.Confirm(Self, sQuestion) then
    ModalResult := mrNone;
end;

procedure THiliteThemeNameDlg.cbThemeNameChange(Sender: TObject);
var
  ThemeName: string;
begin
  ThemeName := StrTrim(cbThemeName.Text);
  if ThemeName <> EmptyStr then
  begin
    if fBlockedThemeNames.Contains(ThemeName) then
    begin
      btnOK.Enabled := False;
      lblError.Visible := True;
      fChosenThemeName := '';
    end
    else
    begin
      btnOK.Enabled := True;
      lblError.Visible := False;
      fChosenThemeName := ThemeName;
    end;
  end
  else
  begin
    btnOK.Enabled := False;
    lblError.Visible := False;
    fChosenThemeName := '';
  end;
end;

procedure THiliteThemeNameDlg.ConfigForm;
var
  Theme: TSyntaxHiliteTheme;
begin
  inherited;
    fBlockedThemeNames := THashSet<string>.Create(
    TTextComparator.ConstructRules
  );
  for Theme in fThemesList do
    if Theme.BuiltIn
      and not fBlockedThemeNames.Contains(Theme.FriendlyName) then
      fBlockedThemeNames.Add(Theme.FriendlyName);
end;

class function THiliteThemeNameDlg.Execute(const AOwner: TComponent;
  const AThemesList: TSyntaxHiliteThemes; out AThemeName: string): Boolean;
begin
  with InternalCreate(AOwner) do
    try
      fThemesList := AThemesList;
      Result := ShowModal = mrOK;
      if Result then
        AThemeName := fChosenThemeName;
    finally
      Free;
    end;
end;

procedure THiliteThemeNameDlg.FormDestroy(Sender: TObject);
begin
  inherited;
  fBlockedThemeNames.Free;
end;

procedure THiliteThemeNameDlg.InitForm;
begin
  inherited;
  btnOK.Enabled := False;
  lblError.Font.Color := clWarningText;
  lblError.Visible := False;
  PopulateThemesCombo;
end;

procedure THiliteThemeNameDlg.PopulateThemesCombo;
var
  Theme: TSyntaxHiliteTheme;
begin
  cbThemeName.Items.BeginUpdate;
  try
    cbThemeName.Items.Clear;
    for Theme in fThemesList do
      if not Theme.BuiltIn then
        cbThemeName.Items.Add(Theme.FriendlyName);
    cbThemeName.ItemIndex := -1;
    cbThemeName.Text := '';
  finally
    cbThemeName.Items.EndUpdate;
  end;
end;

end.

