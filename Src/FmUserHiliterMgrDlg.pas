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
 * Implements a dialogue box that manages named user defined syntax
 * highlighters. It lists available named highlighters which can be selected for
 * use or deletion.
}


unit FmUserHiliterMgrDlg;


interface


uses
  // Delphi
  UBaseObjects, Classes, ActnList, StdCtrls, Controls, ExtCtrls,
  // Project
  FmGenericViewDlg, Hiliter.UGlobals;


type
  TUserHiliterMgrDlg = class(TGenericViewDlg, INoPublicConstruct)
    lbNames: TListBox;
    btnUse: TButton;
    btnDelete: TButton;
    alDlg: TActionList;
    lblNames: TLabel;
    actUse: TAction;
    actDelete: TAction;
    procedure actUseExecute(Sender: TObject);
    procedure actDeleteExecute(Sender: TObject);
    procedure actUseUpdate(Sender: TObject);
    procedure actDeleteUpdate(Sender: TObject);
    procedure lbNamesDblClick(Sender: TObject);
  strict private
    var
      fNamedAttrs: INamedHiliteAttrs;
      fSelected: IHiliteAttrs;
    function SelectedName: string;
  strict protected
    procedure ArrangeForm; override;
    procedure InitForm; override;
  public
    class function Execute(AOwner: TComponent; ANamedAttrs: INamedHiliteAttrs;
      out ASelected: IHiliteAttrs): Boolean;
  end;


implementation


uses
  // Project
  UCtrlArranger, UMessageBox;

{$R *.dfm}


{ TUserHiliterMgrDlg }

procedure TUserHiliterMgrDlg.actDeleteExecute(Sender: TObject);
resourcestring
  sConfirm = 'Are you sure you want to delete this highlighter?';
var
  DeleteName: string;
  DeleteIdx: Integer;
begin
  if TMessageBox.Confirm(Self, sConfirm) then
  begin
    DeleteName := SelectedName;
    fNamedAttrs.Delete(DeleteName);
    DeleteIdx := lbNames.Items.IndexOf(DeleteName);
    Assert(DeleteIdx >= 0, ClassName + '.actDeleteExecute: Name not in list');
    lbNames.Items.Delete(DeleteIdx);
  end;
end;

procedure TUserHiliterMgrDlg.actDeleteUpdate(Sender: TObject);
begin
  actDelete.Enabled := lbNames.ItemIndex >= 0;
end;

procedure TUserHiliterMgrDlg.actUseExecute(Sender: TObject);
begin
  fSelected := fNamedAttrs[SelectedName];
  ModalResult := mrOk;
end;

procedure TUserHiliterMgrDlg.actUseUpdate(Sender: TObject);
begin
  actUse.Enabled := lbNames.ItemIndex >= 0;
end;

procedure TUserHiliterMgrDlg.ArrangeForm;
begin
  TCtrlArranger.AlignLefts([lblNames, lbNames], 0);
  TCtrlArranger.AlignLefts(
    [btnUse, btnDelete], TCtrlArranger.RightOf(lbNames, 12)
  );
  pnlBody.ClientWidth := TCtrlArranger.TotalControlWidth(pnlBody);

  lblNames.Top := 0;
  TCtrlArranger.MoveBelow(lblNames, lbNames, 6);
  TCtrlArranger.AlignTops([lbNames, btnUse]);
  TCtrlArranger.MoveBelow(btnUse, btnDelete, 12);
  pnlBody.ClientHeight := TCtrlArranger.TotalControlHeight(pnlBody);

  inherited;
end;

class function TUserHiliterMgrDlg.Execute(AOwner: TComponent;
  ANamedAttrs: INamedHiliteAttrs; out ASelected: IHiliteAttrs): Boolean;
begin
  with InternalCreate(AOwner) do
    try
      fNamedAttrs := ANamedAttrs;
      Result := ShowModal = mrOK;
      if Result then
        ASelected := fSelected;
    finally
      Free;
    end;
end;

procedure TUserHiliterMgrDlg.InitForm;
var
  Names: TStringList;
  Name: string;
begin
  inherited;
  Names := TStringList.Create;
  try
    Names.CaseSensitive := False;
    for Name in fNamedAttrs.Names do
      Names.Add(Name);
    Names.Sort;
    lbNames.Clear;
    lbNames.Items.AddStrings(Names);
  finally
    Names.Free;
  end;
  lbNames.SetFocus;
end;

procedure TUserHiliterMgrDlg.lbNamesDblClick(Sender: TObject);
begin
  actUse.Execute;
end;

function TUserHiliterMgrDlg.SelectedName: string;
begin
  Assert(lbNames.ItemIndex >= 0, ClassName + '.SelectedName: No name selected');
  Result := lbNames.Items[lbNames.ItemIndex];
end;

end.
