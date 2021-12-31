{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2013-2021, Peter Johnson (gravatar.com/delphidabbler).
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
  ///  <summary>Class that implements a dialogue box that manages and accesses
  ///  a list of named user defined syntax highlighters.</summary?
  TUserHiliterMgrDlg = class(TGenericViewDlg, INoPublicConstruct)
    actDelete: TAction;
    actUse: TAction;
    alDlg: TActionList;
    btnDelete: TButton;
    btnUse: TButton;
    lblNames: TLabel;
    lbNames: TListBox;
    ///  <summary>Deletes the highlighter with the selected name from the list.
    ///  </summary>
    procedure actDeleteExecute(Sender: TObject);
    ///  <summary>Enables or disables the Delete action depending on whether a
    ///  highlighter name is selected in the list.</summary>
    procedure actDeleteUpdate(Sender: TObject);
    ///  <summary>Closes the dialogue box passing back the selected highlighter
    ///  for use as the current highlighter.</summary>
    ///  <remarks>The ASelected parameter of the Execute method is set to the
    ///  selected highlighter highlighter.</remarks>
    procedure actUseExecute(Sender: TObject);
    ///  <summary>Enables or disables the Use action depending on whether a
    ///  highlighter name is selected in the list.</summary>
    procedure actUseUpdate(Sender: TObject);
    ///  <summary>Selects the list item under the mouse then triggers the Use
    ///  action, causing actUseExcute to be called.</summary>
    procedure lbNamesDblClick(Sender: TObject);
  strict private
    var
      ///  <summary>List of named highlighters being managed.</summary>
      fNamedAttrs: INamedHiliteAttrs;
      ///  <summary>Selected highlighter.</summary>
      fSelected: IHiliteAttrs;
    ///  <summary>Returns the name of the highlighter selected in the list box.
    ///  </summary>
    function SelectedName: string;
  strict protected
    ///  <summary>Arranges controls and sets width and height of dialogue box.
    ///  </summary>
    procedure ArrangeForm; override;
    ///  <summary>Initialise form's controls.</summary>
    procedure InitForm; override;
  public
    ///  <summary>Displays dialogue box and passes back any selected highlighter
    ///  attributes.</summary>
    ///  <param name="AOwner">TComponent [in] Owning control over which dialogue
    ///  box is aligned.</param>
    ///  <param name="ANamedAttrs">INamedHiliteAttrs [in] Object containing
    ///  named attributes to be managed. Will be modified if user deletes one or
    ///  more highlighters.</param>
    ///  <param name="ASelected">IHiliteAttrs [out] Set to any highlighter
    ///  selected for use by user. Undefined if user did not select a
    ///  highlighter.</param>
    ///  <returns>Boolean. True if user selected a highlighter for use or False
    ///  if dialogue box was closed without selecting a highlighter for use.
    ///  </returns>
    ///  <remarks>Note that even if False is returned the content of ANamedAttrs
    ///  may have been changed if user deleted a highlighter.</remarks>
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

