{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2026, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Implements a dialogue box that enables the user to edit a list of text items.
}


unit UI.Forms.ListEditorDlg;

interface

uses
  // Delphi
  StdCtrls,
  Controls,
  ExtCtrls,
  Classes,
  // Project
  FmGenericOKDlg,
  UBaseObjects,
  UIStringList;

type
  ///  <summary>A dialogue box that enables the user to edit a list of string
  ///  items.</summary>
  TListEditorDlg = class(TGenericOKDlg, INoPublicConstruct)
    lblInstructions: TLabel;
    edList: TMemo;
  strict protected
    ///  <summary>Returns a clone of the memo control.</summary>
    function GetList: IStringList;
    ///  <summary>Stores a clone of given list in the memo control.</summary>
    procedure SetList(AList: IStringList);
    ///  <summary>Positions controls and sets form size according to body panel
    ///   dimensions.</summary>
    procedure ArrangeForm; override;
  public
    ///  <summary>Displays the List Editor dialogue box and returns the list
    ///  edited by the user.</summary>
    ///  <param name="AOwner"><c>TComponent</c> [in] Reference to the component
    ///  that owns this dialogue box.</param>
    ///  <param name="ACaption"><c>string</c> [in] Caption to be displayed in
    ///  the dialogue box's title bar. Must not be an empty string.</param>
    ///  <param name="AList"><c>IStringList</c> [in] The list to be edited. The
    ///  list contents are displayed in the dialogue box unless <c>AList</c> is
    ///  empty or is <c>nil</c> when nothing is displayed. [out] Set to a new
    ///  <c>IStringList</c> instance containing the list as edited by the
    ///  user. If the user cancels then <c>AList</c> is unchanged.</param>
    ///  <returns><c>Boolean</c>. <c>True</c> if the user accepts the edit or
    ///  <c>False</c> if the user cancels.</returns>
    class function Execute(const AOwner: TComponent; const ACaption: string;
      var AList: IStringList): Boolean;
  end;

implementation

uses
  // Project
  UCtrlArranger,
  UStrUtils;

{$R *.dfm}

{ TListEditorDlg }

procedure TListEditorDlg.ArrangeForm;
begin
  TCtrlArranger.SetLabelHeights(Self);
  lblInstructions.Top := 4;
  TCtrlArranger.AlignLefts([lblInstructions, edList], 0);
  TCtrlArranger.MoveBelow(lblInstructions, edList, 12);
  edList.Width := pnlBody.ClientWidth;
  edList.Height := pnlBody.ClientHeight - edList.Top;
  inherited;
end;

class function TListEditorDlg.Execute(const AOwner: TComponent;
  const ACaption: string; var AList: IStringList): Boolean;
var
  Dlg: TListEditorDlg;
begin
  Assert(not StrIsEmpty(ACaption), ClassName + '.Execute: ACaption = ''''');
  Dlg := InternalCreate(AOwner);
  try
    Dlg.SetList(AList);
    Dlg.Caption := StrTrim(ACaption);
    Result := Dlg.ShowModal = mrOK;
    if Result then
    begin
      AList := Dlg.GetList;
    end;
  finally
    Dlg.Free;
  end;
end;

function TListEditorDlg.GetList: IStringList;
begin
  Result := TIStringList.Create(edList.Lines);
end;

procedure TListEditorDlg.SetList(AList: IStringList);
begin
  if Assigned(AList) then
    AList.CopyTo(edList.Lines, True)
  else
    edList.Clear;
end;

end.
