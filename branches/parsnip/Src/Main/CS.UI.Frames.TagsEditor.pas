{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2014, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * Implements a frame that is used to edit a snippet's tags.
}


unit CS.UI.Frames.TagsEditor;


interface


uses
  // Delphi
  Classes,
  Types,
  Grids,
  Forms,
  Controls,
  StdCtrls,
  ActnList,
  ExtCtrls,
  // 3rd party
  Collections.Lists,
  Collections.Base,
  // Project
  CS.Components.EditCtrls,
  CS.Database.Types,
  UComparers,
  UExceptions,
  UIStringList;


type
  TAutoCompleteEditMgr = class(TObject)
  strict private
    var
      fEdit: TEdit;
  public
    constructor Create(const AEdit: TEdit);
    property Control: TEdit read fEdit;
  end;

type
  TTagsEditorFrame = class(TFrame)
    alEditor: TActionList;
    actMore: TAction;
    sbTagEditors: TScrollBox;
    pnlEditors: TPanel;
    procedure FrameResize(Sender: TObject);
    procedure actMoreExecute(Sender: TObject);
  strict private
    const
      RowSize = 3;
    var
      fEditMgrs: TObjectList<TAutoCompleteEditMgr>;
      fMoreLbl: TLabel;
    function GetEnteredTagNames: IStringList;
    function GetTags: ITagSet;
    procedure SetTags(const Value: ITagSet);
    function CreateEditCtrl: TEdit;
    function CreateMoreLbl: TLabel;
    procedure AddEditRow;
    procedure ValidateEnteredTagNames;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetFocus; override;
    procedure ArrangeControls;
    procedure Clear;
    property Tags: ITagSet read GetTags write SetTags;
  end;

  ETagsEditor = class(EDataEntry);


implementation


uses
  // Delphi
  Math,
  Graphics,
  // Project
  CS.Database.Tags,
  DB.UMain,
  UColours,
  UCtrlArranger,
  UStrUtils;

{$R *.dfm}

{ TTagsEditorFrame }

procedure TTagsEditorFrame.actMoreExecute(Sender: TObject);
begin
  AddEditRow;
end;

procedure TTagsEditorFrame.AddEditRow;
var
  I: Integer;
  CtrlLeft: Integer;
  EditCtrl: TEdit;
  LastEditCtrl: TEdit;
  CtrlTop: Integer;
begin
  CtrlLeft := 0;
  fMoreLbl.Free;
  CtrlTop := TCtrlArranger.TotalControlHeight(pnlEditors);
  for I := 1 to RowSize do
  begin
    EditCtrl := CreateEditCtrl;
    EditCtrl.Top := CtrlTop;
    EditCtrl.Left := CtrlLeft;
    EditCtrl.TabOrder := fEditMgrs.Count;
    fEditMgrs.Add(TAutoCompleteEditMgr.Create(EditCtrl));
    CtrlLeft := CtrlLeft + EditCtrl.Width + 2;
  end;
  LastEditCtrl := fEditMgrs[Pred(fEditMgrs.Count)].Control;
  fMoreLbl := CreateMoreLbl;
  TCtrlArranger.MoveToRightOf(LastEditCtrl, fMoreLbl, 8);
  TCtrlArranger.AlignVCentresTo([LastEditCtrl], [fMoreLbl]);
  pnlEditors.Height := TCtrlArranger.TotalControlHeight(pnlEditors) + 4;
  sbTagEditors.ScrollInView(fMoreLbl);
end;

procedure TTagsEditorFrame.ArrangeControls;
begin
end;

procedure TTagsEditorFrame.Clear;
var
  EditMgr: TAutoCompleteEditMgr;
begin
  for EditMgr in fEditMgrs do
    EditMgr.Control.Clear;
end;

constructor TTagsEditorFrame.Create(AOwner: TComponent);
begin
  inherited;
  fEditMgrs := TObjectList<TAutoCompleteEditMgr>.Create;
  fEditMgrs.OwnsObjects := True;
  fMoreLbl := CreateMoreLbl;
  AddEditRow;
end;

function TTagsEditorFrame.CreateEditCtrl: TEdit;
begin
  Result := TEdit.Create(Self);
  Result.Parent := pnlEditors;
  Result.Width := 160;
  Result.TextHint := 'Add a tag';
end;

function TTagsEditorFrame.CreateMoreLbl: TLabel;
begin
  Result := TLabel.Create(Self);
  Result.Parent := pnlEditors;
  Result.OnClick := nil;
  Result.Action := actMore;
  Result.Font.Style := [fsUnderline];
  Result.Font.Color := clCommandLink;
  Result.Cursor := crHandPoint;
  Result.Caption := 'Add a row';
end;

destructor TTagsEditorFrame.Destroy;
begin
  fEditMgrs.Free; // frees owned manager objects
  inherited;
end;

procedure TTagsEditorFrame.FrameResize(Sender: TObject);
begin
  ArrangeControls;
end;

function TTagsEditorFrame.GetEnteredTagNames: IStringList;
var
  EditMgr: TAutoCompleteEditMgr;
  TagName: string;
begin
  Result := TIStringList.Create;
  for EditMgr in fEditMgrs do
  begin
    TagName := StrTrim(EditMgr.Control.Text);
    if TTag.IsValidTagString(TagName) then
      Result.Add(TagName);
  end;
end;

function TTagsEditorFrame.GetTags: ITagSet;
var
  UsedTagNames: IStringList;
  TagName: string;
begin
  ValidateEnteredTagNames;
  UsedTagNames := GetEnteredTagNames;
  Result := TTagSet.Create;
  for TagName in UsedTagNames do
    Result.Add(TTag.Create(TagName));
end;

procedure TTagsEditorFrame.SetFocus;
begin
  inherited;
  if not fEditMgrs.Empty then
    fEditMgrs.First.Control.SetFocus;
end;

procedure TTagsEditorFrame.SetTags(const Value: ITagSet);
var
  I: Integer;
  Tag: TTag;
begin
  I := 0;
  for Tag in Value do
  begin
    if I >= fEditMgrs.Count then
      AddEditRow;
    fEditMgrs[I].Control.Text := Tag.ToString;
    Inc(I);
  end;
  if Value.Count mod RowSize = 0 then
    AddEditRow; // all edit controls occupied so add an empty row
end;

procedure TTagsEditorFrame.ValidateEnteredTagNames;
var
  EditMgr: TAutoCompleteEditMgr;
  TagName: string;
resourcestring
  sBadTag = 'Invalid tag name: "%s"';
begin
  for EditMgr in fEditMgrs do
  begin
    TagName := StrTrim(EditMgr.Control.Text);
    if (TagName <> '') and not TTag.IsValidTagString(TagName) then
      raise ETagsEditor.CreateFmt(sBadTag, [TagName], EditMgr.Control);
  end;
end;

{ TAutoCompleteEditMgr }

constructor TAutoCompleteEditMgr.Create(const AEdit: TEdit);
begin
  Assert(Assigned(AEdit), ClassName + '.Create: AEdit is nil');
  inherited Create;
  fEdit := AEdit;
end;

end.

