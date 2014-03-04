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
  Forms,
  Controls,
  StdCtrls,
  ActnList,
  ExtCtrls,
  // 3rd party
  Collections.Lists,
  // Project
  CS.Components.EditCtrls,  // required to sub-class TEdit
  CS.Database.Types,
  UComparers,
  UExceptions,
  UIStringList;


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
      fEditCtrls: TObjectList<TEdit>;
      fMoreLbl: TLabel;
      fAllTagNames: TStrings;
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
  Graphics,
  ActiveX,
  ShlObj,
  ComObj,
  // Project
  CS.Database.Tags,
  CS.Utils.COM,
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
  NewEditCtrl: TEdit;
  LastEditCtrl: TEdit;
  CtrlTop: Integer;
begin
  CtrlLeft := 0;
  fMoreLbl.Free;
  CtrlTop := TCtrlArranger.TotalControlHeight(pnlEditors);
  for I := 1 to RowSize do
  begin
    NewEditCtrl := CreateEditCtrl;
    NewEditCtrl.Top := CtrlTop;
    NewEditCtrl.Left := CtrlLeft;
    NewEditCtrl.TabOrder := fEditCtrls.Count;
    fEditCtrls.Add(NewEditCtrl);
    CtrlLeft := CtrlLeft + NewEditCtrl.Width + 2;
  end;
  LastEditCtrl := fEditCtrls.Last;
  fMoreLbl := CreateMoreLbl;
  TCtrlArranger.MoveToRightOf(LastEditCtrl, fMoreLbl, 8);
  TCtrlArranger.AlignVCentresTo([LastEditCtrl], [fMoreLbl]);
  pnlEditors.Height := TCtrlArranger.TotalControlHeight(pnlEditors) + 4;
  sbTagEditors.ScrollInView(fMoreLbl);
end;

procedure TTagsEditorFrame.ArrangeControls;
begin
  // TODO: remove this method if remains empty
end;

procedure TTagsEditorFrame.Clear;
begin
  fEditCtrls.Clear;
  AddEditRow;
end;

constructor TTagsEditorFrame.Create(AOwner: TComponent);
var
  Tag: TTag;
begin
  inherited;
  fAllTagNames := TStringList.Create;
  for Tag in Database.GetAllTags do
    fAllTagNames.Add(Tag.ToString);
  fEditCtrls := TObjectList<TEdit>.Create;
  fEditCtrls.OwnsObjects := True;
  fMoreLbl := CreateMoreLbl;
end;

function TTagsEditorFrame.CreateEditCtrl: TEdit;
var
  Enum: IEnumString;
  AutoComp: IAutoComplete2;
begin
  Result := TEdit.Create(Self);
  Result.Parent := pnlEditors;
  Result.Width := 160;
  Result.TextHint := 'Add a tag';
  Enum := TCOMEnumString.Create(fAllTagNames);
  AutoComp := CreateComObject(CLSID_AutoComplete) as IAutoComplete2;
  AutoComp.SetOptions(
    ACO_AUTOSUGGEST or ACO_UPDOWNKEYDROPSLIST or ACO_AUTOAPPEND
  );
  AutoComp.Init(Result.Handle, Enum, nil, nil);
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
  fEditCtrls.Free; // frees owned manager objects
  fAllTagNames.Free;
  inherited;
end;

procedure TTagsEditorFrame.FrameResize(Sender: TObject);
begin
  ArrangeControls;
end;

function TTagsEditorFrame.GetEnteredTagNames: IStringList;
var
  EditCtrl: TEdit;
  TagName: string;
begin
  Result := TIStringList.Create;
  for EditCtrl in fEditCtrls do
  begin
    TagName := StrTrim(EditCtrl.Text);
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
  if not fEditCtrls.Empty then
    fEditCtrls.First.SetFocus;
end;

procedure TTagsEditorFrame.SetTags(const Value: ITagSet);
var
  I: Integer;
  Tag: TTag;
begin
  I := 0;
  for Tag in Value do
  begin
    if I >= fEditCtrls.Count then
      AddEditRow;
    fEditCtrls[I].Text := Tag.ToString;
    Inc(I);
  end;
  if Value.Count mod RowSize = 0 then
    AddEditRow; // all edit controls occupied so add an empty row
end;

procedure TTagsEditorFrame.ValidateEnteredTagNames;
var
  EditCtrl: TEdit;
  TagName: string;
resourcestring
  sBadTag = 'Invalid tag name: "%s"';
begin
  for EditCtrl in fEditCtrls do
  begin
    TagName := StrTrim(EditCtrl.Text);
    if (TagName <> '') and not TTag.IsValidTagString(TagName) then
      raise ETagsEditor.CreateFmt(sBadTag, [TagName], EditCtrl);
  end;
end;

end.

