{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2012, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * A frame that provides an editor for entering and ammending active text,
 * either as plain text or in markup.
 *
 * Designed specifically for use in the Snippets Editor.
}


unit FmSnippetsEditorDlg.FrActiveTextEditor;


interface


uses
  // Delphi
  Forms, ComCtrls, Menus, Classes, ActnList, Controls, StdCtrls,
  // Project
  ActiveText.UMain;


type
  TSnippetsActiveTextEdFrame = class(TFrame)
    edText: TMemo;
    alEditor: TActionList;
    mnuEdit: TPopupMenu;
    miConvertToPlainText: TMenuItem;
    miConvertToREML: TMenuItem;
    actConvertToPlainText: TAction;
    actConvertToREML: TAction;
    actSwitchToPlainTextMode: TAction;
    actSwitchToREMLMode: TAction;
    tcEditMode: TTabControl;
    procedure actConvertToPlainTextExecute(Sender: TObject);
    procedure actConvertToREMLExecute(Sender: TObject);
    procedure actSwitchToPlainTextModeExecute(Sender: TObject);
    procedure actSwitchToREMLModeExecute(Sender: TObject);
    procedure actConvertToPlainTextUpdate(Sender: TObject);
    procedure actConvertToREMLUpdate(Sender: TObject);
    procedure tcEditModeChange(Sender: TObject);
  public
    type
      TEditMode = (emPlainText, emREML, emAuto);
  strict private
    var
      fEditMode: TEditMode;
      fDefaultEditMode: TEditMode;
    function ActiveTextToPlainText(ActiveText: IActiveText): string;
    function ActiveTextToREML(ActiveText: IActiveText): string;
    function PlainTextToActiveText(Text: string): IActiveText;
    function REMLToActiveText(const Text: string): IActiveText;
    function Parse: IActiveText;
    function GetActiveText: IActiveText;
    procedure SetActiveText(Value: IActiveText);
    procedure SetEditMode(AMode: TEditMode);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Validate;
    procedure Clear;
    procedure Preview;
    function CanPreview: Boolean;
    property ActiveText: IActiveText read GetActiveText write SetActiveText;
    property DefaultEditMode: TEditMode
      read fDefaultEditMode write fDefaultEditMode;
  end;


implementation


uses
  // Project
  ActiveText.UValidator, FmViewExtraDlg, UConsts, UExceptions, UIStringList,
  USnippetExtraHelper, UStrUtils;


{$R *.dfm}

procedure TSnippetsActiveTextEdFrame.actConvertToPlainTextExecute(
  Sender: TObject);
var
  ActiveText: IActiveText;
begin
  ActiveText := GetActiveText;  // raises exception on error
  edText.Text := ActiveTextToPlainText(ActiveText);
  SetEditMode(emPlainText);
end;

procedure TSnippetsActiveTextEdFrame.actConvertToPlainTextUpdate(
  Sender: TObject);
begin
  (Sender as TAction).Enabled := fEditMode = emREML;
end;

procedure TSnippetsActiveTextEdFrame.actConvertToREMLExecute(Sender: TObject);
var
  ActiveText: IActiveText;
begin
  ActiveText := GetActiveText;
  edText.Text := ActiveTextToREML(ActiveText);
  SetEditMode(emREML);
end;

procedure TSnippetsActiveTextEdFrame.actConvertToREMLUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := fEditMode = emPlainText;
end;

function TSnippetsActiveTextEdFrame.ActiveTextToPlainText(
  ActiveText: IActiveText): string;
var
  Lines: IStringList;
begin
  // NOTE: we use IActiveText.ToString here, because there may be text not in
  // blocks and we want to see that: usual renderer will ignore that text.
  // However all lines are trimmed and empty blanks are ingored.
  Lines := TIStringList.Create(ActiveText.ToString, EOL, False, True);
  Result := Lines.GetText(EOL2, False); // insert blank line between paras
end;

function TSnippetsActiveTextEdFrame.ActiveTextToREML(ActiveText: IActiveText):
  string;
begin
  Result := TSnippetExtraHelper.BuildREMLMarkup(ActiveText);
end;

procedure TSnippetsActiveTextEdFrame.actSwitchToPlainTextModeExecute(
  Sender: TObject);
begin
  SetEditMode(emPlainText);
end;

procedure TSnippetsActiveTextEdFrame.actSwitchToREMLModeExecute(
  Sender: TObject);
begin
  SetEditMode(emREML);
end;

function TSnippetsActiveTextEdFrame.CanPreview: Boolean;
begin
  Result := StrTrim(edText.Text) <> '';
end;

procedure TSnippetsActiveTextEdFrame.Clear;
begin
  edText.Clear;
end;

constructor TSnippetsActiveTextEdFrame.Create(AOwner: TComponent);
begin
  inherited;

end;

destructor TSnippetsActiveTextEdFrame.Destroy;
begin

  inherited;
end;

function TSnippetsActiveTextEdFrame.GetActiveText: IActiveText;
var
  ValidatorErrInfo: TActiveTextValidator.TErrorInfo;
                          // active text validation error
  ParseError: EDataEntry; // parser error
resourcestring
  // parse error message
  sActiveTextErr = 'Error parsing extra information markup:' + EOL2 + '%s';
begin
  try
    Result := Parse;
  except
    // Convert active text parser to data exception
    on E: EActiveTextParserError do
    begin
      ParseError := EDataEntry.CreateFmt(
        sActiveTextErr, [E.Message], edText
      );
      if E.HasSelection then
        ParseError.Selection := E.Selection;
      raise ParseError;
    end
    else
      raise;
  end;
  // Validate the active text
  if not TActiveTextValidator.Validate(Result, ValidatorErrInfo) then
    raise EDataEntry.Create(
      ValidatorErrInfo.Description, edText  // no selection info available
    );
end;

function TSnippetsActiveTextEdFrame.Parse: IActiveText;
var
  Text: string;
begin
  Text := StrTrim(edText.Text);
  if Text = '' then
    Exit(TActiveTextFactory.CreateActiveText);
  case fEditMode of
    emPlainText:
    begin
      Result := PlainTextToActiveText(Text);
    end;
    emREML:
    begin
      Result := REMLToActiveText(Text);
    end;
    else
      Result := TActiveTextFactory.CreateActiveText;
  end;
end;

function TSnippetsActiveTextEdFrame.PlainTextToActiveText(
  Text: string): IActiveText;
var
  Lines: IStringList;
  Line: string;
begin
  Result := TActiveTextFactory.CreateActiveText;
  Text := StrTrim(Text);
  if Text = '' then
    Exit;
  { TODO: this code is similar (but safer) than
          TSnippetExtraHelper.PlainTextToActiveText: see if this can replace
          that. }
  Lines := TIStringList.Create(Text, EOL2, False, True);
  for Line in Lines do
  begin
    Result.AddElem(TActiveTextFactory.CreateActionElem(ekPara, fsOpen));
    Result.AddElem(
      TActiveTextFactory.CreateTextElem(StrCompressWhiteSpace(Line))
    );
    Result.AddElem(TActiveTextFactory.CreateActionElem(ekPara, fsClose));
  end;
end;

procedure TSnippetsActiveTextEdFrame.Preview;
var
  ActiveText: IActiveText;
begin
  ActiveText := GetActiveText;  // raises exceptions on error
  // TODO: TViewExtraDlg needs to be renamed
  TViewExtraDlg.Execute(nil, ActiveText);
end;

function TSnippetsActiveTextEdFrame.REMLToActiveText(const Text: string):
  IActiveText;
var
  Paras: IStringList;
begin
  Paras := TIStringList.Create(Text, EOL2, False, True);
  Result := TSnippetExtraHelper.BuildActiveText(Paras);
end;

procedure TSnippetsActiveTextEdFrame.SetActiveText(Value: IActiveText);
begin
  if fDefaultEditMode = emAuto then
    if Value.IsPlainText then
      SetEditMode(emPlainText)
    else
      SetEditMode(emREML)
  else
    SetEditMode(fDefaultEditMode);
  if not Value.IsEmpty then
  begin
    case fEditMode of
      emPlainText:
        edText.Text := ActiveTextToPlainText(Value);
      emREML:
        edText.Text := ActiveTextToREML(Value);
      else
        edText.Text := '';
    end;
  end
  else
    edText.Text := '';
end;

procedure TSnippetsActiveTextEdFrame.SetEditMode(AMode: TEditMode);
begin
  Assert(AMode <> emAuto, ClassName + '.SetEditMode: AMode is emAuto');
  fEditMode := AMode;
  case fEditMode of
    emPlainText:
      tcEditMode.TabIndex := 0;
    emREML:
      tcEditMode.TabIndex := 1;
  end;
end;

procedure TSnippetsActiveTextEdFrame.tcEditModeChange(Sender: TObject);
begin
  case tcEditMode.TabIndex of
    0: SetEditMode(emPlainText);
    1: SetEditMode(emREML);
  end;
end;

procedure TSnippetsActiveTextEdFrame.Validate;
begin
  // Following method parses data entry and converts any exceptions
  GetActiveText;
end;

end.

