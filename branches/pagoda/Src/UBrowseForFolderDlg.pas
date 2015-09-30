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
 * Implements a class that displays a browse for folder dialogue box.
}


unit UBrowseForFolderDlg;


interface


uses
  // Delphi
  Classes,
  // 3rd party
  PJShellFolders;


type
  ///  <summary>
  ///  Displays browse for folder dialogue box aligned over a given host
  ///  control.
  ///  </summary>
  ///  <remarks>
  ///  Wraps a TPJBrowseDialog component.
  ///  </remarks>
  TBrowseForFolderDlg = class(TObject)
  strict private
    var
      ///  <summary>Reference to dialogue box component.</summary>
      fDialog: TPJBrowseDialog;
      ///  <summary>Reference to component above which dialogue box is to be
      ///  aligned.</summary>
      fHost: TComponent;
  strict private
    ///  <summary>Handles dialogue's OnInitialise event. Aligns dialogue box
    ///  over parent form.</summary>
    procedure DlgInitHandler(Sender: TObject);
    ///  <summary>Handles dialogue's OnHelp event. Displays help using keyword
    ///  recorded in HelpKeyword property.</summary>
    procedure DlgHelpHandler(Sender: TObject; var Cancel: Boolean);
    ///  <summary>Getter for FolderName property.</summary>
    function GetFolderName: string;
    ///  <summary>Getter for Headline property.</summary>
    function GetHeadline: string;
    ///  <summary>Getter for Title property.</summary>
    function GetTitle: string;
    ///  <summary>Getter for HelpKeyword property.</summary>
    function GetHelpKeyword: string;
    ///  <summary>Getter for MakeFolderBtnVisible property.</summary>
    function GetMakeFolderBtnVisible: Boolean;
    ///  <summary>Setter for Headline property.</summary>
    procedure SetHeadline(const Value: string);
    ///  <summary>Setter for Title property.</summary>
    procedure SetTitle(const Value: string);
    ///  <summary>Setter for HelpKeyword property.</summary>
    procedure SetHelpKeyword(const Value: string);
    ///  <summary>Setter for MakeFolderBtnVisible property.</summary>
    procedure SetMakeFolderBtnVisible(const Value: Boolean);
  public
    ///  <summary>Constructs object and initialise dialogue box.</summary>
    ///  <param name="AHost">TComponent [in] Reference to component over which
    ///  the dialogue box will be aligned.</param>
    constructor Create(AHost: TComponent);
    ///  <summary>Object destructor. Tidies up object.</summary>
    destructor Destroy; override;
    ///  <summary>Displays dialogue box and returns True if user OKs, False if
    ///  user cancels.</summary>
    function Execute: Boolean;
    ///  <summary>Text display in dialogue window caption.</summary>
    property Title: string read GetTitle write SetTitle;
    ///  <summary>Heading line display in dialogue box.</summary>
    property Headline: string read GetHeadline write SetHeadline;
    ///  <summary>Name of folder selected in dialogue box.</summary>
    property FolderName: string read GetFolderName;
    ///  <summary>Help keyword. No help button is displayed: help topic is
    ///  displayed if F1 key is pressed.</summary>
    property HelpKeyword: string read GetHelpKeyword write SetHelpKeyword;
    ///  <summary>Determines whether "Make folder" button is displayed in
    ///  dialogue box.</summary>
    property MakeFolderBtnVisible: Boolean
      read GetMakeFolderBtnVisible write SetMakeFolderBtnVisible;
  end;


implementation


uses
  // Project
  UDlgHelper, UHelpMgr;


{ TBrowseForFolderDlg }

constructor TBrowseForFolderDlg.Create(AHost: TComponent);
begin
  inherited Create;
  fHost := AHost;
  fDialog := TPJBrowseDialog.Create(nil);
  fDialog.OnInitialise := DlgInitHandler;
  fDialog.OnHelp := DlgHelpHandler;
  fDialog.Options := [boHideMakeFolderBtn, boDirsOnly, boNewDlgStyle];
  fDialog.HelpType := htKeyword;
end;

destructor TBrowseForFolderDlg.Destroy;
begin
  fDialog.Free;
  inherited;
end;

procedure TBrowseForFolderDlg.DlgHelpHandler(Sender: TObject;
  var Cancel: Boolean);
begin
  if fDialog.HelpKeyword = '' then
    Exit;
  HelpMgr.ShowHelp(fDialog.HelpKeyword);
  Cancel := True;
end;

procedure TBrowseForFolderDlg.DlgInitHandler(Sender: TObject);
begin
  TDlgAligner.Align(fDialog.Handle, fHost);
end;

function TBrowseForFolderDlg.Execute: Boolean;
begin
  Result := fDialog.Execute;
end;

function TBrowseForFolderDlg.GetFolderName: string;
begin
  Result := fDialog.FolderName;
end;

function TBrowseForFolderDlg.GetHeadline: string;
begin
  Result := fDialog.Headline;
end;

function TBrowseForFolderDlg.GetHelpKeyword: string;
begin
  Result := fDialog.HelpKeyword;
end;

function TBrowseForFolderDlg.GetMakeFolderBtnVisible: Boolean;
begin
  Result := not (boHideMakeFolderBtn in fDialog.Options);
end;

function TBrowseForFolderDlg.GetTitle: string;
begin
  Result := fDialog.Title;
end;

procedure TBrowseForFolderDlg.SetHeadline(const Value: string);
begin
  fDialog.Headline := Value;
end;

procedure TBrowseForFolderDlg.SetHelpKeyword(const Value: string);
begin
  fDialog.HelpKeyword := Value;
  if Value = '' then
    fDialog.Options := fDialog.Options - [boShowHelp]
  else
    fDialog.Options := fDialog.Options + [boShowHelp];
end;

procedure TBrowseForFolderDlg.SetMakeFolderBtnVisible(const Value: Boolean);
begin
  if Value = GetMakeFolderBtnVisible then
    Exit;
  if Value then
    fDialog.Options := fDialog.Options - [boHideMakeFolderBtn]
  else
    fDialog.Options := fDialog.Options + [boHideMakeFolderBtn];
end;

procedure TBrowseForFolderDlg.SetTitle(const Value: string);
begin
  fDialog.Title := Value;
end;

end.

