{
 * UBrowseForFolderDlg.pas
 *
 * Implements a class that displays a browse for folder dialog box.
 *
 * $Rev$
 * $Date$
 *
 * ***** BEGIN LICENSE BLOCK *****
 *
 * Version: MPL 1.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with the
 * License. You may obtain a copy of the License at http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
 * the specific language governing rights and limitations under the License.
 *
 * The Original Code is UBrowseForFolderDlg.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2011 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
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
  ///  Displays browse for folder dialog box aligned over a given host control.
  ///  </summary>
  ///  <remarks>
  ///  Wraps a TPJBrowseDialog component.
  ///  </remarks>
  TBrowseForFolderDlg = class(TObject)
  strict private
    var
      ///  <summary>Reference to dialog box component.</summary>
      fDialog: TPJBrowseDialog;
      ///  <summary>Reference to component above which dialog box is to be
      ///  aligned.</summary>
      fHost: TComponent;
  strict private
    ///  <summary>Initialises properties and event handlers.</summary>
    procedure InitProperties;
    ///  <summary>Handles dialog's OnInitialise event. Aligns dialog box over
    ///  parent form.</summary>
    procedure DlgInitHandler(Sender: TObject);
    ///  <summary>Handles dialog's OnHelp event. Displays help using keyword
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
    ///  <summary>Setter for Headline property.</summary>
    procedure SetHeadline(const Value: string);
    ///  <summary>Setter for Title property.</summary>
    procedure SetTitle(const Value: string);
    ///  <summary>Setter for HelpKeyword property.</summary>
    procedure SetHelpKeyword(const Value: string);
  public
    ///  <summary>Object constructor. Sets up object and initialise dialog box.
    ///  </summary>
    ///  <param name="AHost">TComponent [in] Reference to component over which
    ///  the dialog box will be aligned.</param>
    ///  <remarks>If AHost is nil the screen's active form is used if available
    ///  otherwise application's main form is used.</remarks>
    constructor Create(AHost: TComponent);
    ///  <summary>Object destructor. Tidies up object.</summary>
    destructor Destroy; override;
    ///  <summary>Displays dialog box and returns True if user OKs, False if
    ///  user cancels.</summary>
    function Execute: Boolean;
    ///  <summary>Text display in dialog window caption.</summary>
    property Title: string read GetTitle write SetTitle;
    ///  <summary>Heading line display in dialog box.</summary>
    property Headline: string read GetHeadline write SetHeadline;
    ///  <summary>Name of folder selected in dialog box.</summary>
    property FolderName: string read GetFolderName;
    ///  <summary>Help keyword. If present dialog box displays a help button. If
    ///  '' then no help button is displayed.</summary>
    property HelpKeyword: string read GetHelpKeyword write SetHelpKeyword;
  end;


implementation


uses
  // Delphi
  Controls, Windows, Forms,
  // Project
  UHelpMgr, UStructs;


{ TBrowseForFolderDlg }

constructor TBrowseForFolderDlg.Create(AHost: TComponent);
begin
  inherited Create;
  fHost := AHost;
  if not Assigned(fHost) then
  begin
    if Assigned(Screen.ActiveCustomForm) then
      fHost := Screen.ActiveCustomForm
    else if Assigned(Application.MainForm) then
      fHost := Application.MainForm;
  end;
  Assert(Assigned(fHost), ClassName + '.Create: AOwner is nil');
  fDialog := TPJBrowseDialog.Create(nil);
  InitProperties;
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
var
  WP: TWindowPlacement;
  X, Y: Integer;
  OwnerBounds, DlgBounds: TRectEx;
begin
  if not (fHost is TWinControl) then
    Exit;
  GetWindowPlacement(
    fDialog.Handle,
    WP
  );
  OwnerBounds := (fHost as TWinControl).BoundsRect;
  DlgBounds := WP.rcNormalPosition;
  X := OwnerBounds.Left + 40;
  Y := OwnerBounds.Top + 40;
  SetWindowPos(
    fDialog.Handle,
    0,
    X, Y,
    0, 0,
    SWP_NOSIZE or SWP_NOZORDER
  );
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

function TBrowseForFolderDlg.GetTitle: string;
begin
  Result := fDialog.Title;
end;

procedure TBrowseForFolderDlg.InitProperties;
begin
  fDialog.OnInitialise := DlgInitHandler;
  fDialog.OnHelp := DlgHelpHandler;
  fDialog.Options := [boDirsOnly];  // using old style dialog
  fDialog.HelpType := htKeyword;
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

procedure TBrowseForFolderDlg.SetTitle(const Value: string);
begin
  fDialog.Title := Value;
end;

end.
