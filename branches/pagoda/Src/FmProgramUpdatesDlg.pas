{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2012-2014, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * Implements a dialogue box that accesses program update web service and
 * reports if a new version of CodeSnip is available.
}


unit FmProgramUpdatesDlg;


interface


uses
  // Delphi
  StdCtrls, Controls, ExtCtrls, Classes,
  // Project
  FmGenericViewDlg, UBaseObjects, UProgramUpdateChecker;


type
  ///  <summary>Dialogue box that checks if a program update is available and
  ///  reports the result, providing a link to download any available update.
  ///  </summary>
  ///  <remarks>This dialogue box indirectly accesses the CodeSnip program
  ///  update we service to get the update information.</remarks>
  TProgramUpdatesDlg = class(TGenericViewDlg, INoPublicConstruct)
    lblProgram: TLabel;
    btnProgUpdate: TButton;
    lblPreReleaseMsg: TLabel;
    ///  <summary>Handles form creation event. Creates owned objects.</summary>
    procedure FormCreate(Sender: TObject);
    ///  <summary>Handles form destruction event. Frees owned objects.</summary>
    procedure FormDestroy(Sender: TObject);
    ///  <summary>Handles clicks on "Download Now" button by displaying, in the
    ///  default browser, the web page from where the latest version of CodeSnip
    ///  can be downloaded.</summary>
    procedure btnProgUpdateClick(Sender: TObject);
  strict private
    var
      ///  <summary>Object that checks whether an update is available and
      ///  provides information about it if so.</summary>
      fUpdateChecker: TProgramUpdateChecker;
    ///  <summary>Checks if a program update is available and updates the UI
    ///  according to the result.</summary>
    procedure CheckProgramUpdates;
  strict protected
    ///  <summary>Triggers checks for updates after the forma is displayed.
    ///  </summary>
    ///  <remarks>Called from ancestor class.</remarks>
    procedure AfterShowForm; override;
    ///  <summary>Arranges the controls and sizes the dialogue box.</summary>
    ///  <remarks>Called from ancestor class.</remarks>
    procedure ArrangeForm; override;
    ///  <summary>Initialises form's controls.</summary>
    ///  <remarks>Called from ancestor class.</remarks>
    procedure InitForm; override;
  public
    ///  <summary>Displays the dialogue box, aligned over the given owner
    ///  control.</summary>
    ///  <remarks>The check for program updates begins as soon as the dialogue
    ///  box appears on-screen, without any user interaction being required to
    ///  start it.</remarks>
    class procedure Execute(AOwner: TComponent);
  end;


implementation


uses
  // Delphi
  SysUtils, Forms, ExtActns, Graphics,
  // Project
  UCtrlArranger;

{$R *.dfm}


{ TCheckUpdatesDlg }

resourcestring
  sChecking = 'Checking...';
  sProgNeedsUpdating = 'CodeSnip version %s is available.';
  sProgUpToDate = 'CodeSnip is up to date.';

procedure TProgramUpdatesDlg.AfterShowForm;
begin
  Screen.Cursor := crHourGlass;
  try
    CheckProgramUpdates;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TProgramUpdatesDlg.ArrangeForm;
begin
  TCtrlArranger.SetLabelHeight(lblPreReleaseMsg);
  TCtrlArranger.MoveBelow(lblProgram, btnProgUpdate, 12);
  TCtrlArranger.MoveBelow(lblProgram, lblPreReleaseMsg, 8);
  pnlBody.ClientHeight := TCtrlArranger.TotalControlHeight(pnlBody) + 8;
  TCtrlArranger.AlignLefts([lblProgram, btnProgUpdate, lblPreReleaseMsg], 0);
  pnlBody.Width := TCtrlArranger.TotalControlWidth(pnlBody);
  inherited;
end;

procedure TProgramUpdatesDlg.btnProgUpdateClick(Sender: TObject);
var
  BrowseAction: TBrowseURL; // action that displays RSS feed URL in browser
begin
  BrowseAction := TBrowseURL.Create(nil);
  try
    BrowseAction.URL := fUpdateChecker.DownloadURL;
    BrowseAction.Execute;
  finally
    BrowseAction.Free;
  end;
end;

procedure TProgramUpdatesDlg.CheckProgramUpdates;
begin
  btnProgUpdate.Visible := False;
  lblProgram.Caption := sChecking;
  Application.ProcessMessages;
  fUpdateChecker.Execute;
  if fUpdateChecker.IsUpdateAvailable then
  begin
    lblProgram.Caption := Format(
      sProgNeedsUpdating, [string(fUpdateChecker.LatestVersion)]
    );
    btnProgUpdate.Visible := True;
  end
  else
  begin
    lblProgram.Caption := sProgUpToDate;
    lblPreReleaseMsg.Visible := True;
  end;
end;

class procedure TProgramUpdatesDlg.Execute(AOwner: TComponent);
begin
  with InternalCreate(AOwner) do
    try
      ShowModal;
    finally
      Free;
    end;
end;

procedure TProgramUpdatesDlg.FormCreate(Sender: TObject);
begin
  inherited;
  fUpdateChecker := TProgramUpdateChecker.Create('Manual');
end;

procedure TProgramUpdatesDlg.FormDestroy(Sender: TObject);
begin
  fUpdateChecker.Free;
  inherited;
end;

procedure TProgramUpdatesDlg.InitForm;
begin
  inherited;
  lblPreReleaseMsg.Font.Color := clGrayText;
end;

end.

