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
 * Implements a dialogue box that accesses program update web service and
 * reports if a new version of CodeSnip is available.
}


unit FmProgramUpdatesDlg;


interface


uses
  // Delphi
  StdCtrls, Controls, ExtCtrls, Classes,
  // Project
  FmGenericViewDlg, UBaseObjects, UUpdateMgr, Web.UProgramUpdateMgr;


type
  TProgramUpdatesDlg = class(TGenericViewDlg, INoPublicConstruct)
    lblProgram: TLabel;
    btnProgUpdate: TButton;
    lblPreReleaseMsg: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnProgUpdateClick(Sender: TObject);
  strict private
    var
      fDBUpdateMgr: TUpdateMgr;
      fProgUpdateMgr: TProgramUpdateMgr;
    procedure CheckProgramUpdates;
  strict protected
    ///  <summary>Triggers checks for updates.</summary>
    ///  <remarks>Called from ancestor class.</remarks>
    procedure AfterShowForm; override;
    procedure ArrangeForm; override;
  public
    class procedure Execute(AOwner: TComponent);
  end;


implementation


uses
  // Delphi
  SysUtils, Forms, ExtActns,
  // Project
  UAppInfo, UCtrlArranger, Web.UInfo;

{$R *.dfm}


{ TCheckUpdatesDlg }

resourcestring
  sChecking = 'Checking...';
  sDBNeedsUpdating = 'An update to the online database is available.';
  sDBUpToDate = 'The online database is up to date.';
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
    BrowseAction.URL := TWebInfo.ProgramDownloadURL;
    BrowseAction.Execute;
  finally
    BrowseAction.Free;
  end;
end;

procedure TProgramUpdatesDlg.CheckProgramUpdates;
var
  Version: string;
begin
  btnProgUpdate.Visible := False;
  lblProgram.Caption := sChecking;
  Application.ProcessMessages;
  if not not fProgUpdateMgr.IsLatest then
  begin
    Version := fProgUpdateMgr.LatestProgramVersion;
    lblProgram.Caption := Format(sProgNeedsUpdating, [Version]);
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
  fDBUpdateMgr := TUpdateMgr.Create(TAppInfo.AppDataDir);
  fProgUpdateMgr := TProgramUpdateMgr.Create;
end;

procedure TProgramUpdatesDlg.FormDestroy(Sender: TObject);
begin
  fProgUpdateMgr.Free;
  fDBUpdateMgr.Free;
  inherited;
end;

end.

