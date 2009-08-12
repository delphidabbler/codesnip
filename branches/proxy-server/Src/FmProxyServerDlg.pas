{
 * FmProxyServerDlg.pas
 *
 * Implements a dialog box that enables users to specify (or remove) a proxy
 * server for use by CodeSnip's web services.
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
 * The Original Code is FmProxyServerDlg.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2009 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
}


unit FmProxyServerDlg;

// todo: make help button visible and add help topic
// todo: add button to test proxy
// todo: reduce uses clauses
// todo: comment methods

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, FmGenericOKDlg, StdCtrls, ExtCtrls;

type
  TProxyServerDlg = class(TGenericOKDlg)
    cbUseProxy: TCheckBox;
    gbProxy: TGroupBox;
    lblIPAddress: TLabel;
    lblPort: TLabel;
    lblUserName: TLabel;
    lblPassword1: TLabel;
    lblPassword2: TLabel;
    edIPAddress: TEdit;
    edPort: TEdit;
    edUserName: TEdit;
    edPassword1: TEdit;
    edPassword2: TEdit;
    lblIPAddressReq: TLabel;
    lblPortReq: TLabel;
    lblReqSymbol: TLabel;
    lblReqExplain: TLabel;
    procedure btnOKClick(Sender: TObject);
    procedure edIPAddressKeyPress(Sender: TObject; var Key: Char);
    procedure edPortKeyPress(Sender: TObject; var Key: Char);
    procedure cbUseProxyClick(Sender: TObject);
  strict private
    procedure Validate;
    procedure SaveData;
    procedure SetProxyCtrlState(const Flag: Boolean);
  strict protected
    procedure ConfigForm; override;
    procedure InitForm; override;
  public
    class function Execute(const AOwner: TComponent): Boolean;
  end;


implementation


uses
  // Project
  UConsts, UExceptions, UFontHelper, UMessageBox, USettings, UStructs,
  USystemInfo, UUtils;

{$R *.dfm}

{ TProxyServerDlg }

procedure TProxyServerDlg.btnOKClick(Sender: TObject);
begin
  try
    ModalResult := mrNone;
    Validate;
    SaveData;
    ModalResult := mrOK;
  except
    on E: EDataEntry do
    begin
      if Assigned(E.Ctrl) then
      begin
        E.Ctrl.SetFocus;
        TMessageBox.Error(Self, E.Message);
      end;
    end;
  end;
end;

procedure TProxyServerDlg.cbUseProxyClick(Sender: TObject);
begin
  inherited;
  SetProxyCtrlState(cbUseProxy.Checked);
end;

procedure TProxyServerDlg.ConfigForm;
begin
  inherited;
  TFontHelper.SetDefaultBaseFont(lblIPAddressReq.Font, False);
  TFontHelper.SetDefaultBaseFont(lblPortReq.Font, False);
  TFontHelper.SetDefaultBaseFont(lblReqSymbol.Font, False);
  if TOSInfo.IsVistaOrLater then
  begin
    edPassword1.PasswordChar := #149;
    edPassword2.PasswordChar := #149;
  end
  else
  begin
    edPassword1.PasswordChar := '*';
    edPassword2.PasswordChar := '*';
  end;
end;

procedure TProxyServerDlg.edIPAddressKeyPress(Sender: TObject; var Key: Char);
begin
  if not (Key in ['0'..'9', '.', BACKSPACE]) then
    Key := #0;
  if (Key = '.') and (CountDelims(edIPAddress.Text, '.') = 3) then
    Key := #0;
  if Key = #0 then
    MessageBeep(Cardinal(-1));
end;

procedure TProxyServerDlg.edPortKeyPress(Sender: TObject; var Key: Char);
begin
  if not (Key in ['0'..'9', BACKSPACE]) then
    Key := #0;
  if Key = #0 then
    MessageBeep(Cardinal(-1));
end;

class function TProxyServerDlg.Execute(const AOwner: TComponent): Boolean;
begin
  with Create(AOwner) do
    try
      Result := ShowModal = mrOK;
    finally
      Free;
    end;
end;

procedure TProxyServerDlg.InitForm;
var
  Section: ISettingsSection;
begin
  inherited;
  // init control contents from proxy server settings
  Section := Settings.ReadSection(ssProxyServer);
  cbUseProxy.Checked := Boolean(StrToIntDef(Section.ItemValues['UseProxy'], 0));
  edIPAddress.Text := Section.ItemValues['IPAddress'];
  edPort.Text := Section.ItemValues['Port'];
  edUserName.Text := Section.ItemValues['UserName'];
  edPassword1.Text := Section.GetEncryptedItemValue('Password');
  edPassword2.Text := edPassword1.Text;
  // init control state
  SetProxyCtrlState(cbUseProxy.Checked);
end;

procedure TProxyServerDlg.SaveData;
var
  Section: ISettingsSection;
begin
  Section := Settings.EmptySection(ssProxyServer);
  Section.ItemValues['UseProxy'] := IntToStr(Ord(cbUseProxy.Checked));
  Section.ItemValues['IPAddress'] := edIPAddress.Text;
  Section.ItemValues['Port'] := edPort.Text;
  Section.ItemValues['UserName'] := edUserName.Text;
  Section.SetEncryptedItemValue('Password', edPassword1.Text);
  Section.Save;
end;

procedure TProxyServerDlg.SetProxyCtrlState(const Flag: Boolean);
var
  Idx: Integer;
begin
  for Idx := 0 to Pred(gbProxy.ControlCount) do
    gbProxy.Controls[Idx].Enabled := Flag;
  gbProxy.Enabled := Flag;
end;

procedure TProxyServerDlg.Validate;

  function IsValidIPAddress(const Addr: string): Boolean;
    {Checks if an IP address has valid format.
      @param Addr [in] IP address to check.
      @return True if IP address has valid format, False otherwise.
    }
  var
    Quads: TStringList;   // quads of IP address
    Quad: string;         // a quad
    IQuad: Integer;       // integer value of a quad
  const
    IPQuadRange: TRange = (Min: 0; Max: 255); // range of valid IP quads
  begin
    Result := False;
    Quads := TStringList.Create;
    try
      // split IP address into quads (they are separated by dots)
      ExplodeStr(Addr, '.', Quads);
      if Quads.Count <> 4 then
        Exit;   // must be 4 quads
      for Quad in Quads do
      begin
        if not TryStrToInt(Quad, IQuad) then
          Exit; // each quad must be an integer
        if not IPQuadRange.Contains(IQuad) then
          Exit; // each quad must be storable in a byte
      end;
    finally
      FreeAndNil(Quads);
    end;
    Result := True;
  end;

  function IsValidPort(const Port: string): Boolean;
    {Checks if a port is valid.
      @param Port [in] String representation of port to be checked.
      @return True if port is valid, False otherwise.
    }
  var
    IPort: Integer; // port nunber as integer
  const
    PortNumbers: TRange = (Min: 0; Max: 65535);  // range of valid port numbers
  begin
    Result := False;
    // check port is a valid number
    if not TryStrToInt(Port, IPort) then
      Exit;
    // check port in range (see http://www.iana.org/assignments/port-numbers)
    if not PortNumbers.Contains(IPort) then
      Exit;
    Result := True;
  end;

resourcestring
  // Error messages
  sMissingIPAddress = 'IP address must be specified';
  sInvalidIPAddress = 'Invalid IP address';
  sMissingPort = 'Port must be specified';
  sInvalidPort = 'Port must be in range 0..65535';
  sMissingUserName = 'User name must be specified';
  sPasswordMismatch = 'Passwords are not the same. Please re-enter.';
begin
  if not cbUseProxy.Checked then
    Exit;
  if edIPAddress.Text = '' then
    raise EDataEntry.Create(sMissingIPAddress, edIPAddress);
  if not IsValidIPAddress(edIPAddress.Text) then
    raise EDataEntry.Create(sInvalidIPAddress, edIPAddress);
  if edPort.Text = '' then
    raise EDataEntry.Create(sMissingPort, edPort);
  if not IsValidPort(edPort.Text) then
    raise EDataEntry.Create(sInvalidPort, edPort);
  if (edPassword1.Text <> '') and (edUserName.Text = '') then
    raise EDataEntry.Create(sMissingUserName, edUserName);
  if edPassword1.Text <> edPassword2.Text then
    raise EDataEntry.Create(sPasswordMismatch, edPassword1);
end;

end.

