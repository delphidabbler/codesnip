{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2009-2014, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * Implements a dialogue box that enables users to configure a proxy server for
 * use by CodeSnip's web services.
}

// TODO -cwebsvc: Remove this form unit

unit FmProxyServerDlg;


interface


uses
  // Delphi
  StdCtrls, Controls, ExtCtrls, Classes, StdActns, ActnList,
  // Project
  FmGenericOKDlg;


type

  {
  TProxyServerDlg:
    Dialogue box that enables users to specify (or remove) a proxy server for
    use by CodeSnip's web services.
  }
  TProxyServerDlg = class(TGenericOKDlg)
    cbUseProxy: TCheckBox;
    gbProxy: TGroupBox;
    lblIPAddress: TLabel;
    lblIPAddressReq: TLabel;
    lblPassword1: TLabel;
    lblPassword2: TLabel;
    lblPort: TLabel;
    lblPortReq: TLabel;
    lblReqExplain: TLabel;
    lblReqSymbol: TLabel;
    lblUserName: TLabel;
    edIPAddress: TEdit;
    edPassword1: TEdit;
    edPassword2: TEdit;
    edPort: TEdit;
    edUserName: TEdit;
    alMain: TActionList;
    actCut: TEditCut;
    actCopy: TEditCopy;
    actPaste: TEditPaste;
    actSelectAll: TEditSelectAll;
    procedure btnOKClick(Sender: TObject);
    procedure edIPAddressKeyPress(Sender: TObject; var Key: Char);
    procedure edPortKeyPress(Sender: TObject; var Key: Char);
    procedure cbUseProxyClick(Sender: TObject);
  strict private
    procedure Validate;
      {Validates data entered into controls.
      }
    procedure SaveData;
      {Stores data entered in controls in settings.
      }
    procedure SetProxyCtrlState(const Flag: Boolean);
      {Sets enabled state of all controls in "proxy server details" group box.
        @param Flag [in] True if controls are to be enabled, False if to be
          disabled.
      }
  strict protected
    procedure ConfigForm; override;
      {Configures form. Ensures correct font is used and sets password
      character.
      }
    procedure InitForm; override;
      {Initialises controls on form from values read from settings.
      }
  public
    class function Execute(const AOwner: TComponent): Boolean;
      {Creates and displays the proxy server dialogue box.
        @param AOwner [in] Component that owns the dialogue box.
      }
  end;


implementation


uses
  // Delphi
  SysUtils, Windows, Character,
  // Project
  CS.Utils.Sound,
  UConsts, UExceptions, UFontHelper, UMessageBox, USettings, UStructs,
  USystemInfo, UStrUtils;


{$R *.dfm}

{ TProxyServerDlg }

procedure TProxyServerDlg.btnOKClick(Sender: TObject);
  {Handles OK button click. Validates entered data and saves proxy information
  in settings. Handles any errors.
    @param Sender [in] Not used.
  }
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
  {Handles click on "Use proxy server" check box. Updates state of other
  controls depending on whether check box is checked.
    @param Sender [in] Not used.
  }
begin
  inherited;
  SetProxyCtrlState(cbUseProxy.Checked);
end;

procedure TProxyServerDlg.ConfigForm;
  {Configures form. Ensures correct font is used and sets password character.
  }
begin
  inherited;
  TFontHelper.SetDefaultBaseFont(lblIPAddressReq.Font);
  TFontHelper.SetDefaultBaseFont(lblPortReq.Font);
  TFontHelper.SetDefaultBaseFont(lblReqSymbol.Font);
  if TOSInfo.IsReallyWindowsVistaOrGreater then
  begin
    edPassword1.PasswordChar := '•';
    edPassword2.PasswordChar := '•';
  end
  else
  begin
    edPassword1.PasswordChar := '*';
    edPassword2.PasswordChar := '*';
  end;
end;

procedure TProxyServerDlg.edIPAddressKeyPress(Sender: TObject; var Key: Char);
  {Filters keypresses in IP address edit box. Permits only numbers or dots to be
  entered.
    @param Sender [in] Not used.
    @param Key [in/out] Key pressed. Set to #0 if key is not permitted.
  }
const
  cDot = '.';   // dot separator (not decimal point)
begin
  if not TCharacter.IsDigit(Key) and (Key <> cDot) and (Key <> BACKSPACE) then
    Key := #0
  else if (Key = cDot) and (
    (edIPAddress.SelStart = 0) or (StrCountDelims(cDot, edIPAddress.Text) = 3)
  ) then
    Key := #0;
  if Key = #0 then
    KeyErrorBeep;
end;

procedure TProxyServerDlg.edPortKeyPress(Sender: TObject; var Key: Char);
  {Filters keypresses in Port edit box. Permits only numbers to be entered.
    @param Sender [in] Not used.
    @param Key [in/out] Key pressed. Set to #0 if key is not permitted.
  }
begin
  if not TCharacter.IsDigit(Key) and (Key <> BACKSPACE) then
    Key := #0;
  if Key = #0 then
    KeyErrorBeep;
end;

class function TProxyServerDlg.Execute(const AOwner: TComponent): Boolean;
  {Creates and displays the proxy server dialogue box.
    @param AOwner [in] Component that owns the dialogue box.
  }
begin
  with Create(AOwner) do
    try
      Result := ShowModal = mrOK;
    finally
      Free;
    end;
end;

procedure TProxyServerDlg.InitForm;
  {Initialises controls on form from values read from settings.
  }
var
  Section: ISettingsSection;  // settings section containing current settings
begin
  inherited;
  // init control contents from proxy server settings
  Section := Settings.ReadSection(ssProxyServer);
  cbUseProxy.Checked := Section.GetBoolean('UseProxy', False);
  edIPAddress.Text := Section.GetString('IPAddress');
  edPort.Text := Section.GetString('Port');
  edUserName.Text := Section.GetString('UserName');
  edPassword1.Text := Section.GetEncryptedString('Password');
  edPassword2.Text := edPassword1.Text;
  // init control state
  SetProxyCtrlState(cbUseProxy.Checked);
end;

procedure TProxyServerDlg.SaveData;
  {Stores data entered in controls in settings.
  }
var
  Section: ISettingsSection;  // settings section to receive data
begin
  Section := Settings.EmptySection(ssProxyServer);
  Section.SetBoolean('UseProxy', cbUseProxy.Checked);
  Section.SetString('IPAddress', edIPAddress.Text);
  Section.SetString('Port', edPort.Text);
  Section.SetString('UserName', edUserName.Text);
  Section.SetEncryptedString('Password', edPassword1.Text);
  Section.Save;
end;

procedure TProxyServerDlg.SetProxyCtrlState(const Flag: Boolean);
  {Sets enabled state of all controls in "proxy server details" group box.
    @param Flag [in] True if controls are to be enabled, False if to be
      disabled.
  }
var
  Idx: Integer; // loops through all child controls of group box
begin
  for Idx := 0 to Pred(gbProxy.ControlCount) do
    gbProxy.Controls[Idx].Enabled := Flag;
  gbProxy.Enabled := Flag;
end;

procedure TProxyServerDlg.Validate;
  {Validates data entered into controls.
  }

  // ---------------------------------------------------------------------------
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
      StrExplode(Addr, '.', Quads);
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
      Quads.Free;
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
  // ---------------------------------------------------------------------------

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

