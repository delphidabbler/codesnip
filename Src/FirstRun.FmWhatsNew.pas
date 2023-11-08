{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2020-2023, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Implements dialogue box that may be displayed the first time CodeSnip 4.x.x
 * is run after an update. The dialogue box displays a HTML page that draws
 * attention to key changes in the updated version of CodeSnip.
}


unit FirstRun.FmWhatsNew;

interface


uses
  // VCL
  Controls,
  Forms,
  StdCtrls,
  ExtCtrls,
  Classes,
  // Project
  FirstRun.FmWhatsNew.FrHTML,
  FmGenericViewDlg,
  FrBrowserBase,
  IntfAligner,
  UBaseObjects;


type
  ///  <summary>Dialogue box that may be displayed the first time CodeSnip 4.x.x
  ///  is run after an update. The dialogue box displays a HTML page that draws
  ///  attention to key changes in the updated version of CodeSnip.</summary>
  TWhatsNewDlg = class(TGenericViewDlg, INoPublicConstruct)
    frmHTML: TWhatsNewHTMLFrame;
  strict private
    type
      ///  <summary>Custom form aligner class for wizard.</summary>
      TAligner = class(TInterfacedObject, IFormAligner)
      public
        ///  <summary>Aligns dialogue box at centre of primary monitor.
        ///  </summary>
        procedure AlignForm(const AForm: TCustomForm);
      end;
  strict protected
    ///  <summary>Returns instance of form aligner object.</summary>
    function GetAligner: IFormAligner; override;
    ///  <summary>Sets form caption and loads HTML to be displayed from
    ///  resources.</summary>
    procedure ConfigForm; override;
    ///  <summary>Sets size of the HTML section of the display and aranges other
    ///  controls around that.</summary>
    procedure ArrangeForm; override;
    ///  <summary>Modifies window creation parameters to ensure the dialgue box
    ///  displays a button in the task bar.</summary>
    ///  <remarks>This is necessary because the dialogue box is displayed before
    ///  CodeSnip's main window is shown, so there is no suitable button
    ///  displayed yet.</remarks>
    procedure CreateParams(var Params: TCreateParams); override;
  public
    ///  <summary>Displays dialogue box with given owner.</summary>
    class procedure Execute(AOwner: TComponent);
  end;


implementation


uses
  // VCL
  SysUtils,
  Math,
  Windows,
  // Project
  UAppInfo,
  UConsts,
  UEncodings,
  UResourceUtils,
  UStructs;

{$R *.dfm}


{ TWhatsNewDlg }

procedure TWhatsNewDlg.ArrangeForm;
const
  cBodyWidth = 500;
  cBodyHeight = 450;
begin
  pnlBody.Width := cBodyWidth;
  pnlBody.Height := cBodyHeight;
  inherited;
end;

procedure TWhatsNewDlg.ConfigForm;
resourcestring
  sDlgTitle = 'Welcome to CodeSnip v%s';
begin
  Caption := Format(sDlgTitle, [TAppInfo.ProgramReleaseVersion]);
  frmHTML.Initialise(
    LoadResourceAsString(HInstance, 'dlg-whatsnew.html', RT_HTML, etUTF8)
  );
end;

procedure TWhatsNewDlg.CreateParams(var Params: TCreateParams);
begin
  inherited;
  Params.ExStyle := Params.ExStyle OR WS_EX_APPWINDOW;
end;

class procedure TWhatsNewDlg.Execute(AOwner: TComponent);
var
  Dlg: TWhatsNewDlg;
begin
  Dlg := InternalCreate(AOwner);
  try
    Dlg.ShowModal;
  finally
    Dlg.Free;
  end;
end;

function TWhatsNewDlg.GetAligner: IFormAligner;
begin
  Result := TAligner.Create;
end;

{ TWhatsNewDlg.TAligner }

procedure TWhatsNewDlg.TAligner.AlignForm(const AForm: TCustomForm);
var
  WorkArea: TRectEx;
begin
  // This form is designed for display centred on desktop, so assume it fits
  WorkArea := Screen.WorkAreaRect;
  AForm.Left := WorkArea.Left + (WorkArea.Width - AForm.Width) div 2;
  AForm.Top := WorkArea.Top + (WorkArea.Height - AForm.Height) div 2;
end;

end.

