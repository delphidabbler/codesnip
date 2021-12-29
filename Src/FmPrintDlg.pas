{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2007-2020, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Implements a print dialogue box.
}


unit FmPrintDlg;


interface


uses
  // Delphi
  Controls, StdCtrls, ExtCtrls, Classes, ImgList, Windows,
  // Project
  FmGenericOKDlg;


type

  {
  TPrintDlg:
    Class that implements a print dialog box.
  }
  TPrintDlg = class(TGenericOKDlg)
    btnPreferences: TButton;
    btnSetup: TButton;
    cbPrinters: TComboBox;
    chkSyntaxPrint: TCheckBox;
    chkUseColor: TCheckBox;
    gpOptions: TGroupBox;
    gpPrinter: TGroupBox;
    ilPrinters: TImageList;
    lblPrinters: TLabel;
    procedure btnOKClick(Sender: TObject);
    procedure btnPrefencesClick(Sender: TObject);
    procedure btnSetupClick(Sender: TObject);
    procedure cbPrintersDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure FormCreate(Sender: TObject);
  strict private
    procedure PopulatePrinterList;
      {Stores name of each installed printer in combo box and selects default
      printer.
      }
    procedure InitOptions;
      {Initialises options controls to default values.
      }
  strict protected
    procedure ArrangeForm; override;
      {Positions Page Setup button to right of window and vertically level with
      OK, Cancel and Help buttons.
      }
    procedure InitForm; override;
      {Initialises dialog box controls.
      }
  public
    class function Execute(const AOwner: TComponent): Boolean;
      {Displays print dialog box and gets information entered by user.
        @param AOwner [in] Owner of dialog box.
        @return True if user OKs dialog box and False if user cancels.
      }
  end;


implementation


uses
  // Delphi
  Printers, Graphics,
  // Project
  FmPreferencesDlg, FrPrintingPrefs, UClassHelpers, UConsts, UMessageBox,
  UPageSetupDlgMgr, UPrintInfo, UStructs, UStrUtils;


{$R *.dfm}


{ TPrintDlg }

procedure TPrintDlg.ArrangeForm;
  {Positions Page Setup button to right of window and vertically level with OK,
  Cancel and Help buttons.
  }
begin
  inherited;
  btnPreferences.Left := 8;
  btnPreferences.Top := btnOK.Top;
end;

procedure TPrintDlg.btnOKClick(Sender: TObject);
  {OK button click handler. Records results of user input.
    @param Sender [in] Not used.
  }
var
  Options: TPrintOptions;   // printing options
begin
  inherited;
  // Record user options
  Options := [];
  if chkUseColor.Checked then
    Include(Options, poUseColor);
  if chkSyntaxPrint.Checked then
    Include(Options, poSyntaxPrint);
  PrintInfo.PrintOptions := Options;
  // Update selected printer
  if cbPrinters.ItemIndex >= 0 then
    Printer.PrinterIndex := cbPrinters.ItemIndex;
end;

procedure TPrintDlg.btnPrefencesClick(Sender: TObject);
  {Default Options button click handler. Display preferences dialog with only
  Printing page visible and updates checkboxes and printer info to reflect any
  change in settings.
    @param Sender [in] Not used.
  }
resourcestring
  // Prompt for query dialog when preferences accepted
  sQuery = 'Do want to apply these preferences now?' + EOL2
    + 'Click Yes to apply your preferences now.' + EOL
    + 'Click No to apply your preferences the next time the program starts.';
  // Message displayed when preferences are not to be applied yet
  sMessage = 'Your preferences will take effect the next time you start the '
    + 'application';
begin
  if TPreferencesDlg.Execute(
    Self,
    [TPrintingPrefsFrame],
    TPrintingPrefsFrame.MakeFrameFlag(TPrintingPrefsFrame.HideRestartMessage)
  ) then
  begin
    if TMessageBox.Confirm(Self, sQuery) then
    begin
      PrintInfo.LoadDefaults;
      InitOptions;
    end
    else
      TMessageBox.Information(Self, sMessage);
  end;
end;

procedure TPrintDlg.btnSetupClick(Sender: TObject);
  {Positions Page Setup button to right of window and vertically level with OK,
  Cancel and Help buttons.
    @param Sender [in] Not used.
  }
begin
  TPageSetupDlgMgr.Execute(Self);
end;

procedure TPrintDlg.cbPrintersDrawItem(Control: TWinControl; Index: Integer;
  Rect: TRect; State: TOwnerDrawState);
  {Handles draw item event for printers combo box. We display printer name and
  a glyph indicating if default printer.
    @param Control [in] Reference to combo box.
    @param Index [in] Index of list item being drawn.
    @param Rect [in] Bounding rectangle of list item being drawn.
    @param State [in] State of control: focussed, selected etc.
  }

  // ---------------------------------------------------------------------------
  function IsDefaultPrinter(const PrnName: string): Boolean;
    {Checks if named printer is default.
      @param PrnName [in] Name of printer to be checked.
      @return True if printer is default, False if not.
    }
  var
    OldPrinterIdx: Integer; // saves current printer
  begin
    try
      // Save currently selected printer
      OldPrinterIdx := Printer.PrinterIndex;
      try
        // Check printer name against default
        Printer.PrinterIndex := -1; // this selects default printer
        Result := StrSameText(PrnName, Printer.Printers[Printer.PrinterIndex]);
      finally
        // Restore selected printer index
        Printer.PrinterIndex := OldPrinterIdx;
      end;
    except
      on EPrinter do
        // Checks for exception: will be because no default printer available
        // fixes bug 2875857
        Result := False;
    end;
  end;
  // ---------------------------------------------------------------------------

var
  Combo: TComboBox;   // reference to combo box
  Canvas: TCanvas;    // reference to combo's canvas
  ItemRect: TRectEx;  // rectangle bounding the item being drawn
  ImgRect: TRectEx;   // rectangle bounding the printer glyph
  TxtRect: TRect;     // rectangle bounding the printer name text
  PrnName: string;    // name of printer being displayed
  TextH: Integer;     // height of printer name text in combo canvas
const
  // Map of normal and default printer images indexes in image list.
  PrinterImgIds: array[Boolean] of Integer = (24, 40);
begin
  // Record various useful values
  Combo := Control as TComboBox;
  PrnName := Combo.Items[Index];
  Canvas := Combo.Canvas;
  ItemRect := Rect;

  // Set font style: bold if printer is default
  if IsDefaultPrinter(PrnName) then
    Canvas.Font.Style := [fsBold]
  else
    Canvas.Font.Style := [];

  // Erase background
  Canvas.Pen.Color := Combo.Color;
  Canvas.Brush.Color := Combo.Color;
  Canvas.FillRect(ItemRect);

  // Draw any highlighting
  if (odSelected in State) then
  begin
    // selected: draw highlight in highlight colour
    Canvas.Brush.Color := clHighlight;
    Canvas.Font.Color := clHighlightText;
    Canvas.Rectangle(ItemRect);
  end
  else
    // no highlighting: just ensure font colour correct
    Canvas.Font.Color := Combo.Font.Color;

  // Draw printer glyph
  ImgRect := TRectEx.CreateBounds(
    ItemRect.Left + 2,
    ItemRect.Top + (ItemRect.Height - ilPrinters.Height) div 2,
    ilPrinters.Width,
    ilPrinters.Height
  );
  // standard printer glyph is at index 0: Ord(False)
  // default printer glyph is at index 1: Ord(True)
  ilPrinters.Draw(
    Canvas, ImgRect.Left, ImgRect.Top, PrinterImgIds[IsDefaultPrinter(PrnName)]
  );

  // Draw printer name
  TextH := Canvas.TextHeight(PrnName);
  TxtRect := TRectEx.Create(
    ImgRect.Right + 4,
    ItemRect.Top + (ItemRect.Height - TextH) div 2,
    ItemRect.Right - 2,
    ItemRect.Top + (ItemRect.Height - TextH) div 2 + TextH
  );
  Canvas.TextRect(TxtRect, PrnName, [tfLeft, tfEndEllipsis, tfTop]);
end;

class function TPrintDlg.Execute(const AOwner: TComponent): Boolean;
  {Displays print dialog box and gets information entered by user.
    @param AOwner [in] Owner of dialog box.
    @return True if user OKs dialog box and False if user cancels.
  }
begin
  with Create(AOwner) do
    try
      Result := ShowModal = mrOK;
    finally
      Free;
    end;
end;

procedure TPrintDlg.FormCreate(Sender: TObject);
begin
  inherited;
  ilPrinters.LoadFromResource(RT_RCDATA, 'ACTIONIMAGES', 16, clFuchsia);
end;

procedure TPrintDlg.InitForm;
  {Initialises dialog box controls.
  }
begin
  inherited;
  PopulatePrinterList;
  InitOptions;
end;

procedure TPrintDlg.InitOptions;
  {Initialises options controls to default values.
  }
begin
  chkUseColor.Checked := poUseColor in PrintInfo.PrintOptions;
  chkSyntaxPrint.Checked := poSyntaxPrint in PrintInfo.PrintOptions;
end;

procedure TPrintDlg.PopulatePrinterList;
  {Stores name of each installed printer in combo box and selects default
  printer.
  }
var
  PrinterName: string;
begin
  for PrinterName in Printer.Printers do
    cbPrinters.Items.Add(PrinterName);
  try
    cbPrinters.ItemIndex := Printer.PrinterIndex;
  except
    on EPrinter do
      // Exception can occur if no default printer: part of fix for bug 2875857
      cbPrinters.ItemIndex := -1;
  end;
end;

end.

