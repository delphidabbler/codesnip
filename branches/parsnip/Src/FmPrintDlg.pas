{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2007-2014, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * Implements a print dialogue box.
}


unit FmPrintDlg;


interface


uses
  // Delphi
  Controls,
  StdCtrls,
  ExtCtrls,
  Classes,
  ImgList,
  Windows,
  ComCtrls,
  Forms,
  // Project
  CS.SourceCode.Hiliter.Themes,
  FmGenericOKDlg,
  FrRTFShowCase,
  URTFBuilder;


type

  {
  TPrintDlg:
    Class that implements a print dialogue box.
  }
  TPrintDlg = class(TGenericOKDlg)
    btnSetup: TButton;
    cbPrinters: TComboBox;
    gpPrinter: TGroupBox;
    ilPrinters: TImageList;
    lblPrinters: TLabel;
    gpFormatOptions: TGroupBox;
    chkSyntaxHighlight: TCheckBox;
    chkUseColor: TCheckBox;
    frmPreview: TRTFShowCaseFrame;
    procedure btnOKClick(Sender: TObject);
    procedure btnSetupClick(Sender: TObject);
    procedure cbPrintersDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure FormCreate(Sender: TObject);
    procedure FormatCheckBoxClick(Sender: TObject);
  strict private
    type
      {
      TDocFormatPreview
        Class that renders a preview of effect of document formatting changes
        on a printed document.
      }
      TDocFormatPreview = class(TObject)
      strict private
        fRE: TRichEdit;
          {Reference to richedit control used to render preview}
        procedure HiliteSource(const UseColour, SyntaxHighlight: Boolean;
          Builder: TRTFBuilder);
          {Generates sample highlighted source code.
            @param UseColour [in] Whether to use colour or mono highlighter.
            @param SyntaxHighlight [in] Whether source code to be highlighted.
            @param Builder [in] Object that receives highlighted source code.
          }
      public
        constructor Create(const RE: TRichEdit);
          {Class constructor. Sets up object.
            @param RE [in] Rich edit control used to render preview.
          }
        procedure Generate(const UseColour, SyntaxHighlight: Boolean);
          {Generates RTF source code for preview.
            @param UseColour [in] Whether preview to be in colour or monochrome.
            @param SyntaxHighlight [in] Whether preview source code to be syntax
              hilited.
          }
      end;
  strict private
    procedure PopulatePrinterList;
      {Stores name of each installed printer in combo box and selects default
      printer.
      }
    procedure InitOptions;
      {Initialises options controls to default values.
      }
    procedure DisplayPreview;
      {Displays document formatting options preview.
      }
  strict protected
    procedure InitForm; override;
      {Initialises dialogue box controls.
      }
  public
    class function Execute(const AOwner: TComponent): Boolean;
      {Displays print dialogue box and gets information entered by user.
        @param AOwner [in] Owner of dialogue box.
        @return True if user OKs dialogue box and False if user cancels.
      }
  end;


implementation


uses
  // Delphi
  Printers, Graphics,
  // Project
  CS.Config,
  CS.SourceCode.Hiliter.Brushes,
  CS.SourceCode.Hiliter.Renderers,
  UClassHelpers,
  UConsts,
  UMessageBox,
  UPageSetupDlgMgr,
  UPreferences,
  UPrintInfo,
  URTFStyles,
  URTFUtils,
  UStructs,
  UStrUtils;


{$R *.dfm}


{ TPrintDlg }

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
    Include(Options, poUseColour);
  if chkSyntaxHighlight.Checked then
    Include(Options, poSyntaxHilite);
  PrintInfo.PrintOptions := Options;
  // Update selected printer
  if cbPrinters.ItemIndex >= 0 then
    Printer.PrinterIndex := cbPrinters.ItemIndex;
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
        // Fixes bug 2875857
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

procedure TPrintDlg.DisplayPreview;
  {Displays document formatting options preview.
  }
var
  Preview: TDocFormatPreview; // object that renders preview
begin
  Preview := TDocFormatPreview.Create(frmPreview.RichEdit);
  try
    Preview.Generate(chkUseColor.Checked, chkSyntaxHighlight.Checked);
  finally
    Preview.Free;
  end;
end;

class function TPrintDlg.Execute(const AOwner: TComponent): Boolean;
  {Displays print dialogue box and gets information entered by user.
    @param AOwner [in] Owner of dialogue box.
    @return True if user OKs dialogue box and False if user cancels.
  }
begin
  with Create(AOwner) do
    try
      Result := ShowModal = mrOK;
    finally
      Free;
    end;
end;

procedure TPrintDlg.FormatCheckBoxClick(Sender: TObject);
begin
  DisplayPreview;
end;

procedure TPrintDlg.FormCreate(Sender: TObject);
begin
  inherited;
  ilPrinters.LoadFromResource(RT_RCDATA, 'ACTIONIMAGES', 16, clFuchsia);
end;

procedure TPrintDlg.InitForm;
  {Initialises dialogue box controls.
  }
begin
  inherited;
  PopulatePrinterList;
  InitOptions;
  DisplayPreview;
end;

procedure TPrintDlg.InitOptions;
  {Initialises options controls to default values.
  }
begin
  chkUseColor.Checked := poUseColour in PrintInfo.PrintOptions;
  chkSyntaxHighlight.Checked := poSyntaxHilite in PrintInfo.PrintOptions;
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

{ TPrintDlg.TDocFormatPreview }

constructor TPrintDlg.TDocFormatPreview.Create(const RE: TRichEdit);
begin
  Assert(Assigned(RE), ClassName + '.Create: RE is nil');
  inherited Create;
  fRE := RE;
end;

procedure TPrintDlg.TDocFormatPreview.Generate(const UseColour,
  SyntaxHighlight: Boolean);
resourcestring
  // Heading and dummy paragraph text
  sHeading = 'Sample';
  sBodyText = 'Lorem ipsum dolor sit diam amet.';
var
  Builder: TRTFBuilder; // object used to assemble required RTF code
begin
  Builder := TRTFBuilder.Create(0); // use default code page
  try
    // Set global document font and paragraph spacing
    Builder.FontTable.Add('Tahoma', rgfSwiss, 0);
    Builder.SetParaSpacing(TRTFParaSpacing.Create(0.0, 2.0));
    // Add heading text
    Builder.BeginGroup;
    Builder.SetFontSize(10);
    Builder.SetFontStyle([fsBold]);
    Builder.AddText(sHeading);
    Builder.EndGroup;
    Builder.EndPara;
    // Add dummy paragraph
    Builder.SetFontSize(9);
    Builder.AddText(sBodyText);
    Builder.EndPara;
    // Add highlighted source code
    HiliteSource(UseColour, SyntaxHighlight, Builder);
    Builder.EndPara;
    // Load document into rich edit
    TRichEditHelper.Load(fRE, Builder.Render);
  finally
    Builder.Free;
  end;
end;

procedure TPrintDlg.TDocFormatPreview.HiliteSource(const UseColour,
  SyntaxHighlight: Boolean; Builder: TRTFBuilder);
const
  // Sample source code displayed in preview
  cSourceCode = 'procedure Foo;' + EOL
    + 'begin' + EOL
    + '  // sample comment' + EOL
    + '  ShowMessage(''Bar'');' + EOL
    + 'end;';
var
  Renderer: IHiliteRenderer;  // renders highlighted code as RTF
  Brush: TSyntaxHiliterBrush; // performs syntax highlighting
  Theme: TSyntaxHiliteTheme;  // determines syntax highlighting style
begin
  if not SyntaxHighlight then
    // null theme - no syntax highlighting
    Theme := TSyntaxHiliteThemes.NullTheme.Clone
  else
    // proper theme - with or without colour
    Theme := TConfig.Instance.HiliterThemes[
      Preferences.CurrentHiliteThemeIds[htkPrint]
    ].Clone(not UseColour);
  try
    // using "pascal" brush here since sample source is Pascal.
    Brush := TSyntaxHiliterBrushes.CreateBrush(
      TSyntaxHiliterBrushes.PascalBrushID
    );
    try
      Renderer := TRTFHiliteRenderer.Create(Builder, Brush, Theme);
      TSyntaxHiliter.Hilite(cSourceCode, Brush, Renderer);
    finally
      Brush.Free;
    end;
  finally
    Theme.Free;
  end;
end;

end.

