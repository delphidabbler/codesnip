{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2006-2013, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * Implements a frame that allows user to set source code preferences.
 * Designed for use as one of the tabs in the Preferences dialogue box.
}


unit FrSourcePrefs;


interface


uses
  // Delphi
  StdCtrls, Forms, Controls, Classes,
  // Project
  FrPrefsBase, FrRTFShowCase, Hiliter.UGlobals, UPreferences, USourceFileInfo,
  USourceGen;


type

  {
  TSourcePrefsFrame:
    Frame that allows user to set source code preferences. Can persist
    preferences entered by user. Note: Designed for use in preferences dialog
    box.
  }
  TSourcePrefsFrame = class(TPrefsBaseFrame)
    cbCommentStyle: TComboBox;
    cbSnippetFileType: TComboBox;
    chkSyntaxHighlighting: TCheckBox;
    frmPreview: TRTFShowCaseFrame;
    gbFileFormat: TGroupBox;
    gbSourceCode: TGroupBox;
    lblCommentStyle: TLabel;
    lblSnippetFileType: TLabel;
    chkTruncateComments: TCheckBox;
    procedure cbCommentStyleChange(Sender: TObject);
    procedure cbSnippetFileTypeChange(Sender: TObject);
  strict private
    fHiliteAttrs: IHiliteAttrs;
      {Style of syntax highlighting to use in sample output}
    procedure SelectSourceFileType(const FT: TSourceFileType);
      {Selects entry in file type combo box that matches specified source code
      file type.
        @param FT [in] File type to select.
      }
    procedure SelectCommentStyle(const CommentStyle: TCommentStyle);
      {Selects entry in comment style combo box that matches specified style.
        @param CommentStyle [in] Comment style to select.
      }
    function GetSourceFileType: TSourceFileType;
      {Gets source file type selected by user.
        @return Source file type.
      }
    function GetCommentStyle: TCommentStyle;
      {Gets comment style selected by user.
        @return Comment style.
      }
    procedure UpdateControlState;
      {Updates state of dialog's controls depending on values entered.
      }
    procedure UpdatePreview;
      {Updates source code preview according to user selections.
      }
  public
    constructor Create(AOwner: TComponent); override;
      {Class constructor. Initialises controls.
      }
    procedure Activate(const Prefs: IPreferences); override;
      {Called when page activated. Updates controls.
        @param Prefs [in] Object that provides info used to update controls.
      }
    procedure Deactivate(const Prefs: IPreferences); override;
      {Called when page is deactivated. Stores information entered by user.
        @param Prefs [in] Object used to store information.
      }
    ///  <summary>Checks if preference changes require that main window UI is
    ///  updated.</summary>
    ///  <remarks>Called when dialog box containing frame is closing. Always
    ///  returns False because these preferences never affect UI.</remarks>
    function UIUpdated: Boolean; override;
    procedure ArrangeControls; override;
      {Arranges controls on frame. Called after frame has been sized.
      }
    function DisplayName: string; override;
      {Caption that is displayed in the tab sheet that contains this frame when
      displayed in the preference dialog box.
        @return Required display name.
      }
    class function Index: Byte; override;
      {Index number that determines the location of the tab containing this
      frame when displayed in the preferences dialog box.
        @return Required index number.
      }
  end;


implementation


uses
  // Delphi
  SysUtils, Math,
  // Project
  FmPreferencesDlg, Hiliter.UAttrs, Hiliter.UFileHiliter,
  IntfCommon, UConsts, UCtrlArranger, URTFUtils;


{$R *.dfm}


resourcestring
  // File type descriptions
  sHTMLFileDesc = 'HTML';
  sRTFFileDesc = 'Rich text';
  sPascalFileDesc = 'Pascal';
  sTextFileDesc = 'Plain text';


const
  // Maps source code file types to descriptions
  cFileDescs: array[TSourceFileType] of string = (
    sTextFileDesc, sPascalFileDesc, sHTMLFileDesc, sRTFFileDesc
  );


type
  {
  TSourcePrefsPreview:
    Class used to generate source code preview displayed in source preferences
    frame.
  }
  TSourcePrefsPreview = class(TObject)
  strict private
    fHiliteAttrs: IHiliteAttrs;
      {Attributes of syntax highlighter to use to render preview}
    fCommentStyle: TCommentStyle;
      {Value of CommentStyle property}
    function SourceCode: string;
      {Create raw source code used in preview.
        @return Required raw source code.
      }
  public
    constructor Create(const CommentStyle: TCommentStyle;
      const HiliteAttrs: IHiliteAttrs);
      {Class constructor. Sets up object.
        @param CommentStyle [in] Style of commenting to use in preview.
        @param HiliteAttrs [in] Attributes of highlighter used to render
          preview.
      }
    function Generate: TRTF;
      {Generate RTF code used to render preview.
        @return Required RTF code.
      }
  end;


{ TSourcePrefsFrame }

procedure TSourcePrefsFrame.Activate(const Prefs: IPreferences);
  {Called when page activated. Updates controls.
    @param Prefs [in] Object that provides info used to update controls.
  }
begin
  // Update control values per settings
  SelectSourceFileType(Prefs.SourceDefaultFileType);
  SelectCommentStyle(Prefs.SourceCommentStyle);
  chkTruncateComments.Checked := Prefs.TruncateSourceComments;
  chkSyntaxHighlighting.Checked := Prefs.SourceSyntaxHilited;
  (fHiliteAttrs as IAssignable).Assign(Prefs.HiliteAttrs);
  fHiliteAttrs.ResetDefaultFont;
  // Update state of controls and preview
  UpdateControlState;
  UpdatePreview;
end;

procedure TSourcePrefsFrame.ArrangeControls;
  {Arranges controls on frame. Called after frame has been sized.
  }
var
  Col2Left: Integer;  // position of second column of controls
begin
  TCtrlArranger.AlignVCentres(20, [lblCommentStyle, cbCommentStyle]);
  TCtrlArranger.MoveBelow([lblCommentStyle, cbCommentStyle], frmPreview, 8);
  TCtrlArranger.MoveBelow(frmPreview, chkTruncateComments, 8);
  gbSourceCode.ClientHeight := TCtrlArranger.TotalControlHeight(gbSourceCode)
    + 10;

  TCtrlArranger.AlignVCentres(20, [lblSnippetFileType, cbSnippetFileType]);
  TCtrlArranger.MoveBelow(
    [lblSnippetFileType, cbSnippetFileType], chkSyntaxHighlighting, 8
  );
  gbFileFormat.ClientHeight := TCtrlArranger.TotalControlHeight(gbFileFormat)
    + 10;

  TCtrlArranger.MoveBelow(gbSourceCode, gbFileFormat, 8);

  TCtrlArranger.AlignLefts([lblCommentStyle, lblSnippetFileType], 8);
  Col2Left := Max(
    TCtrlArranger.RightOf(lblCommentStyle),
    TCtrlArranger.RightOf(lblSnippetFileType)
  ) + 12;
  TCtrlArranger.AlignLefts(
    [
      cbCommentStyle, frmPreview, cbSnippetFileType, chkSyntaxHighlighting,
      chkTruncateComments
    ],
    Col2Left
  );
end;

procedure TSourcePrefsFrame.cbCommentStyleChange(Sender: TObject);
  {Handles OnChange event in Comment Style combo box by updating preview.
    @param Sender [in] Not used.
  }
begin
  UpdateControlState;
  UpdatePreview;
end;

procedure TSourcePrefsFrame.cbSnippetFileTypeChange(Sender: TObject);
  {Handles OnChange event in Snippets File Type combo box by updating other
  dialog controls with respect to changed value.
    @param Sender [in] Not used.
  }
begin
  UpdateControlState;
end;

constructor TSourcePrefsFrame.Create(AOwner: TComponent);
  {Class constructor. Initialises controls.
    @param AOwner [in] Not used.
  }
var
  FileType: TSourceFileType;  // loops thru source file types
  CSIdx: TCommentStyle;       // loops thru comment styles
begin
  inherited;
  HelpKeyword := 'SourceCodePrefs';
  // Create syntax highlighter object for use in sample output
  fHiliteAttrs := THiliteAttrsFactory.CreateDefaultAttrs;
  // Populate file type combo
  for FileType := Low(TSourceFileType) to High(TSourceFileType) do
    cbSnippetFileType.Items.AddObject(cFileDescs[FileType], TObject(FileType));
  // Populate comment style combo
  for CSIdx := Low(TCommentStyle) to High(TCommentStyle) do
    cbCommentStyle.Items.AddObject(
      TSourceComments.CommentStyleDesc(CSIdx), TObject(CSIdx)
    );
end;

procedure TSourcePrefsFrame.Deactivate(const Prefs: IPreferences);
  {Called when page is deactivated. Stores information entered by user.
    @param Prefs [in] Object used to store information.
  }
begin
  Prefs.SourceCommentStyle := GetCommentStyle;
  Prefs.TruncateSourceComments := chkTruncateComments.Checked;
  Prefs.SourceDefaultFileType := GetSourceFileType;
  Prefs.SourceSyntaxHilited := chkSyntaxHighlighting.Checked;
end;

function TSourcePrefsFrame.DisplayName: string;
  {Caption that is displayed in the tab sheet that contains this frame when
  displayed in the preference dialog box.
    @return Required display name.
  }
resourcestring
  sDisplayName = 'Code Formatting'; // display name
begin
  Result := sDisplayName;
end;

function TSourcePrefsFrame.GetCommentStyle: TCommentStyle;
  {Gets comment style selected by user.
    @return Comment style.
  }
begin
  Result := TCommentStyle(
    cbCommentStyle.Items.Objects[cbCommentStyle.ItemIndex]
  );
end;

function TSourcePrefsFrame.GetSourceFileType: TSourceFileType;
  {Gets source file type selected by user.
    @return Source file type.
  }
begin
  Result := TSourceFileType(
    cbSnippetFileType.Items.Objects[cbSnippetFileType.ItemIndex]
  );
end;

class function TSourcePrefsFrame.Index: Byte;
  {Index number that determines the location of the tab containing this
  frame when displayed in the preferences dialog box.
    @return Required index number.
  }
begin
  Result := 20;
end;

procedure TSourcePrefsFrame.SelectCommentStyle(
  const CommentStyle: TCommentStyle);
  {Selects entry in comment style combo box that matches specified style.
    @param CommentStyle [in] Comment style to select.
  }
begin
  cbCommentStyle.ItemIndex :=
    cbCommentStyle.Items.IndexOfObject(TObject(CommentStyle));
end;

procedure TSourcePrefsFrame.SelectSourceFileType(
  const FT: TSourceFileType);
  {Selects entry in file type combo box that matches specified source code file
  type.
    @param FT [in] File type to select.
  }
begin
  cbSnippetFileType.ItemIndex :=
    cbSnippetFileType.Items.IndexOfObject(TObject(FT));
end;

function TSourcePrefsFrame.UIUpdated: Boolean;
begin
  Result := False;
end;

procedure TSourcePrefsFrame.UpdateControlState;
  {Updates state of dialog's controls depending on values entered.
  }
begin
  chkSyntaxHighlighting.Enabled :=
    TFileHiliter.IsHilitingSupported(GetSourceFileType);
  chkTruncateComments.Enabled := GetCommentStyle <> csNone;
end;

procedure TSourcePrefsFrame.UpdatePreview;
  {Updates source code preview according to user selections.
  }
var
  Preview: TSourcePrefsPreview; // object that creates preview
begin
  // We always use same font size, regardless of user preferences
  fHiliteAttrs.FontSize := Font.Size;
  // Generate and display preview with required comment style
  Preview := TSourcePrefsPreview.Create(GetCommentStyle, fHiliteAttrs);
  try
    // Display preview
    TRichEditHelper.Load(frmPreview.RichEdit, Preview.Generate);
  finally
    Preview.Free;
  end;
end;

{ TSourcePrefsPreview }

resourcestring
  // Localisable source preview text
  sPrevProcName = 'Example';              // name of example proc
  sPrevDesc = 'Description goes here.';   // sample proc description
  sPrevCalledProc = 'DoSomethingHere';    // name of proc called from example

const
  // Example procedure prototype and body
  cPrevProcProto = 'procedure %0:s;' + EOL;
  cPrevProcBody = 'begin' + EOL +'  %1:s;'+ EOL + 'end;';

  // Map of comment style to sample code
  cPrevSamples: array[TCommentStyle] of string = (
    // no comments: just prototype followed by body
    cPrevProcProto + cPrevProcBody,
    // comments after snippet header: prototype then comments then body
    cPrevProcProto + '  {%2:s}' + EOL +  cPrevProcBody,
    // comments before prototype: comments then prototype then body
    '{' + EOL + '  %2:s' + EOL + '}' + EOL + cPrevProcProto + cPrevProcBody
  );

constructor TSourcePrefsPreview.Create(const CommentStyle: TCommentStyle;
  const HiliteAttrs: IHiliteAttrs);
  {Class constructor. Sets up object.
    @param CommentStyle [in] Style of commenting to use in preview.
    @param HiliteAttrs [in] Attributes of highlighter used to render preview.
  }
begin
  inherited Create;
  fCommentStyle := CommentStyle;
  fHiliteAttrs := HiliteAttrs;
end;

function TSourcePrefsPreview.Generate: TRTF;
  {Generate RTF code used to render preview.
    @return Required RTF code.
  }
begin
  Result := TRTF.Create(TRTFDocumentHiliter.Hilite(SourceCode, fHiliteAttrs));
end;

function TSourcePrefsPreview.SourceCode: string;
  {Create raw source code used in preview.
    @return Required raw source code.
  }
begin
  // Source code layout depends on commenting style
  Result := Format(
    cPrevSamples[fCommentStyle],
    [sPrevProcName, sPrevCalledProc, sPrevDesc]
  );
end;

initialization

// Register frame with preferences dialog box
TPreferencesDlg.RegisterPage(TSourcePrefsFrame);

end.

