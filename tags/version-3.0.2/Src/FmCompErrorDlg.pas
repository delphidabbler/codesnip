{
 * FmCompErrorDlg.pas
 *
 * Dialog box that displays compiler error or warning logs.
 *
 * v0.1 of 30 Jan 2005  - Original version.
 * v0.2 of 20 Feb 2005  - Changed name of template html file from to
 *                        dlg-comperror-tplt.html.
 *                      - Localised dialog and HTML text.
 * v0.3 of 23 Feb 2005  - Changed to use new THTMLTpltDlgFrame frame instead of
 *                        THTMLDlgFrame to process HTML template and display
 *                        dialog content.
 * v0.4 of 23 Feb 2005  - Added static method that displays dialog box for a
 *                        specified routine and compiler.
 * v0.5 of 24 Feb 2005  - Changed to compiler names from Compilers global object
 *                        now that name removed from UCompilerTypes.
 * v0.6 of 04 Mar 2005  - Changed to used methods of revised Compiler object.
 *                      - Changed to resolve <%CompilerID%> in template file
 *                        that replaced previous <%Delphi%>.
 * v0.7 of 20 Apr 2005  - Changed to use renamed IntfCompilers unit.
 * v1.0 of 28 May 2006  - Improved and corrected comments.
 *                      - Removed unused unit reference.
 *                      - Changed to derive from base class that can resize
 *                        dialog to fit HTML content. Removed code from this
 *                        unit that did same job.
 * v1.1 of 07 Feb 2007  - Modified to work with revised THTMLViewDlg base class.
 * v1.2 of 11 Feb 2007  - Added GetBodyPanelHeight method override to return
 *                        required height of body panel, which is the height of
 *                        the displayed HTML.
 *                      - Removed redundant GetBrowser method.
 * v2.0 of 05 Jan 2009  - Major revision to allow to display either a list of
 *                        compiler results or the results for a single compiler.
 *                        When multiple compilers are displayed a tab set
 *                        containing the compiler names is displayed. Clicking
 *                        the tab displays that compiler's log. No tab set is
 *                        displayed when the log of a single compiler is
 *                        requested.
 *                      - Added TRequiredCompilers helper object to assist in
 *                        determining which compilers are to be displayed.
 *                      - Made private and protected sections strict.
 *                      - Changed assertions / bug exceptions to use ClassName.
 * v2.1 of 06 Jan 2009  - Changed code that reports each compiler with an error
 *                        or warning log to get compiler information from new
 *                        parameter rather than global Compilers instance.
 *                      - Added actions that enable Ctrl+Tab and Shift+Ctrl+Tab
 *                        to cycle through tabs when more than one is displayed.
 * v2.2 of 10 Jan 2009  - Changed detection of compiler errors / warnings to use
 *                        new ICompiler.HasErrorsOrWarnings method.
 *                      - Fixed bug in displaying correct form caption.
 * v2.3 of 13 Jan 2009  - Replaced control char literals with constants.
 * v2.4 of 25 Jan 2009  - Changed to use renamed ICompilers.GetGlyph method.
 *                      - Routine from UHTMLUtils now used to generate HTML
 *                        tags.
 *
 *
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
 * The Original Code is FmCompErrorDlg.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2005-2009 Peter
 * Johnson. All Rights Reserved.
 *
 * ***** END LICENSE BLOCK *****
}


unit FmCompErrorDlg;


interface


uses
  // Delphi
  Forms, StdCtrls, Controls, ExtCtrls, Classes, Tabs,
  // Project
  FmHTMLViewDlg, FrBrowserBase, FrHTMLDlg, FrHTMLTpltDlg, IntfCompilers,
  USnippets, ImgList, ActnList;


type

  {
  TRequiredCompilers:
    Helper object that stores references to compilers whose compile errors and
    warnings are to be displayed in dialog box.
  }
  TRequiredCompilers = class(TObject)
  strict private
    type
      {
      TEnumerator:
        Private enumerator object. Designed for use only by compiler, not by
        users.
      }
      TEnumerator = class(TObject)
      strict private
        fList: TRequiredCompilers;
          {Object being enumerated}
        fIndex: Integer;
          {Index of current item in enumeration}
      public
        constructor Create(const List: TRequiredCompilers);
          {Class constructor. Sets up and initialises enumeration.
            @param List [in] Reference to object to be enumerated.
          }
        function GetCurrent: ICompiler;
          {Gets current compiler in enumeration.
            @return Current string.
          }
        function MoveNext: Boolean;
          {Moves to next item in enumeration.
            @return True if there is a next item, false if enumeration
              completed.
          }
        property Current: ICompiler read GetCurrent;
          {Current item in enumeration}
      end;
    var
      fCompilers: TInterfaceList;
        {List of required compilers}
    function GetCompiler(Idx: Integer): ICompiler;
      {Read accessor for Compilers[] property.
        @param Idx [in] Index of required compiler in Compilers[].
        @return Reference to indexed compiler.
      }
    function GetCount: Integer;
      {Read accessor for Count property.
        @return Number of compilers in Compilers[] property.
      }
  public
    constructor Create;
      {Class constructor. Sets up object.
      }
    destructor Destroy; override;
      {Class destructor. Tears down object.
      }
    procedure Add(Compiler: ICompiler);
      {Adds a compiler to list.
        @param Compiler [in] Reference to compiler to be added.
      }
    function GetEnumerator: TEnumerator;
      {Gets reference to object's enumerator. For use only by compiler to
      provide for..in functionality.
        @return Required enumerastor.
      }
    property Compilers[Idx: Integer]: ICompiler read GetCompiler; default;
      {List of required compilers}
    property Count: Integer read GetCount;
      {Number of compilers in Compilers[] property}
  end;

  {
  TCompErrorDlg:
    Implements a dialog box that displays error or warning logs from last
    compilation on one or more compilers for a specified routine. It is an
    error if there are no compiler warnings or errors.
  }
  TCompErrorDlg = class(THTMLViewDlg)
    frmHTML: THTMLTpltDlgFrame;
    tsCompilers: TTabSet;
    ilCompilers: TImageList;
    alTabs: TActionList;
    actNextTab: TAction;
    actPrevTab: TAction;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure TabShortcutExecute(Sender: TObject);
    procedure TabShortcutUpdate(Sender: TObject);
    procedure tsCompilersChange(Sender: TObject; NewTab: Integer;
      var AllowChange: Boolean);
    procedure tsCompilersGetImageIndex(Sender: TObject; TabIndex: Integer;
      var ImageIndex: Integer);
  strict private
    fWantTabs: Boolean;
      {Flag indicating whether compilers tabset is to be displayed in dialog
      box}
    fRoutine: TRoutine;
      {Routine for which last compilation took place}
    fRequiredCompilers: TRequiredCompilers;
      {Object that maintains a list of compilers for which errors or warnings
      are to be displayed}
    fCompGlyphIndexes: array[TCompilerId] of Integer;
      {Maps compiler compiler ids to index of compiler image in image list}
    function GetHTMLHeight: Integer;
      {Gets height of rendered HTML required to display warnings or error
      messages. Calculates maximum height of logs of each required compiler.
        @return Required height in pixels.
      }
    function GetTabsetHeight: Integer;
      {Gets total height of tabset control.
        @return Height of tabset or 0 if not required.
      }
    procedure LoadHTML(const Compiler: ICompiler);
      {Loads HTML representation of a compiler's error or warning log into
      browser control.
        @param Compiler [in] Reference to compiler whose log is to be rendered.
        @except EBug raised if compiler result is not a warning or an error.
      }
  strict protected
    procedure InitForm; override;
      {Override of form initialisation that configures tabset and sets caption.
      }
    procedure InitHTMLFrame; override;
      {Initialises HTML frame to display error log for first (or only) compiler.
      }
    function GetBodyPanelHeight: Integer; override;
      {Calculates required height of dialog's body panel.
        @return Required height.
      }
  public
    class procedure Execute(const AOwner: TComponent; const ARoutine: TRoutine;
      const ACompiler: ICompiler); overload;
      {Shows a dialog box that displays error or warning log for a specified
      compiler as a result of test compiling a routine.
        @param AOwner [in] Component that owns this form.
        @param ARoutine [in] Routine that was compiled.
        @param ACompiler [in] Id of compiler that created log.
      }
    class procedure Execute(const AOwner: TComponent; const ARoutine: TRoutine;
      const ACompilers: ICompilers); overload;
      {Shows a dialog box that displays error and warning logs for each compiler
      that reported warnings or errors when test compiling a routine. There is a
      tab for each compiler.
        @param AOwner [in] Component that owns this form.
        @param ARoutines [in] Routine that was compiled.
        @param ACompilers [in] Object that lists all supported compilers.
      }
  end;


implementation


uses
  // Delphi
  SysUtils, Graphics,
  // Project
  UConsts, UExceptions, UHTMLUtils;


{$R *.dfm}

{
  NOTE:

  The error log HTML template is stored in RT_HTML resources as
  "dlg-comperror-tplt.html". It has the following placeholders that are replaced
  by values in this code:

  <%Status%>        log status - Error(s) or Warning(s)
  <%Routine%>       name of routine being compiled
  <%CompilerID%>    if of compiler that caused warning/error
  <%ErrorList%>     a CRLF delimited list of the errors/warnings from the log as
                    HTML list items in form <li>log-line</li>
}


resourcestring
  // Strings for display in dialog
  sSingularCaption    = 'Compiler Error or Warning';
  sPluralCaption      = 'Compiler Errors or Warnings';
  sLogStatusWarning   = 'Warning';
  sLogStatusWarnings  = 'Warnings';
  sLogStatusError     = 'Error';
  sLogStatusErrors    = 'Errors';


{ TCompErrorDlg }

class procedure TCompErrorDlg.Execute(const AOwner: TComponent;
  const ARoutine: TRoutine; const ACompiler: ICompiler);
  {Shows a dialog box that displays error or warning log for a specified
  compiler as a result of test compiling a routine.
    @param AOwner [in] Component that owns this form.
    @param ARoutine [in] Routine that was compiled.
    @param ACompiler [in] Id of compiler that created log.
  }
begin
  Assert(Assigned(ARoutine),                               // ** do not localise
    ClassName + '.Execute: ARoutine is nil');
  Assert(Assigned(ACompiler),                              // ** do not localise
    ClassName + '.Execute: ACompiler is nil');
  with Create(AOwner) do
    try
      // Record selected compiler and currently selected routine
      fRoutine := ARoutine;
      fRequiredCompilers.Add(ACompiler);
      fWantTabs := False;
      // Display dialog
      ShowModal;
    finally
      Free;
    end;
end;

class procedure TCompErrorDlg.Execute(const AOwner: TComponent;
  const ARoutine: TRoutine; const ACompilers: ICompilers);
  {Shows a dialog box that displays error and warning logs for each compiler
  that reported warnings or errors when test compiling a routine. There is a
  tab for each compiler.
    @param AOwner [in] Component that owns this form.
    @param ARoutines [in] Routine that was compiled.
    @param ACompilers [in] Object that lists all supported compilers.
  }
var
  Compiler: ICompiler;  // each supported compiler
begin
  Assert(Assigned(ARoutine),                               // ** do not localise
    ClassName + '.Execute: ARoutine is nil');
  Assert(Assigned(ACompilers),                             // ** do not localise
    ClassName + '.Execute: ACompilers is nil');
  with Create(AOwner) do
    try
      fRoutine := ARoutine;
      for Compiler in ACompilers do
        if Compiler.HasErrorsOrWarnings then
          fRequiredCompilers.Add(Compiler);
      fWantTabs := True;
      ShowModal;
    finally
      Free;
    end;
end;

procedure TCompErrorDlg.FormCreate(Sender: TObject);
  {Form construction event handler. Creates owned object.
    @param Sender [in] Not used.
  }
begin
  inherited;
  fRequiredCompilers := TRequiredCompilers.Create;
end;

procedure TCompErrorDlg.FormDestroy(Sender: TObject);
  {Form destruction event handler. Frees owned object.
    @param Sender [in] Not used.
  }
begin
  FreeAndNil(fRequiredCompilers);
  inherited;
end;

function TCompErrorDlg.GetBodyPanelHeight: Integer;
  {Calculates required height of dialog's body panel.
    @return Required height.
  }
begin
  Result := GetHTMLHeight + GetTabsetHeight;
end;

function TCompErrorDlg.GetHTMLHeight: Integer;
  {Gets height of rendered HTML required to display warnings or error messages.
  Calculates maximum height of logs of each required compiler.
    @return Required height in pixels.
  }
var
  Compiler: ICompiler;  // reference to each required compiler
begin
  Result := 0;
  // load each compiler result into frame and measure height, recording largest
  for Compiler in fRequiredCompilers do
  begin
    LoadHTML(Compiler);
    if frmHTML.DocHeight > Result then
      Result := frmHTML.DocHeight;
  end;
end;

function TCompErrorDlg.GetTabsetHeight: Integer;
  {Gets total height of tabset control.
    @return Height of tabset or 0 if not required.
  }
begin
  if fWantTabs then
  begin
    Result := tsCompilers.Height;
    if tsCompilers.AlignWithMargins then
      Inc(Result, tsCompilers.Margins.Top + tsCompilers.Margins.Bottom);
  end
  else
    Result := 0;
end;

procedure TCompErrorDlg.InitForm;
  {Override of form initialisation that configures tabset and sets caption.
  }
var
  Compiler: ICompiler;  // references each required compiler
  Glyph: TBitmap;       // each compiler glyph
const
  // Array of Singular and Plural captions for form
  cCaption: array[Boolean] of string = (sSingularCaption, sPluralCaption);
begin
  inherited;
  // Set caption
  Caption := cCaption[fRequiredCompilers.Count > 1];
  // Configure tabset
  if fWantTabs then
  begin
    tsCompilers.Tabs.Clear;
    for Compiler in fRequiredCompilers do
    begin
      Glyph := Compiler.GetGlyph;
      if Assigned(Glyph) then
        fCompGlyphIndexes[Compiler.GetID] :=
          ilCompilers.AddMasked(Glyph, Glyph.Canvas.Pixels[0, 0])
      else
        fCompGlyphIndexes[Compiler.GetID] := -1;
      tsCompilers.Tabs.Add(' ' + Compiler.GetName + ' ');
    end;
    tsCompilers.TabIndex := 0;
    LoadHTML(fRequiredCompilers[tsCompilers.TabIndex]);
  end
  else
    // No tabs wanted: hide control
    tsCompilers.Hide;
end;

procedure TCompErrorDlg.InitHTMLFrame;
  {Initialises HTML frame to display error log for first (or only) compiler.
  }
begin
  LoadHTML(fRequiredCompilers[0]);
end;

procedure TCompErrorDlg.LoadHTML(const Compiler: ICompiler);
  {Loads HTML representation of a compiler's error or warning log into browser
  control.
    @param Compiler [in] Reference to compiler whose log is to be rendered.
    @except EBug raised if compiler result is not a warning or an error.
  }
var
  Values: TStringList;  // map of HTML placeholders to actual values
  Log: TStringList;     // stores compiler log
  Status: string;       // report status: error(s) or warning(s)

  // ---------------------------------------------------------------------------
  procedure GetLogInfo(const Log: TStrings; out Status: string);
    {Gets the required log information and returns string that describes status
    of log.
      @param Log [out] Set to list of compiler log entries.
      @param Status [out] Set to log type: warning(s) or error(s).
      @except EBug raised if compiler result is not a warning or an error.
    }
  const
    cWarnText: array[Boolean] of string = (   // singular & plural warning text
      sLogStatusWarning, sLogStatusWarnings
    );
    cErrorText: array[Boolean] of string = (  // singular & plural error text
      sLogStatusError, sLogStatusErrors
    );
    cBadResult = '%s.LoadHTML: '                    // bug error message
      + 'compile result must be warning or error';  // ** do not localise
  begin
    case Compiler.GetLastCompileResult of
      crWarning:
      begin
        // Extract warnings from raw log and note this is warning
        Compiler.Log(cfWarnings, Log);
        Status := cWarnText[Log.Count > 1];
      end;
      crError:
      begin
        // Extract errors from raw log and note this is error
        Compiler.Log(cfErrors, Log);
        Status := cErrorText[Log.Count > 1];
      end;
      else
        // Not a warning or error: this is a bug
        raise EBug.CreateFmt(cBadResult, [ClassName]);
    end;
  end;

  function BuildLogListHTML(const Log: TStrings): string;
    {Returns each line of a log as a HTML list item.
      @param Log [in] List of log entries to be converted.
      @return string containing HTML.
    }
  var
    Line: string;   // each line of log
  begin
    Result := '';
    for Line in Log do
      Result := Result + MakeCompoundTag('li', MakeSafeHTMLText(Line)) + EOL;
  end;
  // ---------------------------------------------------------------------------

begin
  // ** do not localise literal strings in this method
  inherited;
  // Create log and placeholder values string lists
  Log := nil;
  Values := TStringList.Create;
  try
    Log := TStringList.Create;
    // Get compiler log and status
    GetLogInfo(Log, Status);
    // Build log report and load into browser control
    Values.Values['Status']     := Status;
    Values.Values['ErrorList']  := BuildLogListHTML(Log);
    Values.Values['Routine']    := MakeSafeHTMLText(fRoutine.Name);
    Values.Values['CompilerID'] := MakeSafeHTMLText(Compiler.GetName);
    frmHTML.Initialise('dlg-comperror-tplt.html', Values);
  finally
    // Free objects
    Log.Free;
    Values.Free;
  end;
end;

procedure TCompErrorDlg.TabShortcutExecute(Sender: TObject);
  {Cycles through tabs in forward or reverse direction depending on which action
  is triggered.
    @param Sender [in] Action that triggered event.
  }
var
  TabIdx: Integer;  // index of next or previous tab
begin
  // Ctrl+Tab and Shift+Ctrl+Tab have tag property containing direction -
  // (1 => forward and -1 => backward)
  TabIdx := tsCompilers.TabIndex + (Sender as TAction).Tag;
  if TabIdx < 0 then
    TabIdx := Pred(tsCompilers.Tabs.Count)
  else if TabIdx = tsCompilers.Tabs.Count then
    TabIdx := 0;
  tsCompilers.TabIndex := TabIdx;
end;

procedure TCompErrorDlg.TabShortcutUpdate(Sender: TObject);
  {Enables / disables actions that cycle through displayed tags.
    @param Sender [in] Action that triggered event.
  }
begin
  (Sender as TAction).Enabled := fRequiredCompilers.Count > 1;
end;

procedure TCompErrorDlg.tsCompilersChange(Sender: TObject; NewTab: Integer;
  var AllowChange: Boolean);
  {OnChange event handler for tabset. Displays warnings or errors for newly
  selected tab.
    @param Sender [in] Not used.
    @param NewTab [in] Index of newly selected tab.
    @param AllowChange [in/out] Not used.
  }
begin
  LoadHTML(fRequiredCompilers[NewTab]);
end;

procedure TCompErrorDlg.tsCompilersGetImageIndex(Sender: TObject;
  TabIndex: Integer; var ImageIndex: Integer);
  {Handles tabset's OnGetImageIndex event. Finds index of compiler glyph in
  image list.
    @param Sender [in] Not used.
    @param TabIndex [in] Tab for which image index is required.
    @param ImageIndex [in/out] Set to index of appropriate compiler glyph in
      image list, or -1 if compiler has no glyph.
  }
begin
  ImageIndex := fCompGlyphIndexes[fRequiredCompilers[TabIndex].GetID];
end;

{ TRequiredCompilers }

procedure TRequiredCompilers.Add(Compiler: ICompiler);
  {Adds a compiler to list.
    @param Compiler [in] Reference to compiler to be added.
  }
begin
  fCompilers.Add(Compiler);
end;

constructor TRequiredCompilers.Create;
  {Class constructor. Sets up object.
  }
begin
  inherited Create;
  fCompilers := TInterfaceList.Create;
end;

destructor TRequiredCompilers.Destroy;
  {Class destructor. Tears down object.
  }
begin
  FreeAndNil(fCompilers);
  inherited;
end;

function TRequiredCompilers.GetCompiler(Idx: Integer): ICompiler;
  {Read accessor for Compilers[] property.
    @param Idx [in] Index of required compiler in Compilers[].
    @return Reference to indexed compiler.
  }
begin
  Result := fCompilers[Idx] as ICompiler;
end;

function TRequiredCompilers.GetCount: Integer;
  {Read accessor for Count property.
    @return Number of compilers in Compilers[] property.
  }
begin
  Result := fCompilers.Count;
end;

function TRequiredCompilers.GetEnumerator: TEnumerator;
  {Gets reference to object's enumerator. For use only by compiler to
  provide for..in functionality.
    @return Required enumerastor.
  }
begin
  Result := TEnumerator.Create(Self);
end;

{ TRequiredCompilers.TEnumerator }

constructor TRequiredCompilers.TEnumerator.Create(
  const List: TRequiredCompilers);
  {Class constructor. Sets up and initialises enumeration.
    @param List [in] Reference to object to be enumerated.
  }
begin
  inherited Create;
  fList := List;
  fIndex := -1;
end;

function TRequiredCompilers.TEnumerator.GetCurrent: ICompiler;
  {Gets current compiler in enumeration.
    @return Current string.
  }
begin
  Result := fList[fIndex];
end;

function TRequiredCompilers.TEnumerator.MoveNext: Boolean;
  {Moves to next item in enumeration.
    @return True if there is a next item, false if enumeration
      completed.
  }
begin
  Result := fIndex < Pred(fList.Count);
  if Result then
    Inc(fIndex);
end;

end.

