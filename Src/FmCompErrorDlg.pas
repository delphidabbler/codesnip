{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2005-2023, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Implements a dialogue box that displays compiler error and warning logs.
}


unit FmCompErrorDlg;


interface


uses
  // Delphi
  Forms, StdCtrls, Controls, ExtCtrls, Classes, Tabs, ActnList, ImgList,
  Generics.Collections,
  // Project
  Compilers.UGlobals, DB.USnippet, FmGenericViewDlg, FrBrowserBase, FrHTMLDlg,
  FrHTMLTpltDlg, UBaseObjects, USnippetIDs;


type

  ///  <summary>Implements a dialogue box that displays error and warning logs
  ///  from last test compilation.</summary>
  ///  <remarks>It is an error if there are no compiler warnings or errors.
  ///  </remarks>
  TCompErrorDlg = class(TGenericViewDlg, INoPublicConstruct)
    frmHTML: THTMLTpltDlgFrame;
    tsCompilers: TTabSet;
    alTabs: TActionList;
    actNextTab: TAction;
    actPrevTab: TAction;
    ///  <summary>Form construction event handler. Creates owned object.
    ///  </summary>
    procedure FormCreate(Sender: TObject);
    ///  <summary>Form destruction event handler. Free owned object.</summary>
    procedure FormDestroy(Sender: TObject);
    ///  <summary>Handles Ctrl+Tab and Shift+Ctrl+Tab action OnExecute events.
    ///  Cycles through tabs in forward or reverse direction depending on which
    ///  action triggered event.</summary>
    procedure TabShortcutExecute(Sender: TObject);
    ///  <summary>Handles Ctrl+Tab and Shift+Ctrl+Tab action OnUpdate events.
    ///  Enables / disables actions that cycle through displayed tags.</summary>
    procedure TabShortcutUpdate(Sender: TObject);
    /// <summary>OnChange event handler for tabset. Displays warnings or errors
    ///  for compiler related to tab specified by NewTab parameter.</summary>
    ///  <remarks>Tab change is always permitted.</remarks>
    procedure tsCompilersChange(Sender: TObject; NewTab: Integer;
      var AllowChange: Boolean);
  strict private
    type
      ///  <summary>Class that analyses compiler logs and extracts information
      ///  required to be displayed in dialogue box.</summary>
      TCompilerLog = class(TObject)
      strict private
        ///  <summary>Reference to compiler whose log being processed.</summary>
        fCompiler: ICompiler;
        ///  <summary>Records analysed log.</summary>
        fLog: TStrings;
        ///  <summary>Value of Status property.</summary>
        fStatus: string;
        ///  <summary>Analyses compiler log file, extracting required
        ///  information.</summary>
        procedure AnalyseLog;
      public
        ///  <summary>Constructs object for given compiler.</summary>
        constructor Create(Compiler: ICompiler);
        ///  <summary>Tears down object.</summary>
        destructor Destroy; override;
        ///  <summary>Renders log entries as HTML.</summary>
        function LogListHTML: string;
        ///  <summary>Text describing log status. Warning or error.</summary>
        property Status: string read fStatus;
      end;
  strict private
    ///  <summary>Snippet for which last compilation took place.</summary>
    fSnippet: TSnippet;
    ///  <summary>List of compilers for which errors or warnings are to be
    ///  displayed.</summary>
    fRequiredCompilers: TList<ICompiler>;
    ///  <summary>Gets vertical space required to display warnings or error
    ///  messages, in pixels.</summary>
    ///  <remarks>Calculates maximum height of mesages for each required
    ///  compiler.</remarks>
    function GetHTMLHeight: Integer;
    ///  <summary>Gets total height of tabset control.</summary>
    ///  <remarks>Returns 0 if tabset not required.</remarks>
    function GetTabsetHeight: Integer;
    ///  <summary>Loads HTML representation of given compiler's error or warning
    ///  log into browser control.</summary>
    ///  <remarks>EBug raised if compiler result is not a warning or an error.
    ///  </remarks>
    procedure LoadHTML(const Compiler: ICompiler);
  strict protected
    ///  <summary>Arranges controls on form.</summary>
    ///  <remarks>Called from ancestor class.</remarks>
    procedure ArrangeForm; override;
    ///  <summary>Initialises HTML frame and sets UI font for tab set tabs.
    ///  </summary>
    ///  <remarks>Called from ancestor class.</remarks>
    procedure ConfigForm; override;
    ///  <summary>Configures tab set and sets form's caption.</summary>
    ///  <remarks>Called from ancestor class.</remarks>
    procedure InitForm; override;
  public
    ///  <summary>Shows a dialogue box that displays error and warning logs for
    ///  each compiler that reported errors or warnings when test compiling a
    ///  snippet. There is a tab for each compiler.</summary>
    ///  <param name="AOwner">TComponent [in] Component that owns this form.
    ///  </param>
    ///  <param name="ASnippet">TSnippet [in] Reference to snippet that was
    ///  compiled.</param>
    ///  <param name="ACompilers">ICompilers [in] List of all supported
    ///  compilers.</param>
    class procedure Execute(const AOwner: TComponent;
      const ASnippet: TSnippet; const ACompilers: ICompilers);
  end;


implementation


uses
  // Delphi
  Graphics,
  // Project
  DB.UMain, UConsts, UExceptions, UHTMLUtils, UHTMLTemplate;


{$R *.dfm}

{
  NOTE:

  The error log HTML template is stored in RT_HTML resources as
  "dlg-comperror-tplt.html". It has the following placeholders that are replaced
  by values in this code:

  <%Status%>        log status - Error(s) or Warning(s)
  <%SnippetName%>   name of snippet being compiled
  <%CompilerID%>    if of compiler that caused warning/error
  <%ErrorList%>     a CRLF delimited list of the errors/warnings from the log as
                    HTML list items in form <li>log-line</li>
}


resourcestring
  // Strings for display in dialogue
  sLogStatusWarning   = 'Warning';
  sLogStatusWarnings  = 'Warnings';
  sLogStatusError     = 'Error';
  sLogStatusErrors    = 'Errors';


{ TCompErrorDlg }

procedure TCompErrorDlg.ArrangeForm;
begin
  pnlBody.Height := GetHTMLHeight + GetTabsetHeight;
  // set size of dialogue
  inherited;
end;

procedure TCompErrorDlg.ConfigForm;
begin
  inherited;
  LoadHTML(fRequiredCompilers[0]);
  // must set tab set font because for some reason tab control sets its font to
  // Tahoma
  tsCompilers.Font := Self.Font;
end;

class procedure TCompErrorDlg.Execute(const AOwner: TComponent;
  const ASnippet: TSnippet; const ACompilers: ICompilers);
var
  Compiler: ICompiler;  // each supported compiler
  Dlg: TCompErrorDlg;
begin
  Assert(Assigned(ACompilers), ClassName + '.Execute: ACompilers is nil');
  Dlg := InternalCreate(AOwner);
  try
    Dlg.fSnippet := ASnippet;
    for Compiler in ACompilers do
      if Compiler.HasErrorsOrWarnings then
        Dlg.fRequiredCompilers.Add(Compiler);
    Dlg.ShowModal;
  finally
    Dlg.Free;
  end;
end;

procedure TCompErrorDlg.FormCreate(Sender: TObject);
begin
  inherited;
  fRequiredCompilers := TList<ICompiler>.Create;
end;

procedure TCompErrorDlg.FormDestroy(Sender: TObject);
begin
  fRequiredCompilers.Free;
  inherited;
end;

function TCompErrorDlg.GetHTMLHeight: Integer;
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
begin
  Result := tsCompilers.Height;
  if tsCompilers.AlignWithMargins then
    Inc(Result, tsCompilers.Margins.Top + tsCompilers.Margins.Bottom);
end;

procedure TCompErrorDlg.InitForm;
var
  Compiler: ICompiler;  // references each required compiler
begin
  inherited;
  tsCompilers.Tabs.Clear;
  for Compiler in fRequiredCompilers do
    tsCompilers.Tabs.Add(' ' + Compiler.GetName + ' ');
  tsCompilers.TabIndex := 0;
  LoadHTML(fRequiredCompilers[tsCompilers.TabIndex]);
end;

procedure TCompErrorDlg.LoadHTML(const Compiler: ICompiler);
var
  Log: TCompilerLog;  // stores compiler log
begin
  inherited;
  Log := TCompilerLog.Create(Compiler);
  try
    frmHTML.Initialise(
      'dlg-comperror-tplt.html',
      procedure(Tplt: THTMLTemplate)
      begin
        Tplt.ResolvePlaceholderText('Status', Log.Status);
        Tplt.ResolvePlaceholderHTML('ErrorList', Log.LogListHTML);
        Tplt.ResolvePlaceholderText('SnippetName', fSnippet.DisplayName);
        Tplt.ResolvePlaceholderText('CompilerID', Compiler.GetName);
      end
    );
  finally
    Log.Free;
  end;
end;

procedure TCompErrorDlg.TabShortcutExecute(Sender: TObject);
var
  TabIdx: Integer;  // index of next or previous tab
begin
  // Ctrl+Tab and Shift+Ctrl+Tab actions have tag property containing direction:
  // (1 => forward and -1 => backward)
  TabIdx := tsCompilers.TabIndex + (Sender as TAction).Tag;
  if TabIdx < 0 then
    TabIdx := Pred(tsCompilers.Tabs.Count)
  else if TabIdx = tsCompilers.Tabs.Count then
    TabIdx := 0;
  tsCompilers.TabIndex := TabIdx;
end;

procedure TCompErrorDlg.TabShortcutUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := fRequiredCompilers.Count > 1;
end;

procedure TCompErrorDlg.tsCompilersChange(Sender: TObject; NewTab: Integer;
  var AllowChange: Boolean);
begin
  LoadHTML(fRequiredCompilers[NewTab]);
end;

{ TCompErrorDlg.TCompilerLog }

procedure TCompErrorDlg.TCompilerLog.AnalyseLog;
const
  // singular & plural warning text
  cWarnText: array[Boolean] of string = (
    sLogStatusWarning, sLogStatusWarnings
  );
  // singular & plural error text
  cErrorText: array[Boolean] of string = (
    sLogStatusError, sLogStatusErrors
  );
  // bug error message
  cBadResult = '%s.AnalyseLog: compile result must be warning or error';
begin
  case fCompiler.GetLastCompileResult of
    crWarning:
    begin
      // Extract warnings from raw log and note this is warning
      fCompiler.Log(cfWarnings, fLog);
      fStatus := cWarnText[fLog.Count > 1];
    end;
    crError:
    begin
      // Extract errors from raw log and note this is error
      fCompiler.Log(cfErrors, fLog);
      fStatus := cErrorText[fLog.Count > 1];
    end;
    else
      // Not a warning or error: this is a bug
      raise EBug.CreateFmt(cBadResult, [ClassName]);
  end;
end;

constructor TCompErrorDlg.TCompilerLog.Create(Compiler: ICompiler);
begin
  inherited Create;
  fCompiler := Compiler;
  fLog := TStringList.Create;
  AnalyseLog;
end;

destructor TCompErrorDlg.TCompilerLog.Destroy;
begin
  fLog.Free;
  inherited;
end;

function TCompErrorDlg.TCompilerLog.LogListHTML: string;
var
  Line: string;   // each line of log
begin
  Result := '';
  for Line in fLog do
    Result := Result + THTML.CompoundTag('li', THTML.Entities(Line)) + EOL;
end;

end.

