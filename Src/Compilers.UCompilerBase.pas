{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2005-2020, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Abstract base class for classes that control and provide information about
 * compilers. Also provides a specialised exception class.
}


unit Compilers.UCompilerBase;


interface


uses
  // Delphi
  SysUtils, Classes, Graphics,
  // Project
  Compilers.UGlobals, Compilers.URunner, UExceptions, UEncodings;


type

  ///  <summary>Abstract base class for classes that control and provide
  ///  information about compilers.</summary>
  ///  <remarks>Provides common implementation of some ICompilers methods.
  ///  </remarks>
  TCompilerBase = class(TInterfacedObject)
  strict private
    var
      ///  <summary>Full path to compiler's executable file.</summary>
      fExecFile: string;
      ///  <summary>Stores raw log file from last compiler execution.</summary>
      fCompileLog: TStringList;
      ///  <summary>Stores compiler log prefix strings.</summary>
      ///  <remarks>Used in parsing log file when looking for warnings and
      ///  errors.</remarks>
      fPrefixes: TCompLogPrefixes;
      ///  <summary>Result of last compilation.</summary>
      fLastCompileResult: TCompileResult;
      ///  <summary>User defined command line switches.</summary>
      fSwitches: string;
      ///  <summary>List of compiler search directories.</summary>
      fSearchDirs: ISearchDirs;
      ///  <summary>Indicates whether compiler's results are to be displayed in
      ///  UI etc.</summary>
      fDisplayable: Boolean;
    ///  <summary>Generates and returns a list of space separated switches for
    ///  passing to a compiler on its command line.</summary>
    function CommandLineSwitches: string;
    ///  <summary>Builds and returns command line required to execute compiler.
    ///  </summary>
    ///  <param name="Project">string [in] Name of project to be built.</param>
    ///  <param name="Path">string [in] Full path to source code file.</param>
    ///  <returns>string. Command line needed to execute compiler to build the
    ///  source code.</returns>
    function BuildCommandLine(const Project, Path: string): string;
    ///  <summary>Executes the compiler and captures output.</summary>
    ///  <param name="Project">string [in] Command line to use to compile
    ///  source code.</param>
    ///  <param name="Path">string [in] Full path of source file to be compiled.
    ///  </param>
    ///  <returns>Integer. Compiler exit code.</returns>
    ///  <exception>Raises ECompilerError if compiler fails to run.</exception>
    function ExecuteCompiler(const CommandLine, Path: string): Integer;
    ///  <summary>Reads compiler output from given stream and records it in a
    ///  string list, omitting any blank lines.</summary>
    procedure BuildCompileLog(const CompilerOutput: TStream);
    ///  <summary>Filters compiler log, extracting only lines that contain
    ///  message Msg, and storing them in string list Lines.</summary>
    procedure FilterLog(const Msg: string; const Lines: TStrings);
    ///  <summary>Initialises the object.</summary>
    procedure Initialize;
  strict protected
    ///  <summary>Returns the type of encoding used by the compiler for text
    ///  output.</summary>
    ///  <returns>TEncodingType. System default ANSI encoding type.</returns>
    ///  <remarks>Descendants should override if their compiler uses a different
    ///  encoding.</remarks>
    function CompilerOutputEncoding: TEncodingType; virtual;
    ///  <summary>Returns a space separated list of parameters that define any
    ///  search directories to be passed to the compiler on the command line.
    ///  </summary>
    function SearchDirParams: string; virtual; abstract;
    ///  <summary>Returns any namespace parameter to be passed to compiler on
    ///  command line.</summary>
    ///  <remarks>This version returns the empty string. Sub-classes that
    ///  generate the parameter should override.</remarks>
    function NamespaceParam: string; virtual;

  public
    ///  <summary>Constructs and initialises an object instance.</summary>
    constructor Create;

    ///  <summary>Constructs an object instance that is a clone of the given
    ///  compiler object.</summary>
    constructor CreateCopy(const Obj: TCompilerBase);

    ///  <summary>Destroys an object instance.</summary>
    destructor Destroy; override;

    ///  <summary>Returns the human readable name of the compiler.</summary>
    ///  <remarks>Method of ICompiler.</remarks>
    function GetName: string; virtual; abstract;

    ///  <summary>Returns the compiler's unique ID.</summary>
    ///  <remarks>Method of ICompiler.</remarks>
    function GetID: TCompilerID; virtual; abstract;

    ///  <summary>Returns a non-localisable string that uniquely identifies the
    ///  compiler.</summary>
    ///  <remarks>Method of ICompiler.</remarks>
    function GetIDString: string; virtual; abstract;

    ///  <summary>Checks whether the compiler is both installed on this computer
    ///  and made available to CodeSnip.</summary>
    ///  <remarks>Method of ICompiler.</remarks>
    function IsAvailable: Boolean;

    ///  <summary>Returns the full path of the compiler's executable file.
    ///  </summary>
    ///  <remarks>
    ///  <para>Returns the empty string if the compiler is not known to
    ///  CodeSnip.</para>
    ///  <para>Method of ICompiler.</para>
    ///  </remarks>
    function GetExecFile: string;

    ///  <summary>Records the the full path of the compiler's executable file.
    ///  </summary>
    ///  <remarks>
    ///  <para>Passing the empty string to this method disassociates the
    ///  compiler from CodeSnip.</para>
    ///  <para>Method of ICompiler.</para>
    ///  </remarks>
    procedure SetExecFile(const Value: string);

    ///  <summary>Returns a comma separated list of the default command line
    ///  switches for use with the compiler.</summary>
    ///  <remarks>
    ///  <para>The default switches are used if the user has not provided any
    ///  switches.</para>
    ///  <para>Method of ICompiler.</para>
    ///  </remarks>
    function GetDefaultSwitches: string; virtual; abstract;

    ///  <summary>Returns a comma separated list of any user defined switches
    ///  for use with the compiler.</summary>
    ///  <remarks>Method of ICompiler.</remarks>
    function GetSwitches: string;

    ///  <summary>Records the given comma delimited list of user defined
    ///  switches to be used with the compiler.</summary>
    ///  <remarks>Method of ICompiler.</remarks>
    procedure SetSwitches(const Switches: string);

    ///  <summary>Checks if the compiler has RTL unit names that are prefixed by
    ///  its namespace.</summary>
    ///  <remarks>
    ///  <para>Always returns False in this default implementation.</para>
    ///  <para>Method of ICompiler.</para>
    ///  </remarks>
    function RequiresRTLNamespaces: Boolean; virtual;

    ///  <summary>Returns a space separated list of the compiler's default RTL
    ///  unit namespaces.</summary>
    ///  <remarks>
    ///  <para>Returns the empty string in this default implementation.</para>
    ///  <para>Method of ICompiler.</para>
    ///  </remarks>
    function GetDefaultRTLNamespaces: string; virtual;

    ///  <summary>Returns a space separated list of user-defined RTL unit
    ///  namespaces to be searched by the compiler.</summary>
    ///  <remarks>
    ///  <para>Returns the empty string in this default implementation.</para>
    ///  <para>Method of ICompiler.</para>
    ///  </remarks>
    function GetRTLNamespaces: string; virtual;

    ///  <summary>Records a list of user defined RTL unit namespaces to be
    ///  searched by the compiler.</summary>
    ///  <remarks>
    ///  <para>Namespaces is expected to be a space separated list of valid
    ///  Pascal identfiers.</para>
    ///  <para>Does nothing in this default implementation.</para>
    ///  <para>Method of ICompiler.</para>
    ///  </remarks>
    procedure SetRTLNamespaces(const Namespaces: string); virtual;

    ///  <summary>Returns a copy of the list of search directories used by the
    ///  compiler.</summary>
    ///  <remarks>Method of ICompiler.</remarks>
    function GetSearchDirs: ISearchDirs;

    ///  <summary>Records a copy of the given list of search directories to be
    ///  used by the compiler.</summary>
    ///  <remarks>Method of ICompiler.</remarks>
    procedure SetSearchDirs(Dirs: ISearchDirs);

    ///  <summary>Returns the prefixes used in interpreting error, fatal error
    ///  and warning conditions in compiler log files.</summary>
    ///  <remarks>Method of ICompiler.</remarks>
    function GetLogFilePrefixes: TCompLogPrefixes;

    ///  <summary>Records the given prefixes to be used in interpreting error,
    ///  fatal error and warning conditions in compiler log files.</summary>
    ///  <remarks>Method of ICompiler.</remarks>
    procedure SetLogFilePrefixes(const Prefixes: TCompLogPrefixes);

    ///  <summary>Returns a flag indicating if the compiler is displayable.
    ///  </summary>
    ///  <remarks>
    ///  <para>A 'displayable' compiler has its compile results displayed in the
    ///  UI etc.</para>
    ///  <para>Method of ICompiler.</para>
    ///  </remarks>
    function GetDisplayable: Boolean;

    ///  <summary>Sets the flag that determines if the compiler is displayable
    ///  to the given value.</summary>
    ///  <remarks>
    ///  <para>A 'displayable' compiler has its compile results displayed in the
    ///  UI etc.</para>
    ///  <para>Method of ICompiler.</para>
    ///  </remarks>
    procedure SetDisplayable(const Flag: Boolean);

    ///  <summary>Compiles a project and returns the result of compilation.
    ///  </summary>
    ///  <param name="Path">string [in] The full path of the directory
    ///  containing the project file.</param>
    ///  <param name="Project">string [in] Name of project source file.</param>
    ///  <returns>TCompileResult. Result of compilation (success, warning or
    ///  error).</returns>
    ///  <exception>An exception is raised if the compiler can't be executed.
    ///  </exception>
    ///  <remarks>
    ///  <para>The result of the compilation and the compiler output log are
    ///  stored: see the Log and GetLastCompileResult methods.</para>
    ///  <para>Method of ICompiler.</para>
    ///  </remarks>
    function Compile(const Path, Project: string): TCompileResult;

    ///  <summary>Deletes intermediate files created during a compilation of a.
    ///  project.</summary>
    ///  <param name="Path">string [in] The full path of the directory
    ///  containing the project file.</param>
    ///  <param name="Project">string [in] Name of project source file.</param>
    ///  <remarks>
    ///  <para>Does nothing if no project has been compiled.</para>
    ///  <para>Method of ICompiler.</para>
    ///  </remarks>
    procedure DeleteObjFiles(const Path, Project: string); virtual; abstract;

    ///  <summary>Filters the compiler output log and copies the result into a
    ///  string list.</summary>
    ///  <param name="Filter">TCompLogFilter [in] Indicates how the compiler log
    ///  is to be filtered.</param>
    ///  <param name="Lines">TStrings [in] String list that receives the lines
    ///  of the filtered log. May be empty.</param>
    ///  <remarks>Method of ICompiler.</remarks>
    procedure Log(const Filter: TCompLogFilter; const Lines: TStrings);
      overload;

    ///  <summary>Filters the ompiler output log using a filter of given type
    ///  and returns the result as a string with lines delimited by CRLF.
    ///  </summary>
    ///  <remarks>Method of ICompiler.</remarks>
    function Log(const Filter: TCompLogFilter): string;
      overload;

    ///  <summary>Checks if the last compilation resulted in an error or a
    ///  warning.</summary>
    ///  <remarks>
    ///  <para>Returns False if the Compile method has not been called.</para>
    ///  <para>Method of ICompiler.</para>
    ///  </remarks>
    function HasErrorsOrWarnings: Boolean;

    ///  <summary>Returns result of last compilation by this compiler.</summary>
    ///  <remarks>
    ///  <para>crQuery is returned if compiler is not available or if Compile
    ///  method has not been called.</para>
    ///  <para>Method of ICompiler.</para>
    ///  </remarks>
    function GetLastCompileResult: TCompileResult;
  end;

type
  ///  <summary>Class of exception raised when compiler errors occur.</summary>
  ECompilerError = class(ECodeSnip)
  strict private
    var
      ///  <summary>Value of ErrorCode property.</summary>
      fErrorCode: Integer;
      ///  <summary>Value of Compiler property.</summary>
      fCompiler: string;
  public

    ///  <summary>Creates exception instance from another exception.</summary>
    ///  <param name="E">ECompilerRunner [in] Instance of exception that
    ///  provides information about why compiler failed to run.</param>
    ///  <param name="Compiler">string [in] Name of compiler that failed to run.
    ///  </param>
    constructor Create(const E: ECompilerRunner; const Compiler: string);

    ///  <summary>Assigns properties of another exception to this one.</summary>
    ///  <param name="E">Exception [in] Exception whose properties are to be
    ///  copied. Must be an ECompilerError instance.</param>
    procedure Assign(const E: Exception); override;

    ///  <summary>Error code describing why compiler failed to run.</summary>
    property ErrorCode: Integer read fErrorCode;

    ///  <summary>Name of compiler that generated the error.</summary>
    property Compiler: string read fCompiler;
  end;


implementation


uses
  // Project
  Compilers.USearchDirs, IntfCommon, UConsts, UStrUtils, UUtils;


const
  // Default prefixes used to identify error and warning entries in logs
  cPrefixDefaults: TCompLogPrefixes = ('Fatal: ', 'Error: ', 'Warning: ');


{ TCompilerBase }

function TCompilerBase.BuildCommandLine(const Project, Path: string): string;
begin
  Result := Format(
    '"%0:s" %1:s %2:s %3:s %4:s',
    [
      fExecFile,                              // compiler exe
      LongToShortFilePath(
        IncludeTrailingPathDelimiter(Path)
      ) + Project,                            // path to project
      CommandLineSwitches,                    // command line switches
      SearchDirParams,                        // search directory param(s)
      NamespaceParam                          // namespace param
    ]
  );
end;

procedure TCompilerBase.BuildCompileLog(const CompilerOutput: TStream);
var
  Index: Integer;       // index into error string list
  Encoding: TEncoding;  // encoding used by compiler for its output
begin
  // Load log file into string list: compiler output is expected to have
  // encoding of type provided by CompilerOutputEncoding method.
  CompilerOutput.Position := 0;
  Encoding := TEncodingHelper.GetEncoding(CompilerOutputEncoding);
  try
    fCompileLog.LoadFromStream(CompilerOutput, Encoding);
  finally
    TEncodingHelper.FreeEncoding(Encoding);
  end;
  // Strip out any blank lines
  Index := 0;
  while (Index < fCompileLog.Count) do
  begin
    if fCompileLog[Index] = '' then
      fCompileLog.Delete(Index)
    else
      Inc(Index);
  end;
end;

function TCompilerBase.CommandLineSwitches: string;

  // Switch is enclosed in quotes if it contains spaces.
  procedure AppendSwitch(const Switch: string);
  begin
    if Result <> '' then
      Result := Result + ' ';
    if StrContainsStr(' ', Switch) then
      Result := Result + DOUBLEQUOTE + Switch + DOUBLEQUOTE
    else
      Result := Result + Switch;
  end;

var
  Params: TStringList;  // list of parameters
  Param: string;        // a parameter
begin
  Result := '';
  // Get list of params from string
  Params := TStringList.Create;
  try
    StrExplode(GetSwitches, ',', Params, False, True);
    // Process each param: any containing spaces get quoted
    for Param in Params do
      AppendSwitch(Param);
  finally
    Params.Free;
  end;
end;

function TCompilerBase.Compile(const Path, Project: string): TCompileResult;
var
  Res: Integer;   // compiler execution result code
begin
  // Excute compiler and catch output into compile log
  Res := ExecuteCompiler(BuildCommandLine(Project, Path), Path);
  // Work out result of compilation
  if Res = 0 then
  begin
    // no error code: could be clear compile or could have warnings
    if StrContainsStr(fPrefixes[cpWarning], fCompileLog.Text) then
      Result := crWarning
    else
      Result := crSuccess;
  end
  else
    // compiler reported error
    Result := crError;
  fLastCompileResult := Result;
end;

function TCompilerBase.CompilerOutputEncoding: TEncodingType;
begin
  // Best assumption for compiler output is ANSI default code page. Don't know
  // this for sure, but it seems reasonable.
  Result := etSysDefault;
end;

constructor TCompilerBase.Create;
begin
  inherited;
  Initialize;
  fCompileLog.Clear;
  SetLogFilePrefixes(cPrefixDefaults);
  fSwitches := GetDefaultSwitches;
  fExecFile := '';
  fLastCompileResult := crQuery;
  fDisplayable := True;
end;

constructor TCompilerBase.CreateCopy(const Obj: TCompilerBase);
begin
  inherited Create;
  Initialize;
  fCompileLog.Assign(Obj.fCompileLog);
  SetLogFilePrefixes(Obj.fPrefixes);
  fSwitches := Obj.fSwitches;
  SetRTLNamespaces(Obj.GetRTLNamespaces);
  fExecFile := Obj.fExecFile;
  fLastCompileResult := Obj.fLastCompileResult;
  fSearchDirs := Obj.GetSearchDirs;
  fDisplayable := Obj.GetDisplayable;
end;

destructor TCompilerBase.Destroy;
begin
  fCompileLog.Free;
  inherited;
end;

function TCompilerBase.ExecuteCompiler(const CommandLine,
  Path: string): Integer;
var
  CompilerRunner: TCompilerRunner;  // object that executes compiler
  CompilerOutput: TStream;          // stream that captures compiler output
begin
  Result := 0;  // keeps compiler quiet
  CompilerRunner := nil;
  // Create stream to capture compiler output
  CompilerOutput := TMemoryStream.Create;
  try
    // Perform compilation
    CompilerRunner := TCompilerRunner.Create;
    try
      Result := CompilerRunner.Execute(
        CommandLine, ExcludeTrailingPathDelimiter(Path), CompilerOutput
      );
    except
      on E: ECompilerRunner do
        raise ECompilerError.Create(E, GetName);
      else
        raise;
    end;
    // Interpret compiler output
    BuildCompileLog(CompilerOutput);
  finally
    CompilerRunner.Free;
    CompilerOutput.Free;
  end;
end;

procedure TCompilerBase.FilterLog(const Msg: string;
  const Lines: TStrings);
var
  Line: string; // line in compiler log
  Pos: Integer; // position of Msg in log line
begin
  // Loop thru all lines in compiler log
  for Line in fCompileLog do
  begin
    // Check if Msg is in current line
    Pos := StrPos(Msg, Line);
    if Pos > 0 then
    begin
      // Line required: add line without message to output string list
      Inc(Pos, Length(Msg));
      Lines.Add(System.Copy(Line, Pos, MaxInt));
    end;
  end;
end;

function TCompilerBase.GetDefaultRTLNamespaces: string;
begin
  Result := '';
end;

function TCompilerBase.GetDisplayable: Boolean;
begin
  Result := fDisplayable;
end;

function TCompilerBase.GetExecFile: string;
begin
  Result := fExecFile;
end;

function TCompilerBase.GetLastCompileResult: TCompileResult;
begin
  if IsAvailable then
    Result := fLastCompileResult
  else
    Result := crQuery;
end;

function TCompilerBase.GetLogFilePrefixes: TCompLogPrefixes;
begin
  Result := fPrefixes;
end;

function TCompilerBase.GetRTLNamespaces: string;
begin
  Result := '';
end;

function TCompilerBase.GetSearchDirs: ISearchDirs;
begin
  Result := (fSearchDirs as IClonable).Clone as ISearchDirs;
end;

function TCompilerBase.GetSwitches: string;
begin
  Result := fSwitches;
end;

function TCompilerBase.HasErrorsOrWarnings: Boolean;
begin
  Result := fLastCompileResult in [crError, crWarning];
end;

procedure TCompilerBase.Initialize;
begin
  fCompileLog := TStringList.Create;
  fSearchDirs := TSearchDirs.Create;
end;

function TCompilerBase.IsAvailable: Boolean;
begin
  Result := (fExecFile <> '') and (FileExists(fExecFile));
end;

procedure TCompilerBase.Log(const Filter: TCompLogFilter;
  const Lines: TStrings);
begin
  // Filter the log file as required
  case Filter of
    cfAll:
      // return all log file
      Lines.Assign(fCompileLog);
    cfWarnings:
    begin
      // return only warning lines
      Lines.Clear;
      FilterLog(fPrefixes[cpWarning], Lines);
    end;
    cfErrors:
    begin
      // return only error and fatal error lines
      Lines.Clear;
      FilterLog(fPrefixes[cpFatal], Lines);
      FilterLog(fPrefixes[cpError], Lines);
    end;
  end;
end;

function TCompilerBase.Log(const Filter: TCompLogFilter): string;
var
  SL: TStringList;  // string list in which to store log
begin
  // Get filtered log in string list
  SL := TStringList.Create;
  try
    Log(Filter, SL);
    // Concatenate log lines into string and return it
    Result := StrTrim(SL.Text);
  finally
    SL.Free;
  end;
end;

function TCompilerBase.NamespaceParam: string;
begin
  Result := '';
end;

function TCompilerBase.RequiresRTLNamespaces: Boolean;
begin
  Result := False;
end;

procedure TCompilerBase.SetDisplayable(const Flag: Boolean);
begin
  fDisplayable := Flag;
end;

procedure TCompilerBase.SetExecFile(const Value: string);
begin
  fExecFile := Value;
end;

procedure TCompilerBase.SetLogFilePrefixes(const Prefixes: TCompLogPrefixes);
var
  Idx: TCompLogPrefixID;  // loops thru all prefix IDs
begin
  for Idx := Low(TCompLogPrefixID) to High(TCompLogPrefixID) do
    if Prefixes[Idx] <> '' then
      // prefix set
      fPrefixes[Idx] := Prefixes[Idx]
    else
      // no prefix set: use default value
      fPrefixes[Idx] := cPrefixDefaults[Idx];
end;

procedure TCompilerBase.SetRTLNamespaces(const Namespaces: string);
begin
  // Do nothing
end;

procedure TCompilerBase.SetSearchDirs(Dirs: ISearchDirs);
begin
  fSearchDirs := (Dirs as IClonable).Clone as ISearchDirs;
end;

procedure TCompilerBase.SetSwitches(const Switches: string);
begin
  fSwitches := Switches;
end;

{ ECompilerError }

procedure ECompilerError.Assign(const E: Exception);
begin
  Assert(E is ECompilerError,
    ClassName + '.Assign: E is not a ECompilerError instance.'
  );
  inherited;  // copies inherited properties
  // copy properties unique to ECompilerError
  fErrorCode := (E as ECompilerError).fErrorCode;
  fCompiler := (E as ECompilerError).fCompiler;
end;

constructor ECompilerError.Create(const E: ECompilerRunner;
  const Compiler: string);
begin
  Assert(Assigned(E), ClassName + '.Create: E is nil');
  Assert(Compiler <> '', ClassName + '.Create: Compiler is empty string');
  inherited Create(E.Message);
  fCompiler := Compiler;
  fErrorCode := E.ErrorCode;
end;

end.

