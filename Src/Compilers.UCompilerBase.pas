{
 * Compilers.UCompilerBase.pas
 *
 * Abstract base class for classes that control and provide information about
 * compilers. Provides common functionality and specialised exception.
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
 * The Original Code is Compilers.UCompilerBase.pas, formerly UCompilerBase.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2005-2010 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
}


unit Compilers.UCompilerBase;


interface


uses
  // Delphi
  SysUtils, Classes, Graphics,
  // Project
  Compilers.UGlobals, Compilers.URunner, UExceptions;


type

  {
  TCompilerBase:
    Abstract base class for classes that control and provide information about
    compilers. Provides common functionality.
  }
  TCompilerBase = class(TInterfacedObject)
  strict private
    fExecFile: string;
      {Path to compiler's executable file}
    fCompileLog: TStringList;
      {Stores raw log file from last compiler execution}
    fPrefixes: TCompLogPrefixes;
      {Stores compiler log prefix strings for parsing log file}
    fLastCompileResult: TCompileResult;
      {Result of last compilation}
    fBitmap: TBitmap;
      {Glyph reprenting compiler: nil if no glyph}
    fSwitches: string;
      {User defined command line switches}
    function CommandLineSwitches: string;
      {Generate list of space separated switches for compiler command line.
        @return Required list.
      }
    function BuildCommandLine(const Project, Path: string): string;
      {Builds command line required to execute compiler.
        @param Project [in] Name of project to build.
        @param Path [in] Full path to project file.
        @return Command line needed to execute compiler to build the project.
      }
    function ExecuteCompiler(const CommandLine, Path: string): Integer;
      {Executes compiler and captures output.
        @param CommandLine [in] Command line used to compile source code.
        @param Path [in] Full path to the source file being compiled.
        @return Exit code from compiler.
        @except ECompilerError raised if compiler failed to run.
      }
    procedure BuildCompileLog(const CompilerOutput: TStream);
      {Reads compiler output from a stream and records in a string list field,
      omitting any blank lines.
        @param CompilerOutput [in] Stream containing compiler output.
      }
    procedure FilterLog(const Msg: string; const Lines: TStrings);
      {Filters compiler log extracting only lines that contain required message.
        @param Msg [in] Text required to be contained in matching lines of log
          file.
        @param Lines [in] String list that receives filtered lines.
      }
    procedure Initialize;
      {Initializes object.
      }
  protected
    function GlyphResourceName: string; virtual;
      {Name of any resource containing a "glyph" bitmap for a compiler.
        @return Resource name or '' if the compiler has no glyph.
      }
    { ICompiler methods }
    function GetName: string; virtual; abstract;
      {Provides the human readable name of the compiler.
        @return Name of the compiler.
      }
    function GetID: TCompilerID; virtual; abstract;
      {Provides the unique id of the compiler.
        @return Compiler id.
      }
    function GetIDString: string; virtual; abstract;
      {Provides a non-localisable string that identifies the compiler.
        @return Compiler id string.
      }
    function GetGlyph: TBitmap;
      {Returns reference to any 18x18 bitmap associated with the compiler.
        @return Reference to bitmap or nil if there is no such bitmap.
      }
    function IsAvailable: Boolean;
      {Tells whether the compiler is installed on this computer and made
      available to CodeSnip.
        @return True if compiler is available to CodeSnip.
      }
    function GetExecFile: string;
      {Returns full path to compiler's executable file.
        @return Required path.
      }
    procedure SetExecFile(const Value: string);
      {Stores full path to compiler's executable file.
        @param Value [in] Path to compiler.
      }
    function GetDefaultSwitches: string; virtual; abstract;
      {Returns default command line switches for compiler.
        @return Switches separated by commas.
      }
    function GetSwitches: string;
      {Returns user-defined swtches to be used by compiler.
        @return Required switches separated by commas. On creation these are
          default switches.
      }
    procedure SetSwitches(const Switches: string);
      {Sets user defined switches.
        @param Switches [in] Required switches separated by commas.
      }
    function GetLogFilePrefixes: TCompLogPrefixes;
      {Returns prefixes used in interpreting error, fatal error and warning
      conditions in log files.
        @return Array of prefix strings.
      }
    procedure SetLogFilePrefixes(const Prefixes: TCompLogPrefixes);
      {Records prefixes used in interpreting error, fatal error and warning
      conditions in log files.
        @param Prefixes [in] Array of required prefix strings.
      }
    function Compile(const Path, Project: string): TCompileResult;
      {Compiles a project and returns result of compilation. Records result of
      compilation and compiler's output log.
        @param Path [in] Path where the project is found.
        @param Project [in] Name of project (source) file.
        @return Result of compilation i.e. success, warnings or error.
        @except ECompilerError raised if we can't execute the compiler.
      }
    procedure DeleteObjFiles(const Path, Project: string); virtual; abstract;
      {Deletes binary intermdiates files created during a compilation. Does
      nothing if there has been no compilation.
        @param Path [in] Path where project file is found.
        @param Project [in] Name of project (source file)
      }
    procedure Log(const Filter: TCompLogFilter; const Lines: TStrings);
      overload;
      {Copies filtered compiler log to string list.
        @param Filter [in] Indicates how log to be filtered i.e. return all log,
          return warnings only or return errors only.
        @param Lines [in] Lines of the log (cleared if no entries).
      }
    function Log(const Filter: TCompLogFilter): string;
      overload;
      {Returns compiler log as a CRLF delimited string using specified filter.
        @param Filter [in] Indicates how log to be filtered i.e. return all log,
          return warnings only or return errors only.
        @return Text of the log, with lines separated by CRLF.
      }
    function HasErrorsOrWarnings: Boolean;
      {Checks if last compile result was an error or a warning.
        @return True if there are errors or warning, False otherwise.
      }
    function GetLastCompileResult: TCompileResult;
      {Informs of result of last compilation by this compiler.
        @return Result of last compilation or crQuery of compiler not available.
      }
  public
    constructor Create;
      {Class constructor. Sets up object.
      }
    constructor CreateCopy(const Obj: TCompilerBase);
      {Copy constructor. Creates a new object that is copy of another object.
        @param Obj [in] Compiler object to copy.
      }
    destructor Destroy; override;
      {Class destructor. Tears down object.
      }
  end;

  {
  ECompilerError:
    Exception raised when compiler errors occur.
  }
  ECompilerError = class(ECodeSnip)
  strict private
    fErrorCode: Integer;
      {Value of ErrorCode property}
    fCompiler: string;
      {Value of Compiler property}
  public
    constructor Create(const E: ECompilerRunner; const Compiler: string);
      {Class constructor. Creates exception instance from another exception.
        @param E [in] Instance of exception that provides information about why
          compiler failed to run.
        @param Compiler [in] Name of compiler that failed to run.
      }
    procedure Assign(const E: Exception); override;
      {Assigns properties of another exception to this one.
        @param E [in] Exception whose properties are to be copied. Must be an
          ECompilerError instance.
      }
    property ErrorCode: Integer
      read fErrorCode;
      {Error code describing why compiler failed to run}
    property Compiler: string
      read fCompiler;
      {Name of compiler that generated the error}
  end;


implementation


uses
  // Project
  UUtils;


const
  // Default prefixes used to identify error and warning entries in logs
  cPrefixDefaults: TCompLogPrefixes = ('Fatal: ', 'Error: ', 'Warning: ');


{ TCompilerBase }

function TCompilerBase.BuildCommandLine(const Project, Path: string): string;
  {Builds command line required to execute compiler.
    @param Project [in] Name of project to build.
    @param Path [in] Full path to project file.
    @return Command line needed to execute compiler to build the project.
  }
begin
  Result := Format(
    '"%0:s" %1:s %2:s',
    [
      fExecFile,                              // compile exe
      LongToShortFilePath(
        IncludeTrailingPathDelimiter(Path)
      ) + Project,                            // path to project
      CommandLineSwitches                     // command line switches
    ]
  );
end;

procedure TCompilerBase.BuildCompileLog(const CompilerOutput: TStream);
  {Reads compiler output from a stream and records in a string list field,
  omitting any blank lines.
    @param CompilerOutput [in] Stream containing compiler output.
  }
var
  Index: Integer;   // index into error string list
begin
  // Load log file into string list
  fCompileLog.LoadFromStream(CompilerOutput);
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
  {Generate list of space separated switches for compiler command line.
    @return Required list.
  }
var
  Params: TStringList;    // list of parameters
  Param: string;          // a parameter
  Idx: Integer;           // loops thru parameters
begin
  Result := '';
  // Get list of params from string
  Params := TStringList.Create;
  try
    ExplodeStr(GetSwitches, ',', Params, False);
    // Process each param: any containing spaces get quoted
    for Idx := 0 to Pred(Params.Count) do
    begin
      Param := Params[Idx];
      if AnsiPos(' ', Param) > 0 then
        Param := '"' + Param + '"';
      // params are space separated
      if Result <> '' then
        Result := Result + ' ';
      Result := Result + Param;
    end;
  finally
    FreeAndNil(Params);
  end;
end;

function TCompilerBase.Compile(const Path, Project: string): TCompileResult;
  {Compiles a project and returns result of compilation. Records result of
  compilation and compiler's output log.
    @param Path [in] Path where the project is found.
    @param Project [in] Name of project (source) file.
    @return Result of compilation i.e. success, warnings or error.
    @except ECompilerError raised if we can't execute the compiler.
  }
var
  Res: Integer;   // compiler execution result code
begin
  // Excute compiler and catch output into compile log
  Res := ExecuteCompiler(BuildCommandLine(Project, Path), Path);
  // Work out result of compilation
  if Res = 0 then
  begin
    // no error code: could be clear compile or could have warnings
    if AnsiPos(fPrefixes[cpWarning], fCompileLog.Text) > 0 then
      Result := crWarning
    else
      Result := crSuccess;
  end
  else
    // compiler reported error
    Result := crError;
  fLastCompileResult := Result;
end;

constructor TCompilerBase.Create;
  {Class constructor. Sets up object.
  }
begin
  inherited;
  Initialize;
  fCompileLog.Clear;
  SetLogFilePrefixes(cPrefixDefaults);
  fSwitches := GetDefaultSwitches;
  fExecFile := '';
  fLastCompileResult := crQuery;
end;

constructor TCompilerBase.CreateCopy(const Obj: TCompilerBase);
  {Copy constructor. Creates a new object that is copy of another object.
    @param Obj [in] Compiler object to copy.
  }
begin
  inherited Create;
  Initialize;
  fCompileLog.Assign(Obj.fCompileLog);
  SetLogFilePrefixes(Obj.fPrefixes);
  fSwitches := Obj.fSwitches;
  fExecFile := Obj.fExecFile;
  fLastCompileResult := Obj.fLastCompileResult;
end;

destructor TCompilerBase.Destroy;
  {Class destructor. Tears down object.
  }
begin
  FreeAndNil(fCompileLog);
  FreeAndNil(fBitmap);
  inherited;
end;

function TCompilerBase.ExecuteCompiler(const CommandLine,
  Path: string): Integer;
  {Executes compiler and captures output.
    @param CommandLine [in] Command line used to compile source code.
    @param Path [in] Full path to the source file being compiled.
    @return Exit code from compiler.
    @except ECompilerError raised if compiler failed to run.
  }
var
  CompilerRunner: TCompilerRunner;  // object that executes compiler
  OutStm: TStream;                  // stream that captures compiler output
begin
  Result := 0;  // keeps compiler quiet
  CompilerRunner := nil;
  // Create stream to capture compiler output
  OutStm := TMemoryStream.Create;
  try
    // Perform compilation
    CompilerRunner := TCompilerRunner.Create;
    try
      Result := CompilerRunner.Execute(
        CommandLine, ExcludeTrailingPathDelimiter(Path), OutStm
      );
    except
      on E: ECompilerRunner do
        raise ECompilerError.Create(E, GetName);
      else
        raise;
    end;
    // Interpret compiler output
    OutStm.Position := 0;
    BuildCompileLog(OutStm);
  finally
    FreeAndNil(CompilerRunner);
    FreeAndNil(OutStm);
  end;
end;

procedure TCompilerBase.FilterLog(const Msg: string;
  const Lines: TStrings);
  {Filters compiler log extracting only lines that contain required message.
    @param Msg [in] Text required to be contained in matching lines of log file.
    @param Lines [in] String list that receives filtered lines.
  }
var
  Line: string; // line in compiler log
  Pos: Integer; // position of Msg in log line
begin
  // Loop thru all lines in compiler log
  for Line in fCompileLog do
  begin
    // Check if Msg is in current line
    Pos := AnsiPos(Msg, Line);
    if Pos > 0 then
    begin
      // Line required: add line without message to output string list
      Inc(Pos, Length(Msg));
      Lines.Add(System.Copy(Line, Pos, MaxInt));
    end;
  end;
end;

function TCompilerBase.GetExecFile: string;
  {Returns full path to compiler's executable file.
    @return Required path.
  }
begin
  Result := fExecFile;
end;

function TCompilerBase.GetGlyph: Graphics.TBitmap;
  {Returns reference to any 18x18 bitmap associated with the compiler.
    @return Reference to bitmap or nil if there is no such bitmap.
  }
begin
  if not Assigned(fBitmap) and (GlyphResourceName <> '') then
  begin
    // Bitmap not yet created: create it and load from resources
    fBitmap := Graphics.TBitmap.Create;
    fBitmap.LoadFromResourceName(HInstance, GlyphResourceName);
  end;
  Result := fBitmap;
end;

function TCompilerBase.GetLastCompileResult: TCompileResult;
  {Informs of result of last compilation by this compiler.
    @return Result of last compilation or crQuery of compiler not available.
  }
begin
  if IsAvailable then
    Result := fLastCompileResult
  else
    Result := crQuery;
end;

function TCompilerBase.GetLogFilePrefixes: TCompLogPrefixes;
  {Returns prefixes used in interpreting error, fatal error and warning
  conditions in log files.
    @return Array of prefix strings.
  }
begin
  Result := fPrefixes;
end;

function TCompilerBase.GetSwitches: string;
  {Returns user-defined swtches to be used by compiler.
    @return Required switches separated by commas. On creation these are default
      switches.
  }
begin
  Result := fSwitches;
end;

function TCompilerBase.GlyphResourceName: string;
  {Name of any resource containing a "glyph" bitmap for a compiler.
    @return Resource name or '' if the compiler has no glyph.
  }
begin
  // Assume no glyph: descendants override
  Result := '';
end;

function TCompilerBase.HasErrorsOrWarnings: Boolean;
  {Checks if last compile result was an error or a warning.
    @return True if there are errors or warning, False otherwise.
  }
begin
  Result := fLastCompileResult in [crError, crWarning];
end;

procedure TCompilerBase.Initialize;
  {Initializes object.
  }
begin
  fCompileLog := TStringList.Create;
end;

function TCompilerBase.IsAvailable: Boolean;
  {Tells whether the compiler is installed on this computer and made available
  to CodeSnip.
    @return True if compiler is available to CodeSnip.
  }
begin
  Result := (fExecFile <> '') and (FileExists(fExecFile));
end;

procedure TCompilerBase.Log(const Filter: TCompLogFilter;
  const Lines: TStrings);
  {Copies filtered compiler log to string list.
    @param Filter [in] Indicates how log to be filtered i.e. return all log,
      return warnings only or return errors only.
    @param Lines [in] Lines of the log (cleared if no entries).
  }
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
  {Returns compiler log as a CRLF delimited string using specified filter.
    @param Filter [in] Indicates how log to be filtered i.e. return all log,
      return warnings only or return errors only.
    @return Text of the log, with lines separated by CRLF.
  }
var
  SL: TStringList;  // string list in which to store log
begin
  // Get filtered log in string list
  SL := TStringList.Create;
  try
    Log(Filter, SL);
    // Concatenate log lines into string and return it
    Result := Trim(SL.Text);
  finally
    SL.Free;
  end;
end;

procedure TCompilerBase.SetExecFile(const Value: string);
  {Stores full path to compiler's executable file.
    @param Value [in] Path to compiler.
  }
begin
  fExecFile := Value;
end;

procedure TCompilerBase.SetLogFilePrefixes(const Prefixes: TCompLogPrefixes);
  {Records prefixes used in interpreting error, fatal error and warning
  conditions in log files.
    @param Prefixes [in] Array of required prefix strings.
  }
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

procedure TCompilerBase.SetSwitches(const Switches: string);
  {Sets user defined switches.
    @param Switches [in] Required switches separated by commas.
  }
begin
  fSwitches := Switches;
end;

{ ECompilerError }

procedure ECompilerError.Assign(const E: Exception);
  {Assigns properties of another exception to this one.
    @param E [in] Exception whose properties are to be copied. Must be an
      ECompilerError instance.
  }
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
  {Class constructor. Creates exception instance from another exception.
    @param E [in] Instance of exception that provides information about why
      compiler failed to run.
    @param Compiler [in] Name of compiler that failed to run.
  }
begin
  Assert(Assigned(E), ClassName + '.Create: E is nil');
  Assert(Compiler <> '', ClassName + '.Create: Compiler is empty string');
  inherited Create(E.Message);
  fCompiler := Compiler;
  fErrorCode := E.ErrorCode;
end;

end.

