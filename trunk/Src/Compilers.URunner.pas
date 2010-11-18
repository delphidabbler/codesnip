{
 * Compilers.URunner.pas
 *
 * Implements a class that executes a compiler and captures its output and exit
 * code. Also provides specialised exception object that captures information
 * about errors.
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
 * The Original Code is Compilers.URunner.pas, formerly UCompilerRunner.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2006-2010 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
}


unit Compilers.URunner;


interface


uses
  // Delphi
  SysUtils, Classes,
  // Project
  UConsoleApp, UExceptions, UPipe;


type

  {
  TCompilerRunner:
    Class that executes a compiler and captures its output and exit code.
  }
  TCompilerRunner = class(TObject)
  strict private
    fOutStream: TStream;
      {Reference to output stream passed to Execute}
    fOutPipe: TPipe;
      {Pipe used as intermediary between compiler and output stream}
    procedure HandleOutput(Sender: TObject);
      {Handles TConsoleApp's OnWork event by copying current pipe contents to
      output stream.
        @param Sender [in] Not used.
      }
  public
    function Execute(const CommandLine, CurrentPath: string;
      const OutStream: TStream): LongWord;
      {Execute compiler and capture output.
        @param CommandLine [in] Command line used to execute compiler. Should
          contain path to executable, name of file to compile and command line
          switches.
        @param CurrentPath [in] Current directory for compiler.
        @param OutStream [in] Stream to receive compiler's output.
        @return Compiler's exit code.
        @except ECompilerRunner raised if compiler execution fails.
      }
  end;

  {
  ECompilerRunner:
    Class of exception raised by TCompilerRunner.
  }
  ECompilerRunner = class(ECodeSnip)
  strict private
    fErrorCode: Integer;
      {Value of ErrorCode property}
  public
    constructor Create(const CompilerApp: TConsoleApp); overload;
      {Class constructor. Creates exception instance.
        @param E [in] Instance of application object that provides information
          about why compiler failed to run.
      }
    procedure Assign(const E: Exception); override;
      {Assigns properties of another exception to this one.
        @param E [in] Exception whose properties are to be copied. Must be an
          ECompilerRunner instance.
      }
    property ErrorCode: Integer
      read fErrorCode;
      {Error code describing why compiler failed to run}
  end;


implementation


{ TCompilerRunner }

function TCompilerRunner.Execute(const CommandLine, CurrentPath: string;
  const OutStream: TStream): LongWord;
  {Execute compiler and capture output.
    @param CommandLine [in] Command line used to execute compiler. Should
      contain path to executable, name of file to compile and command line
      switches.
    @param CurrentPath [in] Current directory for compiler.
    @param OutStream [in] Stream to receive compiler's output.
    @return Compiler's exit code.
    @except ECompilerRunner raised if compiler execution fails.
  }
var
  App: TConsoleApp; // object that manages execution of compiler
begin
  Result := 0;    // keeps compiler happy
  fOutStream := OutStream;
  App := TConsoleApp.Create;
  try
    // Redirect compiler standard output and standard error to pipe
    fOutPipe := TPipe.Create;
    App.StdOut := fOutPipe.WriteHandle;
    App.StdErr := fOutPipe.WriteHandle;
    // Run app hidden with max run time of 10 seconds
    App.Visible := False;
    App.TimeSlice := 20;
    App.MaxExecTime := 10 * cOneSecInMS;
    // Run the compiler, recording output and exit code
    App.OnWork := HandleOutput;
    if not App.Execute(CommandLine, CurrentPath) then
      raise ECompilerRunner.Create(App);
    Result := App.ExitCode;
  finally
    FreeAndNil(fOutPipe);
    FreeAndNil(App);
  end;
end;

procedure TCompilerRunner.HandleOutput(Sender: TObject);
  {Handles TConsoleApp's OnWork event by copying current pipe contents to output
  stream.
    @param Sender [in] Not used.
  }
begin
  fOutPipe.CopyToStream(fOutStream);
end;

{ ECompilerRunner }

procedure ECompilerRunner.Assign(const E: Exception);
  {Assigns properties of another exception to this one.
    @param E [in] Exception whose properties are to be copied. Must be an
      ECompilerRunner instance.
  }
begin
  Assert(E is ECompilerRunner,
    ClassName + '.Assign: E is not a ECompilerRunner instance.'
  );
  inherited;
  fErrorCode := (E as ECompilerRunner).fErrorCode;
end;

constructor ECompilerRunner.Create(const CompilerApp: TConsoleApp);
  {Class constructor. Creates exception instance.
    @param E [in] Instance of application object that provides information about
      why compiler failed to run.
  }
begin
  Assert(Assigned(CompilerApp), ClassName + '.Create: App is nil');
  Assert(CompilerApp.ErrorCode <> 0, ClassName + '.Create: App has no errors');
  inherited Create(CompilerApp.ErrorMessage);
  fErrorCode := CompilerApp.ErrorCode;
end;

end.

