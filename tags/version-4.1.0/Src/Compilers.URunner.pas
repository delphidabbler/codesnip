{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2006-2012, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * Implements a class that executes a compiler and captures its output and exit
 * code. Also provides specialised exception object that captures information
 * about errors.
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

