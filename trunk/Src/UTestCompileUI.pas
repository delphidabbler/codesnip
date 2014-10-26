{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2008-2014, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * Defines a static class that test compiles a snippet in a thread and displays
 * a wait dialog if the compilation is taking more than a specified amount of
 * time.
}


unit UTestCompileUI;


interface


uses
  // Delphi
  SysUtils, Classes,
  // Project
  Compilers.UGlobals, DB.USnippet, UBaseObjects;


type

  {
  TTestCompileUI:
    Static class that test compiles a snippet in a thread and displays a wait
    dialog if the compilation is taking more than a specified amount of time.
  }
  TTestCompileUI = class(TNoConstructObject)
  strict private
    const PauseBeforeDisplay = 500; // time elapsed before dialog is displayed
    const MinDisplayTime = 1000;    // minimum time that dialog is displayed
    class procedure HandleException(const E: Exception);
      {Handles exception thrown during compilation. Re-raises exception with
      message that depends on an analysis of the exception. Compiler generated
      exceptions are treated differently to other exception types.
        @param E [in] Exception to be handled.
      }
  public
    class procedure Execute(const AOwner: TComponent;
      const ACompilers: ICompilers; const ASnippet: TSnippet);
      {Test compiles a snippet in a thread and displays a wait dialogue if
      compilation takes some time.
        @param AOwner [in] Control that owns the wait dialogue box. Dialogue is
          aligned over this control.
        @param ACompilers [in] Compilers object used to perform compilation.
        @param ASnippet [in] Snippet to be compiled.
      }
  end;


implementation


uses
  // Delphi
  Windows,
  // Project
  Compilers.UCompilerBase, FmWaitDlg, UConsts, UExceptions, UTestCompile,
  UWaitForThreadUI;


{ TTestCompileUI }

class procedure TTestCompileUI.Execute(const AOwner: TComponent;
  const ACompilers: ICompilers; const ASnippet: TSnippet);
  {Test compiles a snippet in a thread and displays a wait dialogue if
  compilation takes some time.
    @param AOwner [in] Control that owns the wait dialogue box. Dialogue is
      aligned over this control.
    @param ACompilers [in] Compilers object used to perform compilation.
    @param ASnippet [in] Snippet to be compiled.
  }
resourcestring
  // Caption for wait dialogue
  sWaitCaption = 'Compiling...';
var
  CompThread: TTestCompileThread;   // thread that performs test compilation
begin
  CompThread := TTestCompileThread.Create(ACompilers, ASnippet);
  try
    try
      TWaitForThreadUI.Run( // this blocks until thread completes
        CompThread,
        TWaitDlg.CreateAutoFree(AOwner, sWaitCaption),
        PauseBeforeDisplay,
        MinDisplayTime
      );
    except
      HandleException(ExceptObject as Exception);
    end;
  finally
    CompThread.Free;
  end;
end;

class procedure TTestCompileUI.HandleException(const E: Exception);
  {Handles exception thrown during compilation. Re-raises exception with message
  that depends on an analysis of the exception. Compiler generated exceptions
  are treated differently to other exception types.
    @param E [in] Exception to be handled.
  }
var
  CompilerErr: ECompilerError;  // reference to ECompilerError exception
resourcestring
  // Error messages for if a compiler fails to run
  sBadCompilerExe = 'Can''t run %0:s.' + EOL2
    + 'Please open the Configure Compilers dialogue box from the Tools menu '
    + 'and change the path to the compiler''s executable file.';
  sCantRunCompiler = '%0:s failed to run with message:' + EOL2 + '"%1:s"';
begin
  if E is ECompilerError then
  begin
    // Compiler error
    CompilerErr := E as ECompilerError;
    if CompilerErr.ErrorCode = ERROR_BAD_EXE_FORMAT then
      // bad exe file: get user to change
      raise ECodeSnip.CreateFmt(sBadCompilerExe, [CompilerErr.Compiler])
    else
      // other compiler error: report it
      raise ECodeSnip.CreateFmt(
        sCantRunCompiler, [CompilerErr.Compiler, CompilerErr.Message]
      );
  end
  else
    // Other kind of exception: pass it along
    raise TExceptionHelper.Clone(E);
end;

end.

