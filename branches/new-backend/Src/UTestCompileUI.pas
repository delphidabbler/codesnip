{
 * UTestCompileUI.pas
 *
 * Defines a static class that test compiles a snippet in a thread and displays
 * a wait dialog if the compilation is taking more than a specified amount of
 * time.
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
 * The Original Code is UTestCompileUI.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2008-2010 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
}


unit UTestCompileUI;


interface


uses
  // Delphi
  SysUtils, Classes,
  // Project
  Compilers.UGlobals, UBaseObjects, USnippets;


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
      const ACompilers: ICompilers; const ARoutine: TRoutine);
      {Test compiles a snippet in a thread and displays a wait dialog if
      compilation takes some time.
        @param AOwner [in] Control that owns the wait dialog box. Dialog is
          aligned over this control.
        @param ACompilers [in] Compilers object used to perform compilation.
        @param ARoutine [in] Snippet to be compiled.
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
  const ACompilers: ICompilers; const ARoutine: TRoutine);
  {Test compiles a snippet in a thread and displays a wait dialog if compilation
  takes some time.
    @param AOwner [in] Control that owns the wait dialog box. Dialog is aligned
      over this control.
    @param ACompilers [in] Compilers object used to perform compilation.
    @param Routine [in] Snippet to be compiled.
  }
resourcestring
  // Caption for wait dialog
  sWaitCaption = 'Compiling...';
var
  WaitDlg: TWaitDlg;                // dialog box to display while compiling
  CompThread: TTestCompileThread;   // thread that performs test compilation
begin
  CompThread := nil;
  // Set up dialog that may be displayed while compiling
  WaitDlg := TWaitDlg.Create(AOwner);
  try
    WaitDlg.Caption := sWaitCaption;
    // Do the compilation
    CompThread := TTestCompileThread.Create(ACompilers, ARoutine);
    try
      TWaitForThreadUI.Run( // this blocks until thread completes
        CompThread, WaitDlg, PauseBeforeDisplay, MinDisplayTime
      );
    except
      HandleException(ExceptObject as Exception);
    end;
  finally
    CompThread.Free;
    WaitDlg.Free;
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
    + 'Please open the Configure Compilers dialog box from the Tools menu and '
    + 'change the path to the compiler''s executable file.';
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

