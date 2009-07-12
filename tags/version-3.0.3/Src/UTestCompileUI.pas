{
 * UTestCompileUI.pas
 *
 * Defines a static class that test compiles a routine in a thread and displays
 * a wait dialog if the compilation is taking a significant of time.
 *
 * v1.0 of 25 Aug 2008  - Original version.
 * v1.1 of 04 Oct 2008  - Changed TTestCompileUI to derive from
 *                        TNoConstructObject and hence prevented it from being
 *                        constructed.
 * v1.2 of 06 Jan 2009  - Changed to use a specified compilers object to perform
 *                        test compilation instead of global Compilers object.
 * v1.3 of 13 Jan 2009  - Added new TestCompileAction that encapsulates a test
 *                        compilation.
 *                      - Changed to encapsulate test compilation in an action
 *                        rather than a thread. TWaitForUIAction actually
 *                        creates the thread: we were using two threads!
 *                      - Replaced control char literals with constants.
 * v1.4 of 06 Jun 2009  - Changed result of TTestCompileAction.Execute from
 *                        True to False since it doesn't trigger OnExecute.
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
 * The Original Code is UTestCompileUI.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2008-2009 Peter
 * Johnson. All Rights Reserved.
 *
 * ***** END LICENSE BLOCK *****
}


unit UTestCompileUI;


interface


uses
  // Delphi
  SysUtils, Classes,
  // Project
  IntfCompilers, UBaseObjects, USnippets;


type

  {
  TTestCompileUI:
    Class that test compiles a snippet in a thread and displays a wait dialog if
    the compilation is taking a significant of time.
  }
  TTestCompileUI = class(TNoConstructObject)
  strict private
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
  FmWaitDlg, UCompilerBase, UConsts, UExceptions, UTestCompile,
  UWaitForActionUI;


type

  {
  TestCompileAction:
    Action that performs a test compilation of a snippet.
  }
  TTestCompileAction = class(TBasicAction)
  strict private
    fRoutine: TRoutine;     // Snippet to be compiled}
    fCompilers: ICompilers; // Object that performs test compilation
  public
    constructor Create(const ARoutine: TRoutine;
      const ACompilers: ICompilers); reintroduce;
      {Class constructor. Sets up object to perform a test compilation.
        @param Routine [in] Snippet to be compiled.
        @param ACompilers [in] Compilers object used to perform compilation.
      }
    function Execute: Boolean; override;
      {Executes test compilation.
        @return False since OnExecute is not called.
      }
  end;

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
  CompAction: TTestCompileAction;   // action that performs test compilation
begin
  CompAction := nil;
  // Set up dialog that may be displayed while compiling
  WaitDlg := TWaitDlg.Create(AOwner);
  try
    WaitDlg.Caption := sWaitCaption;
    CompAction := TTestCompileAction.Create(ARoutine, ACompilers);
    // Use action handler to execute test compile action. This handler
    // displays a dialog if compilation takes longer than 0.5 sec. If shown,
    // dialog is displayed for a minimum of 1 sec.
    try
      TWaitForActionUI.Run(CompAction, WaitDlg, 500, 1000);
    except
      HandleException(ExceptObject as Exception);
    end;
  finally
    FreeAndNil(CompAction);
    FreeAndNil(WaitDlg);
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
    raise E;
end;

{ TTestCompileAction }

constructor TTestCompileAction.Create(const ARoutine: TRoutine;
  const ACompilers: ICompilers);
  {Class constructor. Sets up object to perform a test compilation.
    @param Routine [in] Snippet to be compiled.
    @param ACompilers [in] Compilers object used to perform compilation.
  }
begin
  Assert(Assigned(ARoutine), ClassName + '.Create: ARoutine is nil');
  Assert(Assigned(ACompilers), ClassName + '.Create: ACompilers is nil');
  inherited Create(nil);
  fRoutine := ARoutine;
  fCompilers := ACompilers;
end;

function TTestCompileAction.Execute: Boolean;
  {Executes test compilation.
    @return False since OnExecute is not called.
  }
begin
  TTestCompile.Compile(fCompilers, fRoutine);
  Result := False;
end;

end.

