{
 * UCompileMgr.pas
 *
 * Provides object that manage test compilation and assoicated UI, display of
 * compilation results via a callback and and compiler configuration.
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
 * The Original Code is UCompileMgr.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2009-2011 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
}


unit UCompileMgr;


interface


uses
  // Delphi
  Classes, Controls,
  // Project
  Compilers.UGlobals, USnippets, UView;


type

  {
  TCompileResultDisplay:
    Callback method used to display compiler results after a test compilation.
      @param Compilers [in] Compilers object containing compilation results.
  }
  TCompileResultDisplay = procedure(const Compilers: ICompilers) of object;

  {
  TCompileMgr:
    Object that performs test compilations and display of compile errors and
    warnings.
  }
  TCompileMgr = class(TComponent)
  strict private
    fLastCompiledSnipper: TSnippet; // Value of LastCompiledSnippet property
    fCompilers: ICompilers;         // Value of Compilers property
  strict protected
    property LastCompiledSnippet: TSnippet read fLastCompiledSnipper;
      {Last compiled snippet. May not be added to Snippets object}
  public
    constructor Create(AOwner: TComponent); override;
      {Class constructor. Sets up object.
        @param AOwner [in] Component that owns this object.
      }
    destructor Destroy; override;
      {Class destructor. Tears down object.
      }
    function HaveCompilers: Boolean;
      {Checks if any compilers are set up to work with CodeSnip.
        @return True if at least one compiler is available, False otherwise.
      }
    procedure Compile(const UIParent: TWinControl; const Snippet: TSnippet;
      const DisplayProc: TCompileResultDisplay = nil);
      {Test compiles a snippet and then displays the compilation results. Shows
      a wait dialog box if compilation takes a long time.
        @param UIParent [in] Control that parents any wait window that is
          displayed. Wait window aligned above this control.
        @param Snippet [in] Snippet to be compiled. Stored in
          LastCompiledSnippet property.
        @param DisplayProc [in] Callback method called to display compilation
          results.
      }
    function HaveErrors: Boolean;
      {Checks if any compiler object has a compilation error or warning to
      report.
        @return True if there are any errors, False otherwise.
      }
    procedure ShowErrors;
      {Shows dialog box containing all errors and warnings for last compiled
      snippet.
      }
    procedure ShowError(const CompilerID: TCompilerID);
      {Shows compile error or warning for last compiled snippet on a specified
      compiler.
        @param CompilerID [in] Id of compiler whose errors are to be displayed.
      }
    property Compilers: ICompilers read fCompilers;
      {Compilers object to be used to perform compilation}
  end;

  {
  TMainCompileMgr:
    Extended compilation manager for use with main form. Checks if a view item
    can be compiled and also permits user to configure compilers.
  }
  TMainCompileMgr = class(TCompileMgr)
  public
    function CanCompile(View: IView): Boolean;
      {Checks if the object represented by a view item can be test compiled.
        @param View [in] View item to test.
        @return True if view can be compiled, False otherwise.
      }
    function IsLastCompiledView(View: IView): Boolean;
      {Checks if the object represented by a view item is the last one that was
      test compiled.
        @param View [in] View item to test.
        @return True if view represents last object to be compiled, False
          otherwise.
      }
    function ConfigCompilers: Boolean;
      {Displays Configure Compilers dialog to permit user to update compiler
      properties.
        @return True if user accepts changes, False if not.
      }
  end;


implementation


uses
  // Delphi
  SysUtils,
  // Project
  Compilers.UCompilers, FmCompErrorDlg, FmCompilersDlg, UTestCompileUI;


{ TCompileMgr }

procedure TCompileMgr.Compile(const UIParent: TWinControl;
  const Snippet: TSnippet; const DisplayProc: TCompileResultDisplay);
  {Test compiles a snippet and then displays the compilation results. Shows a
  wait dialog box if compilation takes a long time.
    @param UIParent [in] Control that parents any wait window that is displayed.
      Wait window aligned above this control.
    @param Snippet [in] Snippet to be compiled. Stored in LastCompiledSnippet
       property.
    @param DisplayProc [in] Callback method called to display compilation
      results.
  }
begin
  Assert(Assigned(Snippet), ClassName + '.Compile: Snippet is nil');
  // Compile snippet and optionally display result
  TTestCompileUI.Execute(UIParent, fCompilers, Snippet);
  if Assigned(DisplayProc) then
    DisplayProc(fCompilers);
  // Copy snippet to LastCompiledSnippet property
  fLastCompiledSnipper.Free;
  fLastCompiledSnipper := (Snippets as ISnippetsEdit).CreateTempSnippet(
    Snippet
  );
end;

constructor TCompileMgr.Create(AOwner: TComponent);
  {Class constructor. Sets up object.
    @param AOwner [in] Component that owns this object.
  }
begin
  inherited Create(AOwner);
  fCompilers := TCompilersFactory.CreateAndLoadCompilers;
end;

destructor TCompileMgr.Destroy;
  {Class destructor. Tears down object.
  }
begin
  fLastCompiledSnipper.Free;
  fCompilers := nil;
  inherited;
end;

function TCompileMgr.HaveCompilers: Boolean;
  {Checks if any compilers are set up to work with CodeSnip.
    @return True if at least one compiler is available, False otherwise.
  }
begin
  Result := (fCompilers.AvailableCount > 0);
end;

function TCompileMgr.HaveErrors: Boolean;
  {Checks if any compiler object has a compilation error or warning to report.
    @return True if there are any errors, False otherwise.
  }
var
  Compiler: ICompiler;  // references each compiler
begin
  Result := False;
  for Compiler in fCompilers do
  begin
    if Compiler.HasErrorsOrWarnings then
    begin
      Result := True;
      Exit;
    end;
  end;
end;

procedure TCompileMgr.ShowError(const CompilerID: TCompilerID);
  {Shows compile error or warning for last compiled snippet on a specified
  compiler.
    @param CompilerID [in] Id of compiler whose errors are to be displayed.
  }
var
  Compiler: ICompiler;  // reference to required compiler
begin
  Compiler := fCompilers[CompilerID];
  Assert(Compiler.HasErrorsOrWarnings,
    ClassName + '.ShowError: Compiler has no errors or warnings');
  Assert(Assigned(fLastCompiledSnipper),
    ClassName + '.ShowError: LastCompiledSnippet is nil');
  TCompErrorDlg.Execute(Owner, fLastCompiledSnipper.ID, Compiler);
end;

procedure TCompileMgr.ShowErrors;
  {Shows dialog box containing all errors and warnings for last compiled
  snippet.
  }
begin
  Assert(HaveErrors,
    ClassName + '.ShowErrors: No compilers have errors or warnings');
  Assert(Assigned(fLastCompiledSnipper),
    ClassName + '.ShowErrors: LastCompiledSnippet is nil');
  TCompErrorDlg.Execute(Owner, fLastCompiledSnipper.ID, fCompilers);
end;

{ TMainCompileMgr }

function TMainCompileMgr.CanCompile(View: IView): Boolean;
  {Checks if the object represented by a view item can be test compiled.
    @param View [in] View item to test.
    @return True if view can be compiled, False otherwise.
  }
var
  SnippetView: ISnippetView;  // view as snippet view if supported
begin
  Result := Assigned(View)
    and HaveCompilers
    and Supports(View, ISnippetView, SnippetView)
    and SnippetView.Snippet.CanCompile;
end;

function TMainCompileMgr.ConfigCompilers: Boolean;
  {Displays Configure Compilers dialog to permit user to update compiler
  properties.
    @return True if user accepts changes, False if not.
  }
begin
  Result := TCompilersDlg.Execute(Owner, Compilers);
end;

function TMainCompileMgr.IsLastCompiledView(View: IView): Boolean;
  {Checks if the object represented by a view item is the last one that was test
  compiled.
    @param View [in] View item to test.
    @return True if view represents last object to be compiled, False otherwise.
  }
var
  SnippetView: ISnippetView;  // view as snippet view if supported
begin
  Result := Assigned(View)
    and Assigned(LastCompiledSnippet)
    and Supports(View, ISnippetView, SnippetView)
    and SnippetView.Snippet.IsEqual(LastCompiledSnippet);
end;

end.

