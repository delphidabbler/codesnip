{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2009-2022, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Provides objects that manage test compilation and assoicated UI, display of
 * compilation results via a callback and and compiler configuration.
}


unit UCompileMgr;


interface


uses
  // Delphi
  Classes, Controls,
  // Project
  Compilers.UGlobals, DB.USnippet, UView;


type

  {
  TCompileResultDisplay:
    Callback method used to display compiler results after a test compilation.
      @param Compilers [in] Compilers object containing compilation results.
  }
  TCompileResultDisplay = procedure(const Compilers: ICompilers) of object;

type
  {
  TCompileMgr:
    Object that performs test compilations and display of compile errors and
    warnings.
  }
  TCompileMgr = class(TComponent)
  strict private
    fLastCompiledSnippet: TSnippet; // Value of LastCompiledSnippet property
    fCompilers: ICompilers;         // Value of Compilers property
    ///  <summary>Handles database change events. Clears test compilation if
    ///  related snippet is changed or deleted.</summary>
    ///  <param name="Sender">TObject [in] Object that triggered event. Not
    ///  used.</param>
    ///  <param name="EvtInfo">IInterface [in] Object that carries information
    ///  about the database change event.</param>
    procedure DBChangeEventHandler(Sender: TObject; const EvtInfo: IInterface);
  strict protected
    property LastCompiledSnippet: TSnippet read fLastCompiledSnippet;
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
    property Compilers: ICompilers read fCompilers;
      {Compilers object to be used to perform compilation}
  end;

  {
  TMainCompileMgr:
    Extended compilation manager for use with main form. Checks if a view item
    can be compiled, permits user to configure compilers and checks for newly
    installed compilers.
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
    ///  <summary>Check for new compiler installations, get user permission to
    ///  install any that are found and register any compilers that user
    ///  selects.</summary>
    ///  <remarks>
    ///  <para>Does nothing if compiler detection is disabled or if there are
    ///  no installed but unregistered compilers.</para>
    ///  <para>Should be called at program startup.</para>
    ///  </remarks>
    procedure CheckForNewCompilerInstalls;
  end;


implementation


uses
  // Delphi
  SysUtils,
  Generics.Collections,
  // Project
  Compilers.UAutoDetect,
  Compilers.UCompilers,
  Compilers.USettings,
  DB.UMain,
  FmCompErrorDlg,
  FmCompilersDlg,
  FmRegisterCompilersDlg,
  UConsts,
  UMessageBox,
  UStrUtils,
  UTestCompileUI;


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
  fLastCompiledSnippet.Free;
  fLastCompiledSnippet := (Database as IDatabaseEdit).CreateTempSnippet(
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
  Database.AddChangeEventHandler(DBChangeEventHandler);
end;

procedure TCompileMgr.DBChangeEventHandler(Sender: TObject;
  const EvtInfo: IInterface);
var
  EventInfo: IDatabaseChangeEventInfo;  // information about the event
begin
  if not Assigned(fLastCompiledSnippet) then
    Exit;
  EventInfo := EvtInfo as IDatabaseChangeEventInfo;
  if not (EventInfo.Kind in [evBeforeSnippetChange, evBeforeSnippetDelete]) then
    Exit;
  Assert(EventInfo.Info is TSnippet,
    ClassName + '.DBChangeEventHandler: EventInfo is not TSnippet');
  if (EventInfo.Info as TSnippet).IsEqual(fLastCompiledSnippet) then
    // Snippet being changed is last compiled snippet: free and nil it so to
    // ensure incorrect results can't be viewed.
    FreeAndNil(fLastCompiledSnippet);
end;

destructor TCompileMgr.Destroy;
  {Class destructor. Tears down object.
  }
begin
  Database.RemoveChangeEventHandler(DBChangeEventHandler);
  fLastCompiledSnippet.Free;
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

procedure TCompileMgr.ShowErrors;
  {Shows dialog box containing all errors and warnings for last compiled
  snippet.
  }
begin
  Assert(HaveErrors,
    ClassName + '.ShowErrors: No compilers have errors or warnings');
  Assert(Assigned(fLastCompiledSnippet),
    ClassName + '.ShowErrors: LastCompiledSnippet is nil');
  TCompErrorDlg.Execute(Owner, fLastCompiledSnippet, fCompilers);
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

procedure TMainCompileMgr.CheckForNewCompilerInstalls;
var
  CandidateCompilers: TCompilerList;  // compilers available for registration
  SelectedCompilers: TCompilerList;   // compilers chosen for registration
  Persister: IPersistCompilers;       // object to store compiler data in config

  // Display message box informing user of which compilers were registered
  // MUST be called with non zero number of registered compilers
  procedure NotifyResults;
  var
    CompList: string;     // string containing list of compilers registered
    Compiler: ICompiler;  // each compiler
    RegCount: Integer;    // count of compilers registered
  resourcestring
    sPrefixS = 'The following compiler was registered:';
    sPrefixP = 'The following compilers were registered:';
    sNoRegistrations = 'Unexpected error. None of the requested compilers were '
      + 'registered.';
  begin
    CompList := '';
    RegCount := 0;
    for Compiler in Compilers do
    begin
      if (SelectedCompilers.IndexOf(Compiler) >= 0)
        and Compiler.IsAvailable then
      begin
        CompList := CompList + '• ' + Compiler.GetName + EOL;
        Inc(RegCount);
      end;
    end;
    if RegCount > 0 then
    begin
      CompList := StrIf(RegCount = 1, sPrefixS, sPrefixP) + EOL2 + CompList;
      TMessageBox.Information(Owner, CompList);
    end
    else
      TMessageBox.Error(Owner, sNoRegistrations);
  end;

  // Display message to user informing that no compilers were registred
  // MUST be called only with a zero number of registered compilers
  procedure NotifyNoRegistrations;
  resourcestring
    sNothingRegistered = 'No compilers were selected for registration';
  begin
    TMessageBox.Information(Owner, sNothingRegistered);
  end;

begin
  if not TCompilerSettings.PermitStartupDetection then
    Exit;
  SelectedCompilers := nil;
  CandidateCompilers := TCompilerList.Create;
  try
    SelectedCompilers := TCompilerList.Create;
    // Build list of compilers that are installed but not registered
    TCompilerAutoDetect.ListRegisterableCompilers(
      Self.Compilers, CandidateCompilers
    );
    if CandidateCompilers.Count = 0 then
      Exit;   // no compilers to register

    // We have candidate compilers: get user to select
    if TRegisterCompilersDlg.Execute(
      Owner,
      CandidateCompilers,
      SelectedCompilers
    ) then
    begin
      if SelectedCompilers.Count > 0 then
      begin
        // User selected one or more compilers to register
        // register compiler(s)
        TCompilerAutoDetect.RegisterSpecificCompilers(
          Compilers, SelectedCompilers
        );
        // update config file with changes
        Persister := TPersistCompilers.Create;
        Persister.Save(Compilers);
        // tell user what got registered
        NotifyResults;
      end
      else
        // User didn't select a file: tell them
        NotifyNoRegistrations;
    end;
  finally
    SelectedCompilers.Free;
    CandidateCompilers.Free;
  end;
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

