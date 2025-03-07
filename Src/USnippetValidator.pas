{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2009-2021, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Implements a static class that checks a snippet for validity.
}


unit USnippetValidator;


interface


uses
  // Project
  ActiveText.UMain,
  DB.SnippetKind,
  DB.Snippets,
  DB.Vaults,
  UBaseObjects,
  UStructs;


type
  {
  TSnippetValidator:
    Static class that checks a snippet for validity.
  }
  TSnippetValidator = class(TNoConstructObject)
  strict private
    const
      cAllSnippetKinds: TSnippetKinds =   // Set of all possible snippet kinds
        [skFreeform, skRoutine, skConstant, skTypeDef, skUnit, skClass];
  public

    ///  <summary>Validate a snippet display name.</summary>
    ///  <param name="DisplayName"><c>string</c> [in] Display name to be
    ///  checked.</param>
    ///  <param name="ErrorMsg"><c>string</c> [out] Message that describes
    ///  error. Undefined if <c>True</c> returned.</param>
    ///  <returns><c>Boolean</c>. <c>True</c> if source code is valid or
    ///  <c>False</c> if not.</returns>
    class function ValidateDisplayName(const DisplayName: string;
      out ErrorMsg: string): Boolean;

    class function ValidateDependsList(const Snippet: TSnippet;
      out ErrorMsg: string): Boolean; overload;
      {Recursively checks dependency list of a snippet for validity.
        @param Snippet [in] Snippet for which dependencies are to be checked.
        @param ErrorMsg [out] Message that describes error. Undefined if True
          returned.
        @return True if dependency list is valid or False if not.
      }

    ///  <summary>Recursively checks dependency list of a snippet for validity.
    ///  </summary>
    ///  <param name="AKey"><c>string</c> [in] Key of snippet for which
    ///  dependencies are to be checked.</param>
    ///  <param name="AVaultID"><c>TVaultID</c> [in] ID of the vault to which
    ///  the snippet belongs.</param>
    ///  <param name="AData"><c>TSnippetEditData</c> [in] Data describing
    ///  properties and references of snippet for which dependencies are to be
    ///  checked.</param>
    ///  <param name="AErrorMsg"><c>string</c> [out] Message that describes any
    ///  error. Undefined if <c>True</c> is returned.</param>
    ///  <returns><c>Boolean</c>. <c>True</c> if dependency list is valid or
    ///  <c>False</c> if not.</returns>
    class function ValidateDependsList(const AKey: string;
      const AVaultID: TVaultID; const AData: TSnippetEditData;
      out AErrorMsg: string): Boolean; overload;

    class function ValidateSourceCode(const Source: string;
      out ErrorMsg: string; out ErrorSel: TSelection): Boolean;
      {Validates a source code from a snippet.
        @param Source [in] Source code to be checked.
        @param ErrorMsg [out] Message that describes error. Undefined if True
          returned.
        @param ErrorSel [out] Selection that can be used to highlight error.
        @return True if source code is valid or False if not.
      }
    class function ValidateDescription(const Desc: string; out ErrorMsg: string;
      out ErrorSel: TSelection): Boolean;
      {Validates a description code from a snippet.
        @param Desc [in] Description to be checked.
        @param ErrorMsg [out] Message that describes error. Undefined if True
          returned.
        @param ErrorSel [out] Selection that can be used to highlight error.
        @return True if description is valid or False if not.
      }
    class function ValidateExtra(const Extra: IActiveText;
      out ErrorMsg: string): Boolean;
      {Validates a extra information from a snippet.
        @param Extra [in] Extra information to be checked.
        @param ErrorMsg [out] Message that describes error. Undefined if True
          returned.
        @return True if extra information is valid, False if not.
      }
    class function Validate(const Snippet: TSnippet; out ErrorMsg: string):
      Boolean; overload;
      {Checks a snippet for validity.
        @param Snippet [in] Snippet to be checked.
        @param ErrorMsg [out] Message that describes error. Undefined if True
          returned.
        @return True if snippet valid or False if not.
      }
    class function Validate(const Snippet: TSnippet; out ErrorMsg: string;
      out ErrorSel: TSelection): Boolean; overload;
      {Checks a snippet for validity.
        @param Snippet [in] Snippet to be checked.
        @param ErrorMsg [out] Message that describes error. Undefined if True
          returned.
        @param ErrorSel [out] Selection that can be used to highlight error.
        @return True if snippet valid or False if not.
      }
    class function ValidDependsKinds(const Kind: TSnippetKind): TSnippetKinds;
      {Gets set of snippet kinds that are valid in a snippet's dependency list.
        @param Kind [in] Kind of snippet for which valid dependency kinds
          required.
        @return Set of valid kinds for snippets in dependency list.
      }
  end;


implementation


uses
  // Delphi
  SysUtils,
  // Project
  ActiveText.UValidator,
  DB.Main,
  UStrUtils;


{ TSnippetValidator }

class function TSnippetValidator.Validate(const Snippet: TSnippet;
  out ErrorMsg: string): Boolean;
  {Checks a snippet for validity.
    @param Snippet [in] Snippet to be checked.
    @param ErrorMsg [out] Message that describes error. Undefined if True
      returned.
    @return True if snippet valid or False if not.
  }
var
  DummySel: TSelection; // unused parameter to overloaded Validate call
begin
  Result := Validate(Snippet, ErrorMsg, DummySel);
end;

class function TSnippetValidator.Validate(const Snippet: TSnippet;
  out ErrorMsg: string; out ErrorSel: TSelection): Boolean;
  {Checks a snippet for validity.
    @param Snippet [in] Snippet to be checked.
    @param ErrorMsg [out] Message that describes error. Undefined if True
      returned.
    @param ErrorSel [out] Selection that can be used to highlight error.
    @return True if snippet valid or False if not.
  }
begin
  Result :=
    {TODO -cVault: Add validation of display name here}
    ValidateDescription(Snippet.Description.ToString, ErrorMsg, ErrorSel)
    and ValidateSourceCode(Snippet.SourceCode, ErrorMsg, ErrorSel)
    and ValidateDependsList(Snippet, ErrorMsg)
    and ValidateExtra(Snippet.Extra, ErrorMsg);
end;

class function TSnippetValidator.ValidateDependsList(const Snippet: TSnippet;
  out ErrorMsg: string): Boolean;
  {Recursively checks dependency list of a snippet for validity.
    @param Snippet [in] Snippet for which dependencies are to be checked.
    @param ErrorMsg [out] Message that describes error. Undefined if True
      returned.
    @return True if dependency list is valid or False if not.
  }

  // ---------------------------------------------------------------------------
  function DependsListIsCircular(const Snippet: TSnippet;
    const DependsList: TSnippetList): Boolean;
    {Checks if dependency list is circular, i.e. a snippet is referenced in own
    chain of dependencies. Recursive function.
      @param Snippet [in] Snippet to be checked.
      @param DependsList [in] A dependency list.
      @return True if dependency list is circular, false if not.
    }
  var
    RequiredSnippet: TSnippet;  // iterates through DependsList
  begin
    Result := False;
    for RequiredSnippet in DependsList do
    begin
      if RequiredSnippet.ID = Snippet.ID then
        Result := True
      else
        Result := DependsListIsCircular(Snippet, RequiredSnippet.Depends);
      if Result then
        Exit;
    end;
  end;

  function DependsListHasKinds(const DependsList: TSnippetList;
    const Kinds: TSnippetKinds): Boolean;
    {Recursively checks if a dependency list contains snippets of specified
    kinds.
      @param DependsList [in] A dependency list.
      @param Kinds [in] Set of snippet kinds to check for.
      @return True if one or more of specified kinds are found, false if not.
    }
  var
    RequiredSnippet: TSnippet;  // iterates through depends list
  begin
    Result := False;
    if Kinds = [] then
      Exit; // no kinds specified so depends list can't have these kinds!
    for RequiredSnippet in DependsList do
    begin
      if RequiredSnippet.Kind in Kinds then
        Result := True
      else
        Result := DependsListHasKinds(RequiredSnippet.Depends, Kinds);
      if Result then
        Exit;
    end;
  end;
  // ---------------------------------------------------------------------------

resourcestring
  // Error messages
  sInvalidKind = 'Invalid snippet kind "%0:s" in depends list for snippet '
    + 'with key "%1:s"';
  sCircular = '%0:s snippet with key "%1:s" cannot depend on itself.';
var
  DeniedDepends: TSnippetKinds; // snippet kinds that can't be in depends list
begin
  // No snippets kinds may depend on themselves
  // ** MUST do circularity test before any other. Other tests MUST NOT be
  // applied if this test fails: endless loop could result
  Result := not DependsListIsCircular(Snippet, Snippet.Depends);
  if not Result then
  begin
    ErrorMsg := Format(
      sCircular, [
        TSnippetKindInfoList.Items[Snippet.Kind].DisplayName,
        Snippet.Key
      ]
    );
    Exit;
  end;
  // Now check kinds of snippets in dependency list for validity.
  // determine which snippet kinds can't appear in a dependency list
  DeniedDepends := cAllSnippetKinds - ValidDependsKinds(Snippet.Kind);
  // check dependency list for invalid kinds
  Result := not DependsListHasKinds(Snippet.Depends, DeniedDepends);
  if not Result then
    ErrorMsg := Format(
      sInvalidKind,
      [
        TSnippetKindInfoList.Items[Snippet.Kind].DisplayName,
        Snippet.Key
      ]
    );
end;

class function TSnippetValidator.ValidateDependsList(const AKey: string;
  const AVaultID: TVaultID; const AData: TSnippetEditData;
  out AErrorMsg: string): Boolean;
var
  TempSnippet: TSnippet;  // temporary snippet that is checked for dependencies
begin
  TempSnippet := (Database as IDatabaseEdit).CreateTempSnippet(
    AKey, AVaultID, AData
  );
  try
    Result := ValidateDependsList(TempSnippet, AErrorMsg);
  finally
    TempSnippet.Free;
  end;
end;

class function TSnippetValidator.ValidateDescription(const Desc: string;
  out ErrorMsg: string; out ErrorSel: TSelection): Boolean;
  {Validates a description code from a snippet.
    @param Desc [in] Description to be checked.
    @param ErrorMsg [out] Message that describes error. Undefined if True
      returned.
    @param ErrorSel [out] Selection that can be used to highlight error.
    @return True if description is valid or False if not.
  }
resourcestring
  // Error messages
  sErrNoDesc = 'A description must be provided';
  sErrDescHasClosingBrace = 'Description must not contain a ''}'' character';
const
  ClosingBrace = '}';
begin
  Result := False;
  if StrTrim(Desc) = '' then
  begin
    ErrorMsg := sErrNoDesc;
    ErrorSel := TSelection.Create(0, Length(Desc));
  end
  else if StrContainsStr(ClosingBrace, Desc) then
  begin
    ErrorMsg := sErrDescHasClosingBrace;
    ErrorSel := TSelection.Create(
      StrPos(ClosingBrace, Desc) - 1, Length(ClosingBrace)
    );
  end
  else
    Result := True;
end;

class function TSnippetValidator.ValidateDisplayName(const DisplayName: string;
  out ErrorMsg: string): Boolean;
var
  TrimmedDisplayName: string;
resourcestring
  sErrEmpty = 'A display name must be provided';
begin
  TrimmedDisplayName := StrTrim(DisplayName);
  Result := TrimmedDisplayName <> '';
  if not Result then
    ErrorMsg := sErrEmpty;
end;

class function TSnippetValidator.ValidateExtra(const Extra: IActiveText;
  out ErrorMsg: string): Boolean;
  {Validates a extra information from a snippet.
    @param Extra [in] Extra information to be checked.
    @param ErrorMsg [out] Message that describes error. Undefined if True
      returned.
    @return True if extra information is valid, False if not.
  }
var
  ErrorInfo: TActiveTextValidator.TErrorInfo; // info about error
begin
  Result :=  TActiveTextValidator.Validate(Extra, ErrorInfo);
  if not Result then
    ErrorMsg := ErrorInfo.Description;
end;

class function TSnippetValidator.ValidateSourceCode(const Source: string;
  out ErrorMsg: string; out ErrorSel: TSelection): Boolean;
  {Validates a source code from a snippet.
    @param Source [in] Source code to be checked.
    @param ErrorMsg [out] Message that describes error. Undefined if True
      returned.
    @param ErrorSel [out] Selection that can be used to highlight error.
    @return True if source code is valid or False if not.
  }
resourcestring
  // Error message
  sErrNoSource = 'Some source code must be provided';
begin
  // Source code must be provided
  Result := StrTrim(Source) <> '';
  if not Result then
  begin
    ErrorMsg := sErrNoSource;
    ErrorSel := TSelection.Create(0, Length(Source));
  end;
end;

class function TSnippetValidator.ValidDependsKinds(
  const Kind: TSnippetKind): TSnippetKinds;
  {Gets set of snippet kinds that are valid in a snippet's dependency list.
    @param Kind [in] Kind of snippet for which valid dependency kinds required.
    @return Set of valid kinds for snippets in dependenc list.
  }
begin
  case Kind of
    skFreeform: Result := [skRoutine, skConstant, skTypeDef, skFreeform];
    skRoutine: Result := [skRoutine, skConstant, skTypeDef, skClass];
    skConstant: Result := [skConstant, skTypeDef];
    skTypeDef: Result := [skConstant, skTypeDef, skClass];
    skUnit: Result := [];
    skClass: Result := [skRoutine, skConstant, skTypeDef, skClass];
  end;
end;

end.

