{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2009-2013, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * Implements a static class that checks a snippet for validity.
}


unit USnippetValidator;


interface


uses
  // Project
  CS.ActiveText,
  DB.USnippet,
  DB.USnippetKind,
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
    class function ValidateName(const Name: string;
      const CheckForUniqueness: Boolean; out ErrorMsg: string;
      out ErrorSel: TSelection): Boolean; overload;
      {Validates a snippet's name.
        @param Name [in] Snippet name to be checked.
        @param CheckForUniqueness [in] Flag indicating whether a check should
          be made to see if snippet name is already in user database.
        @param ErrorMsg [out] Message that describes error. Undefined if True
          returned.
        @param ErrorSel [out] Selection that can be used to highlight error.
        @return True if name is valid or False if not.
      }
  public
    class function ValidateDependsList(const Snippet: TSnippet;
      out ErrorMsg: string): Boolean; overload;
      {Recursively checks dependency list of a snippet for validity.
        @param Snippet [in] Snippet for which dependencies are to be checked.
        @param ErrorMsg [out] Message that describes error. Undefined if True
          returned.
        @return True if dependency list is valid or False if not.
      }
    class function ValidateDependsList(const SnippetName: string;
      const Data: TSnippetEditData; out ErrorMsg: string): Boolean; overload;
      {Recursively checks dependency list of a snippet for validity.
        @param SnippetName [in] Name of snippet for which dependencies are to be
          checked.
        @param Data [in] Data describing properties and references of snippet
          for which dependencies are to be checked.
        @param ErrorMsg [out] Message that describes error. Undefined if True
          returned.
        @return True if dependency list is valid or False if not.
      }
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
    class function ValidateName(const Name: string;
      const CheckForUniqueness: Boolean): Boolean; overload;
      {Validates a snippet's name.
        @param Name [in] Snippet name to be checked.
        @param CheckForUniqueness [in] Flag indicating whether a check should
          be made to see if snippet name is already in user database.
        @return True if name is valid or False if not.
      }
    class function ValidateName(const Name: string;
      const CheckForUniqueness: Boolean; out ErrorMsg: string): Boolean;
      overload;
      {Validates a snippet's name.
        @param Name [in] Snippet name to be checked.
        @param CheckForUniqueness [in] Flag indicating whether a check should
          be made to see if snippet name is already in user database.
        @param ErrorMsg [out] Message that describes error. Undefined if True
          returned.
        @return True if name is valid or False if not.
      }
    ///  <summary>Validates a snippet's display name.</summary>
    ///  <param name="DisplayName">string [in] Display name to be validated.
    ///  </param>
    ///  <param name="ErrorMsg">string [out] Message that describes error. -
    ///  Undefined if True returned.</param>
    ///  <returns>Boolean. True if display name is valid or False if not.
    ///  </returns>
    class function ValidateDisplayName(const DisplayName: string;
      out ErrorMsg: string): Boolean;
    class function ValidateNotes(const Notes: IActiveText;
      out ErrorMsg: string): Boolean;
      {Validates a snippet's notes.
        @param Notes [in] Notes information to be checked.
        @param ErrorMsg [out] Message that describes error. Undefined if True
          returned.
        @return True if notes are valid, False if not.
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
  CS.ActiveText.Validator,
  DB.UMain,
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
  Result := ValidateName(Snippet.ID.ToString, False, ErrorMsg, ErrorSel)
    and ValidateDescription(Snippet.Description.ToString, ErrorMsg, ErrorSel)
    and ValidateSourceCode(Snippet.SourceCode, ErrorMsg, ErrorSel)
    and ValidateDependsList(Snippet, ErrorMsg)
    and ValidateNotes(Snippet.Notes, ErrorMsg);
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
    + 'named "%1:s"';
  sCircular = '%0:s Snippet named "%1:s" cannot depend on itself.';
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
        Snippet.ID.ToString
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
        Snippet.ID.ToString
      ]
    );
end;

class function TSnippetValidator.ValidateDependsList(const SnippetName: string;
  const Data: TSnippetEditData; out ErrorMsg: string): Boolean;
  {Recursively checks dependency list of a snippet for validity.
    @param SnippetName [in] Name of snippet for which dependencies are to be
      checked.
    @param Data [in] Data describing properties and references of snippet for
      which dependencies are to be checked.
    @param ErrorMsg [out] Message that describes error. Undefined if True
      returned.
    @return True if dependency list is valid or False if not.
  }
var
  TempSnippet: TSnippet;  // temporary snippet that is checked for dependencies
begin
  TempSnippet := (Database as IDatabaseEdit).CreateTempSnippet(
    Data, SnippetName
  );
  try
    Result := ValidateDependsList(TempSnippet, ErrorMsg);
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
  if StrIsBlank(Desc) then
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
resourcestring
  sErrNoName = 'A title must be provided';
begin
  Result := not StrIsBlank(DisplayName);
  if not Result then
    ErrorMsg := sErrNoName;
end;

class function TSnippetValidator.ValidateName(const Name: string;
  const CheckForUniqueness: Boolean; out ErrorMsg: string): Boolean;
  {Validates a snippet's name.
    @param Name [in] Snippet name to be checked.
    @param CheckForUniqueness [in] Flag indicating whether a check should be
      made to see if snippet name is already in user database.
    @param ErrorMsg [out] Message that describes error. Undefined if True
      returned.
    @return True if name is valid or False if not.
  }
resourcestring
  // Error messages
  sErrNoName = 'A name must be provided';
  sErrDupName = '"%s" is already in the database. Please choose another name';
  sErrBadName = '"%s" is not a valid Pascal identifier';
var
  TrimmedName: string;  // Name param trimmed of leading trailing spaces
begin
  Result := False;
  TrimmedName := StrTrim(Name);
  if TrimmedName = '' then
    ErrorMsg := sErrNoName
  else if not IsValidIdent(TrimmedName) then
    ErrorMsg := Format(sErrBadName, [TrimmedName])
  else if CheckForUniqueness and
    (Database.Snippets.Find(TrimmedName) <> nil) then
    ErrorMsg := Format(sErrDupName, [TrimmedName])
  else
    Result := True;
end;

class function TSnippetValidator.ValidateName(const Name: string;
  const CheckForUniqueness: Boolean; out ErrorMsg: string;
  out ErrorSel: TSelection): Boolean;
  {Validates a snippet's name.
    @param Name [in] Snippet name to be checked.
    @param CheckForUniqueness [in] Flag indicating whether a check should be
      made to see if snippet name is already in user database.
    @param ErrorMsg [out] Message that describes error. Undefined if True
      returned.
    @param ErrorSel [out] Selection that can be used to highlight error.
    @return True if name is valid or False if not.
  }
begin
  Result := ValidateName(Name, CheckForUniqueness, ErrorMsg);
  if not Result then
    ErrorSel := TSelection.Create(0, Length(Name));
end;

class function TSnippetValidator.ValidateName(const Name: string;
  const CheckForUniqueness: Boolean): Boolean;
  {Validates a snippet's name.
    @param Name [in] Snippet name to be checked.
    @param CheckForUniqueness [in] Flag indicating whether a check should be
      made to see if snippet name is already in user database.
    @return True if name is valid or False if not.
  }
var
  DummyErrMsg: string;
begin
  Result := ValidateName(Name, CheckForUniqueness, DummyErrMsg);
end;

class function TSnippetValidator.ValidateNotes(const Notes: IActiveText;
  out ErrorMsg: string): Boolean;
  {Validates a snippet's notes.
    @param Notes [in] Notes information to be checked.
    @param ErrorMsg [out] Message that describes error. Undefined if True
      returned.
    @return True if notes are valid, False if not.
  }
var
  ErrorInfo: TActiveTextValidator.TErrorInfo; // info about error
begin
  Result :=  TActiveTextValidator.Validate(Notes, ErrorInfo);
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
  Result := not StrIsBlank(Source);
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

