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
  CS.Database.Types,
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
    class function ValidateNotes(Notes: IActiveText; out ErrorMsg: string):
      Boolean;
      {Validates a snippet's notes.
        @param Notes [in] Notes information to be checked.
        @param ErrorMsg [out] Message that describes error. Undefined if True
          returned.
        @return True if notes are valid, False if not.
      }
    class function ValidateDescription(Desc: IActiveText; out ErrorMsg: string;
      out ErrorSel: TSelection): Boolean;
      {Validates a description code from a snippet.
        @param Desc [in] Description to be checked.
        @param ErrorMsg [out] Message that describes error. Undefined if True
          returned.
        @param ErrorSel [out] Selection that can be used to highlight error.
        @return True if description is valid or False if not.
      }
  public
    class function ValidateDependsList(Snippet: ISnippet; out ErrorMsg: string):
      Boolean;
      {Recursively checks dependency list of a snippet for validity.
        @param Snippet [in] Snippet for which dependencies are to be checked.
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
    class function ValidateName(const Name: string;
      const CheckForUniqueness: Boolean): Boolean; overload;
      {Validates a snippet's name.
        @param Name [in] Snippet name to be checked.
        @param CheckForUniqueness [in] Flag indicating whether a check should
          be made to see if snippet name is already in database.
        @return True if name is valid or False if not.
      }
    { TODO: Rename following method and its overloads as ValidateSnippetID }
    { TODO: Rename Name parameter of following method and its overloads as
            IDStr }
    class function ValidateName(const Name: string;
      const CheckForUniqueness: Boolean; out ErrorMsg: string): Boolean;
      overload;
      {Validates a snippet's name.
        @param Name [in] Snippet name to be checked.
        @param CheckForUniqueness [in] Flag indicating whether a check should
          be made to see if snippet name is already in database.
        @param ErrorMsg [out] Message that describes error. Undefined if True
          returned.
        @return True if name is valid or False if not.
      }
    ///  <summary>Validates a snippet's title.</summary>
    ///  <param name="Title">string [in] Title to be validated.</param>
    ///  <param name="ErrorMsg">string [out] Message that describes error. -
    ///  Undefined if True returned.</param>
    ///  <returns>Boolean. True if title is valid or False if not.
    ///  </returns>
    class function ValidateTitle(const Title: string; out ErrorMsg: string):
      Boolean;
    class function Validate(Snippet: ISnippet; out ErrorMsg: string):
      Boolean;
      {Checks a snippet for validity.
        @param Snippet [in] Snippet to be checked.
        @param ErrorMsg [out] Message that describes error. Undefined if True
          returned.
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
  CS.ActiveText.Renderers.PlainText,
  CS.ActiveText.Validator,
  DB.UMain,
  DB.USnippetKind,
  UStrUtils;


{ TSnippetValidator }

class function TSnippetValidator.Validate(Snippet: ISnippet;
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
  Result := ValidateDescription(Snippet.Description, ErrorMsg, DummySel)
    and ValidateSourceCode(Snippet.SourceCode, ErrorMsg, DummySel)
    and ValidateDependsList(Snippet, ErrorMsg)
    and ValidateNotes(Snippet.Notes, ErrorMsg);
end;

class function TSnippetValidator.ValidateDependsList(Snippet: ISnippet;
  out ErrorMsg: string): Boolean;
  {Recursively checks dependency list of a snippet for validity.
    @param Snippet [in] Snippet for which dependencies are to be checked.
    @param ErrorMsg [out] Message that describes error. Undefined if True
      returned.
    @return True if dependency list is valid or False if not.
  }

  // ---------------------------------------------------------------------------
  function DependsListIsCircular(Snippet: ISnippet;
    DependsList: ISnippetIDList): Boolean;
    {Checks if dependency list is circular, i.e. a snippet is referenced in own
    chain of dependencies. Recursive function.
      @param Snippet [in] Snippet to be checked.
      @param DependsList [in] A dependency list.
      @return True if dependency list is circular, false if not.
    }
  var
    RequiredSnippet: TSnippetID;  // iterates through DependsList
  begin
    Result := False;
    for RequiredSnippet in DependsList do
    begin
      if RequiredSnippet = Snippet.ID then
        Result := True
      else
        Result := DependsListIsCircular(
          Snippet, Database.LookupSnippet(RequiredSnippet).RequiredSnippets
        );
      if Result then
        Exit;
    end;
  end;

  function DependsListHasKinds(DependsList: ISnippetIDList;
    const Kinds: TSnippetKinds): Boolean;
    {Recursively checks if a dependency list contains snippets of specified
    kinds.
      @param DependsList [in] A dependency list.
      @param Kinds [in] Set of snippet kinds to check for.
      @return True if one or more of specified kinds are found, false if not.
    }
  var
    RequiredSnippetID: TSnippetID;
    RequiredSnippet: ISnippet;
  begin
    Result := False;
    if Kinds = [] then
      Exit; // no kinds specified so depends list can't have these kinds!
    for RequiredSnippetID in DependsList do
    begin
      RequiredSnippet := Database.LookupSnippet(RequiredSnippetID);
      if RequiredSnippet.Kind in Kinds then
        Result := True
      else
        Result := DependsListHasKinds(RequiredSnippet.RequiredSnippets, Kinds);
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
  Result := not DependsListIsCircular(Snippet, Snippet.RequiredSnippets);
  if not Result then
  begin
    ErrorMsg := Format(
      sCircular, [
        TSnippetKindInfoList.Items[Snippet.Kind].DisplayName,
        Snippet.Title
      ]
    );
    Exit;
  end;
  // Now check kinds of snippets in dependency list for validity.
  // determine which snippet kinds can't appear in a dependency list
  DeniedDepends := cAllSnippetKinds - ValidDependsKinds(Snippet.Kind);
  // check dependency list for invalid kinds
  Result := not DependsListHasKinds(Snippet.RequiredSnippets, DeniedDepends);
  if not Result then
    ErrorMsg := Format(
      sInvalidKind,
      [
        TSnippetKindInfoList.Items[Snippet.Kind].DisplayName,
        Snippet.Title
      ]
    );
end;

class function TSnippetValidator.ValidateDescription(Desc: IActiveText;
  out ErrorMsg: string; out ErrorSel: TSelection): Boolean;
  {Validates a description code from a snippet.
    @param Desc [in] Description to be checked.
    @param ErrorMsg [out] Message that describes error. Undefined if True
      returned.
    @param ErrorSel [out] Selection that can be used to highlight error.
    @return True if description is valid or False if not.
  }
var
  ErrorInfo: TActiveTextValidator.TErrorInfo; // info about error
  DescText: string;
resourcestring
  // Error messages
  sErrNoDesc = 'A description must be provided';
  sErrDescHasClosingBrace = 'Description must not contain a ''}'' character';
  sErrorStub = 'Error in snippet''s description: %s';
const
  ClosingBrace = '}';
begin
  if not TActiveTextValidator.Validate(Desc, ErrorInfo) then
  begin
    ErrorMsg := Format(sErrorStub, [ErrorInfo.Description]);
    ErrorSel := TSelection.Create(0);
    Exit(False);
  end;
  DescText := TActiveTextPlainTextRenderer.Render(Desc, sLineBreak, []);
  if StrIsBlank(DescText) then
  begin
    ErrorMsg := sErrNoDesc;
    ErrorSel := TSelection.Create(0, Length(DescText));
    Exit(False);
  end;
  if StrContainsStr(ClosingBrace, DescText) then
  begin
    ErrorMsg := sErrDescHasClosingBrace;
    ErrorSel := TSelection.Create(
      StrPos(ClosingBrace, DescText) - 1, Length(ClosingBrace)
    );
    Exit(False);
  end;
  Result := True;
end;

class function TSnippetValidator.ValidateName(const Name: string;
  const CheckForUniqueness: Boolean; out ErrorMsg: string): Boolean;
  {Validates a snippet's name.
    @param Name [in] Snippet name to be checked.
    @param CheckForUniqueness [in] Flag indicating whether a check should be
      made to see if snippet name is already in database.
    @param ErrorMsg [out] Message that describes error. Undefined if True
      returned.
    @return True if name is valid or False if not.
  }
resourcestring
  // Error messages
  sErrNoName = 'A name must be provided';
  sErrDupName = '"%s" is already in the database. Please choose another name';
  sErrBadID = '"%s" is not a valid snippet identifier';
var
  TrimmedName: string;  // Name param trimmed of leading trailing spaces
begin
  Result := False;
  TrimmedName := StrTrim(Name);
  if TrimmedName = '' then
    ErrorMsg := sErrNoName
  else if not TSnippetID.IsValidIDString(TrimmedName) then
    ErrorMsg := Format(sErrBadID, [TrimmedName])
  else if CheckForUniqueness
    and Database.SnippetExists(TSnippetID.Create(TrimmedName)) then
    ErrorMsg := Format(sErrDupName, [TrimmedName])
  else
    Result := True;
end;

class function TSnippetValidator.ValidateName(const Name: string;
  const CheckForUniqueness: Boolean): Boolean;
  {Validates a snippet's name.
    @param Name [in] Snippet name to be checked.
    @param CheckForUniqueness [in] Flag indicating whether a check should be
      made to see if snippet name is already in database.
    @return True if name is valid or False if not.
  }
var
  DummyErrMsg: string;
begin
  Result := ValidateName(Name, CheckForUniqueness, DummyErrMsg);
end;

class function TSnippetValidator.ValidateNotes(Notes: IActiveText;
  out ErrorMsg: string): Boolean;
  {Validates a snippet's notes.
    @param Notes [in] Notes information to be checked.
    @param ErrorMsg [out] Message that describes error. Undefined if True
      returned.
    @return True if notes are valid, False if not.
  }
var
  ErrorInfo: TActiveTextValidator.TErrorInfo; // info about error
resourcestring
  sErrorStub = 'Error in snippet''s notes: %s';
begin
  Result := TActiveTextValidator.Validate(Notes, ErrorInfo);
  if not Result then
    ErrorMsg := Format(sErrorStub, [ErrorInfo.Description]);
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

class function TSnippetValidator.ValidateTitle(const Title: string;
  out ErrorMsg: string): Boolean;
resourcestring
  sErrNoName = 'A title must be provided';
begin
  Result := not StrIsBlank(Title);
  if not Result then
    ErrorMsg := sErrNoName;
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

