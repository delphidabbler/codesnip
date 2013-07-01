{
 * USnippetValidator.pas
 *
 * Implements a static class that checks a snippet for validity.
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
 * The Original Code is USnippetValidator.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2009-2010 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
}


unit USnippetValidator;


interface


uses
  // Project
  UActiveText, UBaseObjects, USnippets, UStructs;


type
  {
  TSnippetValidator:
    Static class that checks a snippet for validity.
  }
  TSnippetValidator = class(TNoConstructObject)
  strict private
    const
      cAllSnippetKinds: TSnippetKinds =   // Set of all possible snippet kinds
        [skFreeform, skRoutine, skConstant, skTypeDef];
  public
    class function ValidateDependsList(const Snippet: TRoutine;
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
      const CheckForUniqueness: Boolean; out ErrorMsg: string;
      out ErrorSel: TSelection): Boolean;
      {Validates a snippet's name.
        @param Name [in] Snippet name to be checked.
        @param CheckForUniqueness [in] Flag indicating whether a check should
          be made to see if snippet name is already in user database.
        @param ErrorMsg [out] Message that describes error. Undefined if True
          returned.
        @param ErrorSel [out] Selection that can be used to highlight error.
        @return True if name is valid or False if not.
      }
    class function ValidateExtra(const Extra: IActiveText;
      out ErrorMsg: string): Boolean;
      {Validates a extra information from a snippet.
        @param Extra [in] Extra information to be checked.
        @param ErrorMsg [out] Message that describes error. Undefined if True
          returned.
        @return True if extra information is valid, False if not.
      }
    class function Validate(const Snippet: TRoutine; out ErrorMsg: string;
      out ErrorSel: TSelection): Boolean;
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
        @return Set of valid kinds for snippets in dependenc list.
      }
  end;


implementation


uses
  // Delphi
  SysUtils, StrUtils,
  // Project
  USnippetKindInfo, UURIEncode;


{ TSnippetValidator }

class function TSnippetValidator.Validate(const Snippet: TRoutine;
  out ErrorMsg: string; out ErrorSel: TSelection): Boolean;
  {Checks a snippet for validity.
    @param Snippet [in] Snippet to be checked.
    @param ErrorMsg [out] Message that describes error. Undefined if True
      returned.
    @param ErrorSel [out] Selection that can be used to highlight error.
    @return True if snippet valid or False if not.
  }
begin
  Result := ValidateName(Snippet.Name, False, ErrorMsg, ErrorSel)
    and ValidateDescription(Snippet.Description, ErrorMsg, ErrorSel)
    and ValidateSourceCode(Snippet.SourceCode, ErrorMsg, ErrorSel)
    and ValidateDependsList(Snippet, ErrorMsg)
    and ValidateExtra(Snippet.Extra, ErrorMsg);
end;

class function TSnippetValidator.ValidateDependsList(const Snippet: TRoutine;
  out ErrorMsg: string): Boolean;
  {Recursively checks dependency list of a snippet for validity.
    @param Snippet [in] Snippet for which dependencies are to be checked.
    @param ErrorMsg [out] Message that describes error. Undefined if True
      returned.
    @return True if dependency list is valid or False if not.
  }

  // ---------------------------------------------------------------------------
  function DependsListIsCircular(const Snippet: TRoutine;
    const DependsList: TRoutineList): Boolean;
    {Checks if dependency list is circular, i.e. a snippet is referenced in own
    chain of dependencies. Recursive function.
      @param Snippet [in] Snippet to be checked.
      @param DependsList [in] A dependency list.
      @return True if dependency list is circular, false if not.
    }
  var
    RequiredSnippet: TRoutine;  // iterates through DependsList
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

  function DependsListHasKinds(const DependsList: TRoutineList;
    const Kinds: TSnippetKinds): Boolean;
    {Recursively checks if a dependency list contains snippets of specified
    kinds.
      @param DependsList [in] A dependency list.
      @param Kinds [in] Set of snippet kinds to check for.
      @return True if one or more of specified kinds are found, false if not.
    }
  var
    RequiredSnippet: TRoutine;  // iterates through depends list
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
  sInvalidKind = 'Invalid snippet kind in depends list for %0:s "%1:s".';
  sCircular = '%0:s "%1:s" cannot depend on itself';
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
        TSnippetKindInfoList.Instance[Snippet.Kind].Description,
        Snippet.Name
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
        TSnippetKindInfoList.Instance[Snippet.Kind].Description,
        Snippet.Name
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
  TempSnippet: TRoutine;  // temporary snippet that is checked for dependencies
begin
  TempSnippet := (Snippets as ISnippetsEdit).CreateTempRoutine(
    SnippetName, Data
  );
  try
    Result := ValidateDependsList(TempSnippet, ErrorMsg);
  finally
    FreeAndNil(TempSnippet);
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
  if Trim(Desc) = '' then
  begin
    ErrorMsg := sErrNoDesc;
    ErrorSel := TSelection.Create(0, Length(Desc));
  end
  else if AnsiContainsText(Desc, ClosingBrace) then
  begin
    ErrorMsg := sErrDescHasClosingBrace;
    ErrorSel := TSelection.Create(
      AnsiPos(ClosingBrace, Desc) - 1, Length(ClosingBrace)
    );
  end
  else
    Result := True;
end;

class function TSnippetValidator.ValidateExtra(const Extra: IActiveText;
  out ErrorMsg: string): Boolean;
  {Validates a extra information from a snippet.
    @param Extra [in] Extra information to be checked.
    @param ErrorMsg [out] Message that describes error. Undefined if True
      returned.
    @return True if extra information is valid, False if not.
  }

  // ---------------------------------------------------------------------------
  function ValidateURL(URL: string; out ErrorMsg: string): Boolean;
    {Validates a-link href URLs.
      @param URL [in] URL to validate.
    }
  const
    cHTTPProtocol = 'http://';  // http protocol prefix
    cFileProtocol = 'file://';  // file protocal prefix
  resourcestring
    // validation error messages
    sLinkErr = 'Hyperlink URL "%s" in extra information must use either the '
      + '"http://" or "file://" protocols';
    sURLLengthErr = 'Hyperlink URL "%s" in extra information is badly formed';
  begin
    Result := True;
    URL := URIDecode(URL);
    if AnsiStartsText(cHTTPProtocol, URL) then
    begin
      // http protocol: check length
      if Length(URL) < Length(cHTTPProtocol) + 6 then
      begin
        Result := False;
        ErrorMsg := Format(sURLLengthErr, [URL]);
        Exit;
      end;
    end
    else if AnsiStartsText(cFileProtocol, URL) then
    begin
      // file protocol: check length
      if Length(URL) < Length(cFileProtocol) + 4 then
      begin
        Result := False;
        ErrorMsg := Format(sURLLengthErr, [URL]);
        Exit;
      end;
    end
    else
    begin
      // Error neither file nor http protocols
      Result := False;
      ErrorMsg := Format(sLinkErr, [URL]);
      Exit;
    end;
  end;
  // ---------------------------------------------------------------------------

var
  Elem: IActiveTextElem;              // each element in active text
  ActionElem: IActiveTextActionElem;  // references action element
begin
  // Scan all active text looking of hyperlinks: check that URL has a
  // supported protocol and some url text after it
  Result := True;
  for Elem in Extra do
  begin
    if Supports(Elem, IActiveTextActionElem, ActionElem)
      and (ActionElem.Kind = ekLink) then
      if not ValidateURL(ActionElem.Param, ErrorMsg) then
      begin
        Result := False;
        Exit;
      end;
  end;
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
resourcestring
  // Error messages
  sErrNoName = 'A name must be provided';
  sErrDupName = '"%s" is already in the database. Please choose another name';
  sErrBadName = 'Name must begin with ''A''..''Z'', ''a''..''z'' or ''_''';
  sErrBadIdent = '"%s" is not a valid Pascal identifier';
var
  TrimmedName: string;  // Name param trimmed of leading trailing spaces
begin
  Result := False;
  TrimmedName := Trim(Name);
  if TrimmedName = '' then
    ErrorMsg := sErrNoName
  else if not CharInSet(TrimmedName[1], ['A'..'Z', 'a'..'z', '_']) then
    ErrorMsg := sErrBadName
  else if not IsValidIdent(TrimmedName) then
    ErrorMsg := Format(sErrBadIdent, [TrimmedName])
  else if CheckForUniqueness and
    (Snippets.Routines.Find(TrimmedName, True) <> nil) then
    ErrorMsg := Format(sErrDupName, [TrimmedName])
  else
    Result := True;
  if not Result then
    ErrorSel := TSelection.Create(0, Length(Name));
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
  Result := Trim(Source) <> '';
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
    skFreeform: Result := cAllSnippetKinds;
    skRoutine: Result := cAllSnippetKinds - [skFreeform];
    skConstant, skTypeDef: Result := [skConstant, skTypeDef];
  end;
end;

end.

