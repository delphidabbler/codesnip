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
 * Portions created by the Initial Developer are Copyright (C) 2009 Peter
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
  UBaseObjects, USnippets;


type
  {
  TSnippetValidator:
    Static class that checks a snippet for validity.
  }
  TSnippetValidator = class(TNoConstructObject)
  public
    class function HasValidDependsList(const Snippet: TRoutine;
      out ErrorMsg: string): Boolean; overload;
      {Recursively checks dependency list of a snippet for validity.
        @param Snippet [in] Snippet for which dependencies are to be checked.
        @param ErrorMsg [out] Message that describes error. Undefined if True
          returned.
        @return True if dependency list is valid or False if not.
      }
    class function HasValidDependsList(const SnippetName: string;
      const Data: TRoutineEditData; out ErrorMsg: string): Boolean; overload;
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
      out ErrorMsg: string): Boolean;
      {Validates a source code from a snippet.
        @param Source [in] Source code to be checked.
        @param ErrorMsg [out] Message that describes error. Undefined if True
          returned.
        @return True if source code is valid or False if not.
      }
    class function ValidateDescription(const Desc: string;
      out ErrorMsg: string): Boolean;
      {Validates a description code from a snippet.
        @param Desc [in] Description to be checked.
        @param ErrorMsg [out] Message that describes error. Undefined if True
          returned.
        @return True if description is valid or False if not.
      }
    class function ValidateName(const Name: string;
      const CheckForUniqueness: Boolean; out ErrorMsg: string): Boolean;
      {Validates a snippet's name.
        @param Name [in] Snippet name to be checked.
        @param CheckForUniqueness [in] Flag indicating whether a check should
          be made to see if snippet name is already in user database.
        @param ErrorMsg [out] Message that describes error. Undefined if True
          returned.
        @return True if name is valid or False if not.
      }
    class function Validate(const Snippet: TRoutine;
      out ErrorMsg: string): Boolean;
      {Checks a snippet for validity.
        @param Snippet [in] Snippet to be checked.
        @param ErrorMsg [out] Message that describes error. Undefined if True
          returned.
        @return True if snippet valid or False if not.
      }
  end;


implementation


uses
  // Delphi
  SysUtils, StrUtils,
  // Project
  USnippetKindInfo;


{ TSnippetValidator }

class function TSnippetValidator.HasValidDependsList(const Snippet: TRoutine;
  out ErrorMsg: string): Boolean;
  {Recursively checks dependency list of a snippet for validity.
    @param Snippet [in] Snippet for which dependencies are to be checked.
    @param ErrorMsg [out] Message that describes error. Undefined if True
      returned.
    @return True if dependency list is valid or False if not.
  }
type
  {
  TSnippetKinds:
    Set of TSnippetKind values.
  }
  TSnippetKinds = set of TSnippetKind;

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
begin
  Result := True;
  case Snippet.Kind of
    skFreeform:
    begin
      // Freeform snippet can have any snippet in list but must not depend on
      // itself
      Result := not DependsListIsCircular(Snippet, Snippet.Depends);
      if not Result then
        ErrorMsg := Format(
          sCircular, [
            TSnippetKindInfoList.Instance[Snippet.Kind].Description,
            Snippet.Name
          ]
        );
    end;
    skRoutine:
    begin
      // Routine must not depend on itself and must not depend of freeform code
      // ** MUST do test for circularity first
      Result := not DependsListIsCircular(Snippet, Snippet.Depends);
      if not Result then
        ErrorMsg := Format(
          sCircular, [
            TSnippetKindInfoList.Instance[Snippet.Kind].Description,
            Snippet.Name
          ]
        )
      else
      begin
        Result := not DependsListHasKinds(Snippet.Depends, [skFreeform]);
        if not Result then
          ErrorMsg := Format(
            sInvalidKind,
            [
              TSnippetKindInfoList.Instance[Snippet.Kind].Description,
              Snippet.Name
            ]
          );
      end;
    end;
    skConstant, skTypeDef:
    begin
      // Constants and TypeDefs may only depend on other Constants and TypeDefs
      // and cannot depend on themselves
      // ** MUST do test for circularity first
      Result := not DependsListIsCircular(Snippet, Snippet.Depends);
      if not Result then
        ErrorMsg := Format(
          sCircular, [
            TSnippetKindInfoList.Instance[Snippet.Kind].Description,
            Snippet.Name
          ]
        )
      else
      begin
        Result := not DependsListHasKinds(
          Snippet.Depends, [skFreeform, skRoutine]
        );
        if not Result then
          ErrorMsg := Format(
            sInvalidKInd,
            [
              TSnippetKindInfoList.Instance[Snippet.Kind].Description,
              Snippet.Name
            ]
          )
      end
    end;
  end;
end;

class function TSnippetValidator.HasValidDependsList(const SnippetName: string;
  const Data: TRoutineEditData; out ErrorMsg: string): Boolean;
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
    Result := HasValidDependsList(TempSnippet, ErrorMsg);
  finally
    FreeAndNil(TempSnippet);
  end;
end;

class function TSnippetValidator.Validate(const Snippet: TRoutine;
  out ErrorMsg: string): Boolean;
  {Checks a snippet for validity.
    @param Snippet [in] Snippet to be checked.
    @param ErrorMsg [out] Message that describes error. Undefined if True
      returned.
    @return True if snippet valid or False if not.
  }
begin
  Result := ValidateName(Snippet.Name, False, ErrorMsg)
    and ValidateDescription(Snippet.Description, ErrorMsg)
    and ValidateSourceCode(Snippet.SourceCode, ErrorMsg)
    and HasValidDependsList(Snippet, ErrorMsg);
end;

class function TSnippetValidator.ValidateDescription(const Desc: string;
  out ErrorMsg: string): Boolean;
  {Validates a description code from a snippet.
    @param Desc [in] Description to be checked.
    @param ErrorMsg [out] Message that describes error. Undefined if True
      returned.
    @return True if description is valid or False if not.
  }
resourcestring
  // Error messages
  sErrNoDesc = 'A description must be provided';
  sErrDescHasClosingBrace = 'Description must not contain a ''}'' character';
begin
  Result := False;
  if Trim(Desc) = '' then
    ErrorMsg := sErrNoDesc
  else if AnsiContainsText(Desc, '}') then
    ErrorMsg := sErrDescHasClosingBrace
  else
    Result := True;
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
  sErrDupName = '%s is already in the database. Please choose another name';
  sErrBadName = '%s is not a valid Pascal identifier';
begin
  Result := False;
  if Trim(Name) = '' then
    ErrorMsg := sErrNoName
  else if not IsValidIdent(Name) then
    ErrorMsg := Format(sErrBadName, [Name])
  else if CheckForUniqueness and
    (Snippets.Routines.Find(Name, True) <> nil) then
    ErrorMsg := Format(sErrDupName, [Name])
  else
    Result := True;
end;

class function TSnippetValidator.ValidateSourceCode(const Source: string;
  out ErrorMsg: string): Boolean;
  {Validates a source code from a snippet.
    @param Source [in] Source code to be checked.
    @param ErrorMsg [out] Message that describes error. Undefined if True
      returned.
    @return True if source code is valid or False if not.
  }
resourcestring
  // Error message
  sErrNoSource = 'Some source code must be provided';
begin
  // Source code must be provided
  Result := Trim(Source) <> '';
  if not Result then
    ErrorMsg := sErrNoSource;
end;

end.

