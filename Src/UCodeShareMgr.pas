{
 * UCodeShareMgr.pas
 *
 * Implements a static class that manages sharing of user defined snippets.
 * Provides support for exporting routines, importing routines and submitting
 * routines to the online database.
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
 * The Original Code is UCodeShareMgr.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2008-2009 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
}


unit UCodeShareMgr;


interface


uses
  // Project
  UBaseObjects, USnippets, UView;


type

  {
  TCodeShareMgr:
    Sealed static class that manages sharing of user defined snippets. Provides
    support for exporting routines, importing routines and submitting routines
    to the online database.
  }
  TCodeShareMgr = class sealed(TNoConstructObject)
  strict private
    class function GetRoutineFromView(const ViewItem: TViewItem): TRoutine;
      {Gets reference to any user defined routine represented by a view item.
        @param ViewItem [in] View item for which routine is required.
        @return Reference to required routine or nil if view item does not
          represent a routine or if routine is not user defined.
      }
  public
    class procedure Submit(const ViewItem: TViewItem);
      {Submits code for consideration to be included in main database.
        @param ViewItem [in] View item that may contain a user defined routine.
          If so the routine is included in code for submission by default.
      }
    class function CanShare: Boolean;
      {Checks if there are any user defined routines that can be shared (i.e.
      exported or submitted.
        @return True if user defined routines exist in database.
      }
    class procedure ExportCode(const ViewItem: TViewItem);
      {Exports user defined code to an export file.
        @param ViewItem [in] View item that may contain a user defined routine.
          If so the routine is included in the export file by default.
      }
    class procedure ImportCode;
  end;


implementation


uses
  // Project
  FmCodeExportDlg, FmCodeSubmitDlg, UCodeImportMgr;


{ TCodeShareMgr }

class function TCodeShareMgr.CanShare: Boolean;
  {Checks if there are any user defined routines that can be shared (i.e.
  exported or submitted.
    @return True if user defined routines exist in database.
  }
begin
  Result := Snippets.Routines.Count(True) > 0;
end;

class procedure TCodeShareMgr.ExportCode(const ViewItem: TViewItem);
  {Exports user defined code to an export file.
    @param ViewItem [in] View item that may contain a user defined routine. If
      so the routine is included in the export file by default.
  }
begin
  TCodeExportDlg.Execute(nil, GetRoutineFromView(ViewItem));
end;

class function TCodeShareMgr.GetRoutineFromView(
  const ViewItem: TViewItem): TRoutine;
  {Gets reference to any user defined routine represented by a view item.
    @param ViewItem [in] View item for which routine is required.
    @return Reference to required routine or nil if view item does not represent
      a routine or if routine is not user defined.
  }
begin
  if (ViewItem.Kind = vkRoutine) and (ViewItem.Routine.UserDefined) then
    Result := ViewItem.Routine
  else
    Result := nil;
end;

class procedure TCodeShareMgr.ImportCode;
  {Imports user defined code from an export file.
  }
begin
  TCodeImportMgr.Execute;
end;

class procedure TCodeShareMgr.Submit(const ViewItem: TViewItem);
  {Submits code for consideration to be included in main database.
    @param ViewItem [in] View item that may contain a user defined routine. If
      so the routine is included in code for submission by default.
  }
begin
  TCodeSubmitDlg.Execute(nil, GetRoutineFromView(ViewItem));
end;

end.

