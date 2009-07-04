{
 * USnippetMgr.pas
 *
 * Implements abstract base class for objects that create and output code
 * snippets.
 *
 * v0.1 of 06 Jan 2006  - Original version.
 * v1.0 of 24 May 2006  - Improved and corrected comments.
 * v1.1 of 04 Feb 2007  - Replaced redundant TDetailView class references with
 *                        TViewItem.
 * v1.2 of 04 Oct 2008  - Made constructor protected and made Execute method
 *                        static.
 *                      - Made protected and private sections strict.
 *                      - Now use ClassName method in all assert statements.
 * v1.3 of 16 May 2009  - Adapted to use snippet kind to determine whether a
 *                        snippet can be generated.
 *                      - Flagged TSnippetMgr as abstract.
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
 * The Original Code is USnippetMgr.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2006-2009 Peter
 * Johnson. All Rights Reserved.
 *
 * ***** END LICENSE BLOCK *****
}


unit USnippetMgr;


interface


uses
  // Project
  UBaseObjects, USnippetSourceGen, USourceGen, UView;


type

  {
  TSnippetMgr:
    Abstract base class for objects that create and output code snippets.
  }
  TSnippetMgr = class abstract(TNoPublicConstructObject)
  strict private
    fSourceGen: TSnippetSourceGen;
      {Snippet source code generator object}
  strict protected
    constructor InternalCreate(const View: TViewItem); virtual;
      {Class constructor. Creates object to get source code from view object.
        @param View [in] View object containing source code. View must be of a
          type that contains source code.
      }
    procedure DoExecute; virtual; abstract;
      {Abstract method to create and output source code.
      }
    function SourceCode(const CommentStyle: TCommentStyle): string;
      {Generates source code using generator object.
        @param CommentStyle [in] Style of commenting to use in generated source
          code.
        @return Generated source code.
      }
  public
    destructor Destroy; override;
      {Class destructor. Tears down object.
      }
    class procedure Execute(const View: TViewItem); virtual;
      {Creates and outputs source code for a view item.
        @param View [in] View object containing source code to be outputted.
          View must be of a type that contains source code.
      }
    class function CanHandleView(const View: TViewItem): Boolean;
      {Checks whether a snippet can be created from a view, i.e. whether the
      view contains source code.
        @param View [in] View to be checked.
        @return True if view can be saved as a snippet, false otherwise.
      }
  end;


implementation


uses
  // Delphi
  SysUtils,
  // Project
  USnippets;


{ TSnippetMgr }

class function TSnippetMgr.CanHandleView(const View: TViewItem): Boolean;
  {Checks whether a snippet can be created from a view, i.e. whether the view
  contains source code.
    @param View [in] View to be checked.
    @return True if view can be saved as a snippet, false otherwise.
  }
var
  Snippet: TRoutine;  // references snippets in a category
begin
  if View.Kind = vkCategory then
  begin
    // View is category: can handle view if category contains at least one valid
    // routine
    Result := False;
    for Snippet in View.Category.Routines do
      if Snippet.Kind = skRoutine then
      begin
        Result := True;
        Exit;
      end;
  end
  else if View.Kind = vkRoutine then
    // View is snippet: can handle view if snippet is a routine
    Result := View.Routine.Kind = skRoutine
  else
    // Can't handle view
    Result := False;
end;

destructor TSnippetMgr.Destroy;
  {Class destructor. Tears down object.
  }
begin
  FreeAndNil(fSourceGen);
  inherited;
end;

class procedure TSnippetMgr.Execute(const View: TViewItem);
  {Creates and outputs source code for a view item.
    @param View [in] View object containing source code to be outputted. View
      must be of a type that contains source code.
  }
begin
  with InternalCreate(View) do
    try
      DoExecute;
    finally
      Free;
    end;
end;

constructor TSnippetMgr.InternalCreate(const View: TViewItem);
  {Class constructor. Creates object to get source code from view object.
    @param View [in] View object containing source code. View must be of a type
      that contains source code.
  }
begin
  Assert(Assigned(View), ClassName + '.InternalCreate: View is nil');
  Assert(CanHandleView(View),
    ClassName + '.InternalCreate: View is not saveable');
  inherited InternalCreate;
  fSourceGen := TSnippetSourceGen.Create(View);
end;

function TSnippetMgr.SourceCode(const CommentStyle: TCommentStyle): string;
  {Generates source code using generator object.
    @param CommentStyle [in] Style of commenting to use in generated source
      code.
    @return Generated source code.
  }
begin
  Result := fSourceGen.Generate(CommentStyle);
end;

end.

