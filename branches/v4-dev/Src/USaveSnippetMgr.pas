{
 * USaveSnippetMgr.pas
 *
 * Defines a class that manages generation, previewing and saving of a code
 * snippet.
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
 * The Original Code is USaveSnippetMgr.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2005-2010 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
}


unit USaveSnippetMgr;


interface


uses
  // Project
  UBaseObjects, USourceFileInfo, USourceFileOutputMgr, USourceGen, UView;


type

  {
  TSaveSnippetMgr:
    Manages generation, previewing and saving of a suitable code snippet or
    category to disk. Generated file can be a Pascal include file, a plain text
    file, an HTML file or a RTF file. The last two files types can optionally be
    syntax highlighted.
  }
  TSaveSnippetMgr = class(TSaveSourceMgr)
  strict private
    fView: IView;                     // View to be output
    fDocTitle: string;                // Title of saved documents
  strict protected
    constructor InternalCreate(View: IView);
      {Class constructor. Sets up object for view.
        @param View [in] View to be output.
      }
    function GetFileTypeDesc(const FileType: TSourceFileType): string; override;
    function GetDefaultFileName: string; override;
    function GetDlgTitle: string; override;
    function GetDlgHelpKeyword: string; override;
    procedure GenerateSource(const CommentStyle: TCommentStyle;
      out RawSourceCode, DocTitle: string); override;
    procedure CheckFileName(const FileName: string; out NameOK: Boolean;
      out ErrorMessage: string); override;
  public
    destructor Destroy; override;
      {Class destructor. Tears down object.
      }
    class procedure Execute(View: IView);
      {Creates and outputs a compilable include file generated from a view item.
        @param View [in] View from which source code is generated. CanHandleView
          must return True for this view.
      }
    class function CanHandleView(View: IView): Boolean;
      {Checks whether a snippet include file can be created from a view.
        @param View [in] View to be checked.
        @return True if view contains code that can be output as a compilable
          snippet include file, False otherwise.
      }
  end;


implementation


uses
  // Delphi
  SysUtils,
  // Project
  UEncodings, USnippetSourceGen;


resourcestring
  // Dialog box title
  sSaveDlgTitle = 'Save %0:s Snippet';
  // Output document title for routines and categories
  sDocTitle = '"%0:s" %1:s';
  sCategory = 'category';
  sRoutine = 'routine';
  // File filter strings
  sHtmExtDesc = 'HTML file';
  sRtfExtDesc = 'Rich text file';
  sIncExtDesc = 'Pascal include file';
  sTxtExtDesc = 'Plain text file';


{ TSaveSnippetMgr }

class function TSaveSnippetMgr.CanHandleView(View: IView): Boolean;
  {Checks whether a snippet include file can be created from a view.
    @param View [in] View to be checked.
    @return True if view contains code that can be output as a compilable
      snippet include file, False otherwise.
  }
begin
  Result := TSnippetSourceGen.CanGenerate(View);
end;

procedure TSaveSnippetMgr.CheckFileName(const FileName: string;
  out NameOK: Boolean; out ErrorMessage: string);
begin
  NameOK := True;
end;

destructor TSaveSnippetMgr.Destroy;
  {Class destructor. Tears down object.
  }
begin
  inherited;
end;

class procedure TSaveSnippetMgr.Execute(View: IView);
  {Creates and outputs a compilable include file generated from a view item.
    @param View [in] View from which source code is generated. CanHandleView
      must return True for this view.
  }
begin
  with InternalCreate(View) do
    try
      DoExecute;
    finally
      Free;
    end;
end;

procedure TSaveSnippetMgr.GenerateSource(const CommentStyle: TCommentStyle;
  out RawSourceCode, DocTitle: string);
begin
  RawSourceCode := TSnippetSourceGen.Generate(fView, CommentStyle);
  DocTitle := fDocTitle;
end;

function TSaveSnippetMgr.GetDefaultFileName: string;
begin
  Result := fView.Description;
end;

function TSaveSnippetMgr.GetDlgHelpKeyword: string;
begin
  Result := 'SaveSnippetDlg';
end;

function TSaveSnippetMgr.GetDlgTitle: string;
begin
  Result := Format(sSaveDlgTitle, [fView.Description]);
end;

function TSaveSnippetMgr.GetFileTypeDesc(
  const FileType: TSourceFileType): string;
const
  Descriptions: array[TSourceFileType] of string = (
    sTxtExtDesc, sIncExtDesc, sHtmExtDesc, sRtfExtDesc
  );
begin
  Result := Descriptions[FileType];
end;

constructor TSaveSnippetMgr.InternalCreate(View: IView);
  {Class constructor. Sets up object for view.
    @param View [in] View to be output.
  }
begin
  // Record reference to view object: we do this here because overridden methods
  // calls made in inherited constructor.
  fView := View;

  inherited InternalCreate;

  // Record document title
  if Supports(View, ICategoryView) then
    fDocTitle := Format(sDocTitle, [View.Description, sCategory])
  else if Supports(View, ISnippetView) then
    fDocTitle := Format(sDocTitle, [View.Description, sRoutine]);
end;

end.

