{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2005-2021, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Defines a class that manages generation, previewing and saving of a code
 * snippet.
}


unit USaveSnippetMgr;


interface


uses
  // Project
  USourceFileInfo, USaveSourceMgr, USourceGen, UView;


type
  ///  <summary>
  ///  Manages generation, previewing and saving of a suitable code snippet or
  ///  category to disk.
  ///  </summary>
  ///  <remarks>
  ///  Generated file can be a Pascal include file, a plain text file, an HTML
  ///  file or a RTF file. The last two file types can optionally be syntax
  ///  highlighted.
  ///  </remarks>
  TSaveSnippetMgr = class(TSaveSourceMgr)
  strict private
    ///  <summary>View containing item for which source code to be output.
    ///  </summary>
    fView: IView;
  strict protected
    ///  <summary>Object constructor. Sets up object to save source code
    ///  encapsulated by a view.</summary>
    constructor InternalCreate(View: IView);
    ///  <summary>Gets description of given source code file type.</summary>
    function GetFileTypeDesc(const FileType: TSourceFileType): string; override;
    ///  <summary>Gets default file name to display in dialog box.</summary>
    function GetDefaultFileName: string; override;
    ///  <summary>Gets dialog box title.</summary>
    function GetDlgTitle: string; override;
    ///  <summary>Get dialog box's help keyword.</summary>
    function GetDlgHelpKeyword: string; override;
    ///  <summary>Gets title to be used for source document.</summary>
    function GetDocTitle: string; override;
    ///  <summary>Generates raw, un-highlighted, source code.</summary>
    ///  <param name="CommentStyle">TCommentStyle [in] Style of commenting to be
    ///  used in source code.</param>
    ///  <param name="TruncateComments">Boolean [in] Indicates whether multi
    ///  paragraph comments are to be truncated to first paragraph.</param>
    ///  <returns>String containing generated source code.</returns>
    function GenerateSource(const CommentStyle: TCommentStyle;
      const TruncateComments: Boolean): string;
      override;
    ///  <summary>Checks if a file name is valid for the kind of file being
    ///  saved.</summary>
    ///  <param name="FileName">string [in] Name of file to check.</param>
    ///  <param name="NameOK">Boolean [out] Set True if file name OK, False if
    ///  not.</param>
    ///  <param name="ErrorMessage">string [out] Error message describing
    ///  problem with invalid file name. Undefined if NameOK is True.</param>
    procedure CheckFileName(const FileName: string; out NameOK: Boolean;
      out ErrorMessage: string); override;
  public
    ///  <summary>Creates and outputs a compilable include file generated from a
    ///  view item with name and format speficied by user.</summary>
    ///  <param name="View">IView [in] View from which source code is generated.
    ///  CanHandleView must return True for this view.</param>
    class procedure Execute(View: IView);
    ///  <summary>Checks whether a snippet include file can be created from a
    ///  given view.</summary>
    class function CanHandleView(View: IView): Boolean;
  end;


implementation


uses
  // Delphi
  SysUtils,
  // Project
  USnippetSourceGen;


resourcestring
  // Dialog box title
  sSaveSnippetDlgTitle = 'Save %0:s Snippet';
  sSaveCategoryDlgTitle = 'Save %0:s Category';
  // Output document title for snippets and categories
  sDocTitle = '"%0:s" %1:s';
  sCategory = 'category';
  sSnippet = 'routine';
  // File filter strings
  sHtmExtDesc = 'HTML file';
  sRtfExtDesc = 'Rich text file';
  sIncExtDesc = 'Pascal include file';
  sTxtExtDesc = 'Plain text file';


{ TSaveSnippetMgr }

class function TSaveSnippetMgr.CanHandleView(View: IView): Boolean;
begin
  Result := TSnippetSourceGen.CanGenerate(View);
end;

procedure TSaveSnippetMgr.CheckFileName(const FileName: string;
  out NameOK: Boolean; out ErrorMessage: string);
begin
  NameOK := True;
end;

class procedure TSaveSnippetMgr.Execute(View: IView);
begin
  with InternalCreate(View) do
    try
      DoExecute;
    finally
      Free;
    end;
end;

function TSaveSnippetMgr.GenerateSource(const CommentStyle: TCommentStyle;
  const TruncateComments: Boolean): string;
begin
  Result := TSnippetSourceGen.Generate(fView, CommentStyle, TruncateComments);
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
  if Supports(fView, ICategoryView) then
    Result := Format(sSaveCategoryDlgTitle, [fView.Description])
  else if Supports(fView, ISnippetView) then
    Result := Format(sSaveSnippetDlgTitle, [fView.Description])
  else
    Result := '';
end;

function TSaveSnippetMgr.GetDocTitle: string;
begin
  if Supports(fView, ICategoryView) then
    Result := Format(sDocTitle, [fView.Description, sCategory])
  else if Supports(fView, ISnippetView) then
    Result := Format(sDocTitle, [fView.Description, sSnippet])
  else
    Result := '';
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
begin
  // Record reference to view object: we do this here because overridden methods
  // calls made in inherited constructor.
  fView := View;
  inherited InternalCreate;
end;

end.

