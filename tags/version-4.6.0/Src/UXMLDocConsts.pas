{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2008-2013, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * Constants defined node names and attributes used in the various XML documents
 * used by CodeSnip.
}


unit UXMLDocConsts;


interface


uses
  // Project
  Compilers.UGlobals;


const
  // XML processing instruction node with version
  cXMLNode                    = 'xml';
  cXMLNodeText                = 'version="1.0"';

  // Document nodes and attribute names
  // user-database XML only
  cUserDataRootNode           = 'codesnip-data';
  cCategoriesNode             = 'categories';
  cCategoryNode               = 'category';
  cCategoryIdAttr             = 'id';
  cCatSnippetsNode            = 'cat-routines';
  cSourceCodeFileNode         = 'source-code';
  cXRefNode                   = 'xref';
  // export XML only
  cExportRootNode             = 'codesnip-export';
  cProgVersionNode            = 'prog-version';
  cUserInfoNode               = 'user-info';
  cUserNameNode               = 'name';
  cUserEmailNode              = 'email';
  cUserCommentsNode           = 'comments';
  cSourceCodeTextNode         = 'source-code-text';
  // common to user-database and export XML
  cRootVersionAttr            = 'version';
  cRootWatermarkAttr          = 'watermark';
  cPascalNameNode             = 'pascal-name';
  cDisplayNameNode            = 'display-name';
  cSnippetsNode               = 'routines';
  cSnippetNode                = 'routine';
  cSnippetNameAttr            = 'name';
  cCatIdNode                  = 'cat-id';
  cDescriptionNode            = 'description';
  cUnitsNode                  = 'units';
  cDependsNode                = 'depends';
  cCommentsNode               = 'comments';
  cCreditsNode                = 'credits';
  cCreditsUrlNode             = 'credits-url';
  cExtraNode                  = 'extra';
  cKindNode                   = 'kind';
  cHighlightSource            = 'highlight-source';
  cStandardFormatNode         = 'standard-format';
  cCompilerResultsNode        = 'compiler-results';
  cCompilerResultNode         = 'compiler-result';
  cCompilerResultIdAttr       = 'id';

  // ID values: common to user-database and export XML
  cCompilerIDs: array[TCompilerID] of string = (
    'd2', 'd3', 'd4', 'd5', 'd6', 'd7',
    'd2005', 'd2006', 'd2007', 'd2009', 'd2010',
    'dXE', 'dXE2', 'dXE3', 'dDX4',
    'fpc'
  );


implementation

end.

