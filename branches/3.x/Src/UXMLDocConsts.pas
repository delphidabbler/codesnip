{
 * UXMLDocConsts.pas
 *
 * Constants defined node names and attributes used in the various XML documents
 * used by CodeSnip.
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
 * The Original Code is UXMLDocConsts.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2008-2013 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
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
  cCatRoutinesNode            = 'cat-routines';
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
  cRoutinesNode               = 'routines';
  cRoutineNode                = 'routine';
  cRoutineNameAttr            = 'name';
  cCatIdNode                  = 'cat-id';
  cDescriptionNode            = 'description';
  cUnitsNode                  = 'units';
  cDependsNode                = 'depends';
  cCommentsNode               = 'comments';           // v1 docs only
  cCreditsNode                = 'credits';            // v1 docs only
  cCreditsUrlNode             = 'credits-url';        // v1 docs only
  cExtraNode                  = 'extra';              // v2 docs only
  cKindNode                   = 'kind';               // v3 docs only
  cStandardFormatNode         = 'standard-format';
  cCompilerResultsNode        = 'compiler-results';
  cCompilerResultNode         = 'compiler-result';
  cCompilerResultIdAttr       = 'id';

  // ID values: common to user-database and export XML
  cCompilerIDs: array[TCompilerID] of string = (
    'd2', 'd3', 'd4', 'd5', 'd6', 'd7',
    'd2005', 'd2006', 'd2007', 'd2009', 'd2010',
    'dXE', 'dXE2', 'dXE3', 'dXE4',
    'fpc'
  );


implementation

end.

