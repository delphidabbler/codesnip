{
 * FrMemoPreview.pas
 *
 * Abstract base class for frames used to display previews of documents using
 * controls that descend from TCustomMemo.
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
 * The Original Code is FrMemoPreview.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2007-2009 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
}


unit FrMemoPreview;


interface


uses
  // Delphi
  Forms, Classes, Controls, ExtCtrls, StdCtrls;


type

  {
  TMemoPreviewFrame:
    Abstract base class for frames used to display previews of documents using
    controls that descend from TCustomMemo.
  }
  TMemoPreviewFrame = class(TFrame)
    pnlView: TPanel;
  strict private
    procedure SetMargin;
      {Sets fixed size margin around control.
      }
  strict protected
    function GetMemoCtrl: TCustomMemo; virtual; abstract;
      {Gets reference to frame's custom memo control.
        @return Required control reference.
      }
    function GetTitle(const DocContent: string): string; virtual; abstract;
      {Extracts a document title from a document if possible.
        @param DocContent [in] Document content.
        @return Required tile or '' if no title present or title not supported
          by document.
      }
    procedure LoadContent(const DocContent: string); virtual; abstract;
      {Loads content into frame's custom memo control.
        @param DocContent [in] Content to be displayed in control. Must have a
          format that is displayed by the control.
      }
  protected // do not make strict
    { IPreview methods: partial implementation }
    procedure Display(const DocContent: string; out Title: string);
      {Displays document in preview dialog box.
        @param DocContent [in] Content of document to be displayed.
        @param Title [out] Title of document, if any.
      }
    { IClipboardMgr methods }
    function CanCopy: Boolean;
      {Checks if there is any selected text in control that can be copied.
      Implements IClipboardMgr.CanCopy for subclasses.
        @return True if there is selected text, false otherwise.
      }
    procedure CopyToClipboard;
      {Copies control's current selection to clipboard. Implements
      IClipboardMgr.CopyToClipboard for subclasses.
      }
    { ISelectionMgr methods }
    function CanSelectAll: Boolean;
      {Checks if control permits all text to be selected. Implements
      ISelectionMgr.CanSelectAll for subclasses.
        @return True if control contains some text, false otherwise.
      }
    procedure SelectAll;
      {Selects all text in control. Implements ISelectionMgr.SelectAll for
      subclasses.
      }
  end;


implementation


uses
  // Project
  IntfPreview, UMemoHelper;


{$R *.dfm}


{ TMemoPreviewFrame }

function TMemoPreviewFrame.CanCopy: Boolean;
  {Checks if there is any selected text in control that can be copied.
  Implements IClipboardMgr.CanCopy for subclasses.
    @return True if there is selected text, false otherwise.
  }
begin
  Result := GetMemoCtrl.SelLength > 0;
end;

function TMemoPreviewFrame.CanSelectAll: Boolean;
  {Checks if control permits all text to be selected. Implements
  ISelectionMgr.CanSelectAll for subclasses.
    @return True if control contains some text, false otherwise.
  }
begin
  Result := Length(GetMemoCtrl.Text) > 0;
end;

procedure TMemoPreviewFrame.CopyToClipboard;
  {Copies control's current selection to clipboard. Implements
  IClipboardMgr.CopyToClipboard for subclasses.
  }
begin
  GetMemoCtrl.CopyToClipboard;
end;

procedure TMemoPreviewFrame.Display(const DocContent: string;
  out Title: string);
  {Displays document in preview dialog box.
    @param DocContent [in] Content of document to be displayed.
    @param Title [out] Title of document, if any.
  }
begin
  // Set margin of preview control
  SetMargin;
  // Load the document
  GetMemoCtrl.Lines.BeginUpdate;
  try
    LoadContent(DocContent);
    Title := GetTitle(DocContent);
  finally
    GetMemoCtrl.Lines.EndUpdate;
  end;
end;

procedure TMemoPreviewFrame.SelectAll;
  {Selects all text in control. Implements ISelectionMgr.SelectAll for
  subclasses.
  }
begin
  GetMemoCtrl.SelectAll;
end;

procedure TMemoPreviewFrame.SetMargin;
  {Sets fixed size margin around control.
  }
begin
  with TMemoHelper.Create(GetMemoCtrl) do
    try
      SetMargin(cPreviewMargin);
    finally
      Free;
    end;
end;

end.

