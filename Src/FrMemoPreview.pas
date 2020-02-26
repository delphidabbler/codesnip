{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2007-2020, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Implements an abstract base class for frames used to display previews of
 * documents using controls that descend from TCustomMemo.
}


unit FrMemoPreview;


interface


uses
  // Delphi
  Forms, Classes, Controls, ExtCtrls, StdCtrls,
  // Project
  UEncodings;


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
    procedure LoadContent(const DocContent: TEncodedData); virtual; abstract;
      {Loads content into frame's custom memo control.
        @param DocContent [in] Content to be displayed in control. Must have a
          format that is displayed by the control.
      }
  public
    { IPreview methods: partial implementation }
    procedure Display(const DocContent: TEncodedData);
      {Displays document in preview dialog box.
        @param DocContent [in] Content of document to be displayed.
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

procedure TMemoPreviewFrame.Display(const DocContent: TEncodedData);
  {Displays document in preview dialog box.
    @param DocContent [in] Content of document to be displayed.
  }
begin
  // Set margin of preview control
  SetMargin;
  // Load the document
  GetMemoCtrl.Lines.BeginUpdate;
  try
    LoadContent(DocContent);
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

