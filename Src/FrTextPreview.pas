{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2005-2021, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Implements a frame used to display previews of plain text documents.
}


unit FrTextPreview;


interface


uses
  // Delphi
  StdCtrls, Classes, Controls, ExtCtrls, Menus,
  // Project
  FrMemoPreview, IntfFrameMgrs, IntfPreview, UEncodings;


type

  {
  TTextPreviewFrame:
    Frame used to display previews of plain text documents, including source
    code.
  }
  TTextPreviewFrame = class(TMemoPreviewFrame,
    IPreview, IClipboardMgr, ISelectionMgr
  )
    edDisplay: TMemo;
  strict protected
    { Implementation of base class' abstract methods }
    function GetMemoCtrl: TCustomMemo; override;
      {Gets reference to memo control used to display plain text.
        @return Required TMemo reference.
      }
    procedure LoadContent(const DocContent: TEncodedData); override;
      {Loads document into memo control.
        @param DocContent [in] Plain text document to be displayed.
      }
  public
    { IPreview: Partially implemented in base class }
    procedure SetPopupMenu(const Menu: TPopupMenu);
      {Sets pop up menu to be displayed when memo control right clicked.
        @param Menu [in] Required popup menu.
      }
    { IClipboardMgr: Implemented in base class }
    { ISelectionMgr: Implemented in base class }
  end;


implementation


{$R *.dfm}


{ TTextPreviewFrame }

function TTextPreviewFrame.GetMemoCtrl: TCustomMemo;
  {Gets reference to memo control used to display plain text.
    @return Required TMemo reference.
  }
begin
  Result := edDisplay;
end;

procedure TTextPreviewFrame.LoadContent(const DocContent: TEncodedData);
  {Loads document into memo control.
    @param DocContent [in] Plain text document to be displayed.
  }
begin
  edDisplay.Text := DocContent.ToString;
end;

procedure TTextPreviewFrame.SetPopupMenu(const Menu: TPopupMenu);
  {Sets pop up menu to be displayed when memo control right clicked.
    @param Menu [in] Required popup menu.
  }
begin
  edDisplay.PopupMenu := Menu;
end;

end.

