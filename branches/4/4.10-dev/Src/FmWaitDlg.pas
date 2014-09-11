{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2006-2013, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * Implements a borderless dialogue box that displays a message.
 *
 * Designed for display when the application is waiting.
}


unit FmWaitDlg;


interface


uses
  // Delphi
  Controls, StdCtrls, Classes, ExtCtrls, Forms, Messages,
  // Project
  FmBase, IntfAligner, UMarquee;


type

  {
  TWaitDlg:
    Implements dialog box for display when application is waiting. Dialog box
    is borderless, displays via its Caption property and shows an animation.
  }
  TWaitDlg = class(TBaseForm)
    lblCaption: TLabel;
    pnlMain: TPanel;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
  strict private
    fMarquee: TMarquee;     // Custom marquee component instance
    fFreeOnClose: Boolean;  // Whether form should free itself on closure
    procedure CMTextChanged(var Msg: TMessage); message CM_TEXTCHANGED;
      {Triggered when form's caption is set. Displays form caption in
      lblCaption.
        @var Msg [in/out] Not used.
      }
  strict protected
    function GetAligner: IFormAligner; override;
      {Creates and returns reference to an object that is used to align the form
      to the owner.
        @return Required aligner object instance.
      }
    procedure CustomiseForm; override;
      {Sizes window to fit caption text and creates and locates the marquee
      control.
      }
    procedure InitForm; override;
      {Sets hourglass cursor and starts marquee when form is shown.
      }
  public
    class function CreateAutoFree(AOwner: TComponent;
      const ACaption: string): TWaitDlg;
      {Creates an instance of the wait dialogue box with a given caption which
      frees itself when the dialogue box is closed.
        @param AOwner [in] Component that owns the dialogue box.
        @param ACaption [in] Caption to be displayed in dialogue box.
      }
  end;


implementation


uses
  // Delphi
  Types, Math,
  // Project
  UDlgHelper, UFontHelper, UFormAligner, UGraphicUtils;


{$R *.dfm}


{ TWaitDlg }

procedure TWaitDlg.CMTextChanged(var Msg: TMessage);
  {Triggered when form's caption is set. Displays form caption in lblCaption.
    @var Msg [in/out] Not used.
  }
begin
  inherited;
  lblCaption.Caption := Text;
end;

class function TWaitDlg.CreateAutoFree(AOwner: TComponent;
  const ACaption: string): TWaitDlg;
  {Creates an instance of the wait dialogue box with a given caption which frees
  itself when the dialogue box is closed.
    @param AOwner [in] Component that owns the dialogue box.
    @param ACaption [in] Caption to be displayed in dialogue box.
  }
begin
  Result := TWaitDlg.Create(AOwner);
  Result.Caption := ACaption;
  Result.fFreeOnClose := True;
end;

procedure TWaitDlg.CustomiseForm;
  {Sizes window to fit caption text and creates and locates the marquee control.
  }
const
  MinFormWidth = 168;
begin
  inherited;
  TFontHelper.SetDefaultBaseFont(lblCaption.Font);
  // Size window and centre label in it (pnlMain auto-sizes to window)
  Self.ClientWidth := Max(MinFormWidth, lblCaption.Width + 24);
  lblCaption.Left := (pnlMain.ClientWidth - lblCaption.Width) div 2;
  // Create and locate marquee
  fMarquee := TMarquee.CreateInstance(Self);
  fMarquee.Parent := pnlMain;
  fMarquee.Left := 12;
  fMarquee.Width := pnlMain.ClientWidth - 24;
  fMarquee.Top := lblCaption.Top + lblCaption.Height + 8;
  fMarquee.Height := 13;
end;

procedure TWaitDlg.FormClose(Sender: TObject; var Action: TCloseAction);
  {Resets default cursor and halts marquee when form closes.
    @param Sender [in] Not used.
    @param Action [in/out] Not used or set.
  }
begin
  inherited;
  fMarquee.Stop;
  Screen.Cursor := crDefault;
  if fFreeOnClose then
    Action := caFree;
end;

procedure TWaitDlg.FormCreate(Sender: TObject);
  {Handles form's OnCreate event. Sets handle of Owner control as parent of
  form's window. If Owner doesn't have handle then either active form or main
  form used as parent.
    @param Sender [in] Not used.
  }
begin
  inherited;
  TDlgHelper.SetDlgParentToOwner(Self);
end;

function TWaitDlg.GetAligner: IFormAligner;
  {Creates and returns reference to an object that is used to align the form to
  the owner.
    @return Required aligner object instance.
  }
begin
  Result := TSimpleFormAligner.Create;
end;

procedure TWaitDlg.InitForm;
  {Sets hourglass cursor and starts marquee when form is shown.
  }
begin
  inherited;
  Screen.Cursor := crHourGlass;
  fMarquee.Start;
end;

end.

