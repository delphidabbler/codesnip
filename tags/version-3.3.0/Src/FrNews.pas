{
 * FrNews.pas
 *
 * Frame used to display news items, containing navigation controls to move
 *  backwards and forwards through a list of news items.
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
 * The Original Code is FrNews.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2006-2009 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
}


unit FrNews;


interface


uses
  // Delphi
  Classes, ActnList, ImgList, Controls, StdCtrls, Buttons, Forms,
  // Project
  FrBrowserBase, FrNewsHTML;


type

  {
  TNewsFrame:
    Frame used to display news items, containing navigation controls to move
    backwards and forwards through a list of news items.
  }
  TNewsFrame = class(TFrame)
    actNext: TAction;
    actPrev: TAction;
    alNews: TActionList;
    frmDisplay: TNewsHTMLFrame;
    ilNews: TImageList;
    sbNext: TSpeedButton;
    sbPrev: TSpeedButton;
    stPages: TStaticText;
    procedure actNextExecute(Sender: TObject);
    procedure actNextUpdate(Sender: TObject);
    procedure actPrevExecute(Sender: TObject);
    procedure actPrevUpdate(Sender: TObject);
  private
    fPages: TStringList;
      {List of available news items}
    fCurrentPage: Integer;
      {Index of currently displayed news page}
    function PageCount: Integer;
      {Gets number of available news pages.
        @return Number of pages.
      }
    function FormatPage(const HTML: string; const Date: TDateTime): string;
      {Formats new page for display.
        @param HTML [in] HTML code of news item.
        @param Date [in] Publication date added to new item.
        @return HTML including publication date.
      }
    procedure DisplayHTML(const HTML: string);
      {Displays HTML in browser frame.
        @param HTML [in] HTML code to be displayed.
      }
    procedure DisplayPage(const Idx: Integer);
      {Displays a news item.
        @param Idx [in] Index of news item to be displayed.
      }
    procedure DisplayPageNumber(const Idx: Integer);
      {Displays page number of a news item.
        @param Idx [in] Index of news item to be displayed.
      }
    procedure HidePageNumber;
      {Hides news item page number.
      }
    procedure DisplayNoNewsPage;
      {Display "no news" page used when there are no news items.
      }
  public
    constructor Create(AOwner: TComponent); override;
      {Class constructor. Sets up frame object.
        @param AOwner [in] Component that owns frame.
      }
    destructor Destroy; override;
      {Class destructor. Tears down object.
      }
    procedure Initialize;
      {Initialises display by displaying placeholder page.
      }
    procedure AddPage(const HTML: string; const Date: TDateTime);
      {Adds a news item page to list of news item pages.
        @param HTML [in] HTML code to display.
        @param Date [in] Date of news item, added to displayed page.
      }
    procedure ShowNews;
      {Shows news. If there are news items first one is displayed. If no news
      items page stating that fact is displayed.
      }
  end;


implementation


uses
  // Delphi
  SysUtils,
  // Project
  UHTMLUtils, UIStringList;


{$R *.dfm}


{ TNewsFrame }

procedure TNewsFrame.actNextExecute(Sender: TObject);
  {Displays next news item.
    @param Sender [in] Not used.
  }
begin
  Inc(fCurrentPage);
  DisplayPage(fCurrentPage);
end;

procedure TNewsFrame.actNextUpdate(Sender: TObject);
  {Disables actNext action if current page is last one or there are no news
  items.
    @param Sender [in] Not used.
  }
begin
  actNext.Enabled := (fCurrentPage < Pred(PageCount));
end;

procedure TNewsFrame.actPrevExecute(Sender: TObject);
  {Displays previous news item.
    @param Sender [in] Not used.
  }
begin
  Dec(fCurrentPage);
  DisplayPage(fCurrentPage);
end;

procedure TNewsFrame.actPrevUpdate(Sender: TObject);
  {Disables actPrev action if current page is first one or there are no news
  items.
    @param Sender [in] Not used.
  }
begin
  actPrev.Enabled := (fCurrentPage > 0) and (PageCount > 0);
end;

procedure TNewsFrame.AddPage(const HTML: string; const Date: TDateTime);
  {Adds a news item page to list of news item pages.
    @param HTML [in] HTML code to display.
    @param Date [in] Date of news item, added to displayed page.
  }
begin
  fPages.Add(FormatPage(HTML, Date));
end;

constructor TNewsFrame.Create(AOwner: TComponent);
  {Class constructor. Sets up frame object.
    @param AOwner [in] Component that owns frame.
  }
begin
  inherited;
  fPages := TStringList.Create;
end;

destructor TNewsFrame.Destroy;
  {Class destructor. Tears down object.
  }
begin
  FreeAndNil(fPages);
  inherited;
end;

procedure TNewsFrame.DisplayHTML(const HTML: string);
  {Displays HTML in browser frame.
    @param HTML [in] HTML code to be displayed.
  }
begin
  frmDisplay.LoadPage(HTML);
end;

procedure TNewsFrame.DisplayNoNewsPage;
  {Display "no news" page used when there are no news items.
  }
var
  DivAttrs: IHTMLAttributes;  // attributes of div tag
resourcestring
  sNoNewsMsg = 'No news at present';  // message displayed when there's no news
begin
  DivAttrs := THTMLAttributes.Create;
  DivAttrs.Add('class', TIStringList.Create(['message', 'highlight']));
  DisplayHTML(
    MakeCompoundTag('div', DivAttrs, MakeCompoundTag('strong', sNoNewsMsg))
  );
  HidePageNumber;
end;

procedure TNewsFrame.DisplayPage(const Idx: Integer);
  {Displays a news item.
    @param Idx [in] Index of news item to be displayed.
  }
begin
  DisplayHTML(fPages[Idx]);
  DisplayPageNumber(Idx);
end;

procedure TNewsFrame.DisplayPageNumber(const Idx: Integer);
  {Displays page number of a news item.
    @param Idx [in] Index of news item to be displayed.
  }
resourcestring
  sItemNumber = '%0:d of %1:d'; // page number format string
begin
  stPages.Caption := Format(sItemNumber, [Idx + 1, PageCount]);
  stPages.Left := (Width - stPages.Width) div 2;
  stPages.Visible := True;
end;

function TNewsFrame.FormatPage(const HTML: string;
  const Date: TDateTime): string;
  {Formats new page for display.
    @param HTML [in] HTML code of news item.
    @param Date [in] Publication date added to new item.
    @return HTML including publication date.
  }
var
  DivAttrs: IHTMLAttributes;  // attributes of div tag
begin
  DivAttrs := THTMLAttributes.Create;
  DivAttrs.Add('class', 'dateline');
  Result := MakeCompoundTag('div', DivAttrs, MakeSafeHTMLText(DateToStr(Date)))
    + HTML;
end;

procedure TNewsFrame.HidePageNumber;
  {Hides news item page number.
  }
begin
  stPages.Visible := False;
end;

procedure TNewsFrame.Initialize;
  {Initialises display by displaying placeholder page.
  }
var
  DivAttrs: IHTMLAttributes;  // attributes of div tag
resourcestring
  sInitMsg = 'NEWS PANE';     // news pane initial text
begin
  // Align static text control that displays page number and hide it
  // (alignment needed since Align property of control is ignored by frame)
  stPages.Top := sbNext.Top + 4;
  HidePageNumber;
  // Prepare to display first news page
  fCurrentPage := 0;
  // Display notice that this is news pane
  DivAttrs := THTMLAttributes.Create;
  DivAttrs.Add('class', 'message');
  DisplayHTML(
    MakeCompoundTag('div', DivAttrs, Format('&mdash; %0:s &mdash;', [sInitMsg]))
  );
end;

function TNewsFrame.PageCount: Integer;
  {Gets number of available news pages.
    @return Number of pages.
  }
begin
  Result := fPages.Count;
end;

procedure TNewsFrame.ShowNews;
  {Shows news. If there are news items first one is displayed. If no news items
  page stating that fact is displayed.
  }
begin
  fCurrentPage := 0;
  if PageCount > 0 then
    DisplayPage(fCurrentPage)
  else
    DisplayNoNewsPage;
end;

end.

