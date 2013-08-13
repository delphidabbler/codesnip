unit FmTestMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, StdCtrls,
  ExtCtrls, ComCtrls;

type
  TForm1 = class(TForm)
    pcMain: TPageControl;
    tsSnippetEdit: TTabSheet;
    edTitle: TLabeledEdit;
    lblDesc: TLabel;
    edDesc: TMemo;
    lblLanguage: TLabel;
    cbLanguage: TComboBox;
    lblSnippets: TLabel;
    lvSnippets: TListView;
    btnAdd: TButton;
    btnUpdate: TButton;
    btnDelete: TButton;
    lblIDDesc: TLabel;
    stID: TStaticText;
    btnNew: TButton;
    lblSource: TLabel;
    edSource: TMemo;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

end.
