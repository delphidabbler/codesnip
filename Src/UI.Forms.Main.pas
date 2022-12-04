unit UI.Forms.Main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, UI.Forms.Root, Vcl.ComCtrls,
  Vcl.ActnCtrls, Vcl.ToolWin, Vcl.ActnMan, Vcl.ActnMenus,
  Vcl.PlatformDefaultStyleActnCtrls, System.Actions, Vcl.ActnList, Vcl.StdActns,
  Vcl.BaseImageCollection, Vcl.ImageCollection, Vcl.ExtActns, Vcl.ExtCtrls,
  System.ImageList, Vcl.ImgList, Vcl.VirtualImageList, Vcl.Menus;

type
  TMainForm = class(TRootForm)
    MainMenuBar: TActionMainMenuBar;
    StatusBar: TStatusBar;
    ActionList: TActionList;
    actSaveSnippet: TAction;
    actAddSnippet: TAction;
    actDeleteSnippet: TAction;
    actSaveUnit: TAction;
    actWelcome: TAction;
    actEditSnippet: TAction;
    actExit: TFileExit;
    actAbout: TAction;
    actCopy: TAction;
    actBugReport: TAction;
    actFindCompiler: TAction;
    actFindText: TAction;
    actGoBack: TAction;
    actGoForward: TAction;
    actCopySnippet: TAction;
    actSelectAll: TAction;
    actViewCategorised: TAction;
    actViewAlphabetical: TAction;
    actViewSnippetKinds: TAction;
    actHelpQuickStart: TAction;
    actCompilers: TAction;
    actHelpCompChecks: TAction;
    actHelpContents: TAction;
    actPreferences: TAction;
    actLicense: TAction;
    actGitHubHome: TBrowseURL;
    actFindXRefs: TAction;
    actSelectSnippets: TAction;
    actFindClear: TAction;
    actTestCompile: TAction;
    actViewCompErrs: TAction;
    actViewTestUnit: TAction;
    actTestBug: TAction;
    actNextTab: TAction;
    actPreviousTab: TAction;
    actPrint: TAction;
    actBackupDatabase: TAction;
    actRestoreDatabase: TAction;
    actSaveDatabase: TAction;
    actUpdateDbase: TAction;
    actExportCode: TAction;
    actImportCode: TAction;
    actCopyInfo: TAction;
    actViewDependencies: TAction;
    actCollapseTree: TAction;
    actExpandTree: TAction;
    actExpandNode: TAction;
    actCollapseNode: TAction;
    actCopySource: TAction;
    actAddCategory: TAction;
    actRenameCategory: TAction;
    actDeleteCategory: TAction;
    actNewDetailsTab: TAction;
    actCloseDetailsTab: TAction;
    actFAQs: TBrowseURL;
    actDuplicateSnippet: TAction;
    actSaveSelection: TAction;
    actLoadSelection: TAction;
    actCloseUnselectedDetailsTabs: TAction;
    actCloseAllDetailsTabs: TAction;
    actFavourites: TAction;
    actAddFavourite: TAction;
    actMoveUserDatabase: TAction;
    actSWAGImport: TAction;
    actBlog: TBrowseURL;
    actDeleteUserDatabase: TAction;
    ActionManager: TActionManager;
    tbarMain: TToolBar;
    tbSaveSnippet: TToolButton;
    tbSaveUnit: TToolButton;
    tbSpacer7: TToolButton;
    tbPrint: TToolButton;
    tbSpacer1: TToolButton;
    tbFindText: TToolButton;
    tbFindCompiler: TToolButton;
    tbSelectSnippets: TToolButton;
    tbFindClear: TToolButton;
    tbSpacer3: TToolButton;
    tbGoBack: TToolButton;
    tbGoForward: TToolButton;
    tbSpacer8: TToolButton;
    tbAddSnippet: TToolButton;
    tbEditSnippet: TToolButton;
    tbDeleteSnippet: TToolButton;
    tbSaveDatabase: TToolButton;
    tbFavourites: TToolButton;
    tbSpacer5: TToolButton;
    tbTestCompile: TToolButton;
    tbSpacer6: TToolButton;
    tbHelpContents: TToolButton;
    pnlBody: TPanel;
    splitVert: TSplitter;
    pnlLeft: TPanel;
    pnlRight: TPanel;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

end.
