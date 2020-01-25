inherited MainForm: TMainForm
  Left = 204
  Top = 150
  Caption = 'MainForm'
  ClientHeight = 444
  ClientWidth = 605
  Constraints.MinHeight = 360
  Constraints.MinWidth = 480
  Menu = mnuMain
  OnResize = FormResize
  ExplicitWidth = 613
  ExplicitHeight = 495
  PixelsPerInch = 96
  TextHeight = 13
  object sbStatusBar: TStatusBar
    Left = 0
    Top = 424
    Width = 605
    Height = 20
    Panels = <
      item
        Width = 260
      end
      item
        Width = 160
      end
      item
        Width = 50
      end>
  end
  object tbarMain: TToolBar
    Left = 0
    Top = 0
    Width = 605
    Height = 30
    AutoSize = True
    BorderWidth = 1
    Caption = 'tbarMain'
    EdgeBorders = [ebTop, ebBottom]
    Images = ilMain
    ParentShowHint = False
    ShowHint = True
    TabOrder = 1
    object tbSaveSnippet: TToolButton
      Left = 0
      Top = 0
      Action = actSaveSnippet
    end
    object tbSaveUnit: TToolButton
      Left = 23
      Top = 0
      Action = actSaveUnit
    end
    object tbSpacer7: TToolButton
      Left = 46
      Top = 0
      Width = 8
      Caption = 'tbSpacer7'
      ImageIndex = 2
      Style = tbsSeparator
    end
    object tbPrint: TToolButton
      Left = 54
      Top = 0
      Action = actPrint
    end
    object tbSpacer1: TToolButton
      Left = 77
      Top = 0
      Width = 8
      Caption = 'tbSpacer1'
      ImageIndex = 7
      Style = tbsSeparator
    end
    object tbFindText: TToolButton
      Left = 85
      Top = 0
      Action = actFindText
    end
    object tbFindTags: TToolButton
      Left = 108
      Top = 0
      Action = actFindTags
    end
    object tbFindCompiler: TToolButton
      Left = 131
      Top = 0
      Action = actFindCompiler
    end
    object tbSelectSnippets: TToolButton
      Left = 154
      Top = 0
      Action = actSelectSnippets
    end
    object tbFindClear: TToolButton
      Left = 177
      Top = 0
      Action = actFindClear
    end
    object tbSpacer3: TToolButton
      Left = 200
      Top = 0
      Width = 8
      Caption = 'tbSpacer3'
      ImageIndex = 9
      Style = tbsSeparator
    end
    object tbGoBack: TToolButton
      Left = 208
      Top = 0
      Action = actGoBack
      Style = tbsDropDown
    end
    object tbGoForward: TToolButton
      Left = 246
      Top = 0
      Action = actGoForward
      Style = tbsDropDown
    end
    object tbSpacer8: TToolButton
      Left = 284
      Top = 0
      Width = 8
      Caption = 'tbSpacer8'
      ImageIndex = 2
      Style = tbsSeparator
    end
    object tbAddSnippet: TToolButton
      Left = 292
      Top = 0
      Action = actAddSnippet
    end
    object tbEditSnippet: TToolButton
      Left = 315
      Top = 0
      Action = actEditSnippet
    end
    object tbDeleteSnippet: TToolButton
      Left = 338
      Top = 0
      Action = actDeleteSnippet
    end
    object tbSaveDatabase: TToolButton
      Left = 361
      Top = 0
      Action = actSaveDatabase
    end
    object tbSpacer5: TToolButton
      Left = 384
      Top = 0
      Width = 8
      Caption = 'tbSpacer5'
      ImageIndex = 13
      Style = tbsSeparator
    end
    object tbTestCompile: TToolButton
      Left = 392
      Top = 0
      Action = actTestCompile
    end
    object tbSpacer6: TToolButton
      Left = 415
      Top = 0
      Width = 8
      Caption = 'tbSpacer6'
      ImageIndex = 12
      Style = tbsSeparator
    end
    object tbSpacer4: TToolButton
      Left = 423
      Top = 0
      Width = 8
      Caption = 'tbSpacer4'
      ImageIndex = 12
      Style = tbsSeparator
    end
    object tbHelpContents: TToolButton
      Left = 431
      Top = 0
      Action = actHelpContents
    end
  end
  object pnlBody: TPanel
    Left = 0
    Top = 30
    Width = 605
    Height = 394
    Align = alClient
    BevelOuter = bvNone
    BorderWidth = 2
    TabOrder = 2
    object splitVert: TSplitter
      Left = 187
      Top = 2
      Width = 6
      Height = 390
      Beveled = True
      OnCanResize = splitVertCanResize
      ExplicitHeight = 292
    end
    object pnlLeft: TPanel
      Left = 2
      Top = 2
      Width = 185
      Height = 390
      Align = alLeft
      BevelOuter = bvLowered
      TabOrder = 0
      inline frmOverview: TOverviewFrame
        Left = 1
        Top = 1
        Width = 183
        Height = 388
        Align = alClient
        TabOrder = 0
        TabStop = True
        ExplicitLeft = 1
        ExplicitTop = 1
        ExplicitWidth = 183
        ExplicitHeight = 388
        inherited pnlTitle: TPanel
          Width = 183
          ExplicitWidth = 183
          inherited bvlTop: TBevel
            Width = 183
            ExplicitWidth = 183
          end
          inherited tbarOverview: TToolBar
            Left = 89
            ExplicitLeft = 89
          end
        end
        inherited pnlGroupings: TPanel
          Width = 183
          ExplicitWidth = 183
        end
        inherited pnlSnippets: TPanel
          Width = 183
          Height = 320
          ExplicitLeft = 0
          ExplicitTop = 68
          ExplicitWidth = 183
          ExplicitHeight = 320
          inherited tvSnippets: TTreeView
            Width = 181
            Height = 316
            ExplicitLeft = 0
            ExplicitTop = 2
            ExplicitWidth = 181
            ExplicitHeight = 316
          end
        end
      end
    end
    object pnlRight: TPanel
      Left = 193
      Top = 2
      Width = 410
      Height = 390
      Align = alClient
      BevelOuter = bvLowered
      TabOrder = 1
      inline frmDetail: TDetailFrame
        Left = 1
        Top = 1
        Width = 408
        Height = 388
        Align = alClient
        TabOrder = 0
        TabStop = True
        ExplicitLeft = 1
        ExplicitTop = 1
        ExplicitWidth = 408
        ExplicitHeight = 388
        inherited pnlTitle: TPanel
          Width = 408
          ExplicitWidth = 408
          inherited bvlTop: TBevel
            Width = 408
            ExplicitWidth = 408
          end
        end
        inherited frmDetailView: TDetailViewFrame
          Width = 408
          Height = 338
          ExplicitWidth = 408
          ExplicitHeight = 338
          inherited pnlBrowser: TPanel
            Width = 408
            Height = 338
            ExplicitWidth = 408
            ExplicitHeight = 338
            inherited wbBrowser: TWebBrowser
              Width = 408
              Height = 338
              ExplicitWidth = 408
              ExplicitHeight = 308
              ControlData = {
                4C0000002B2A0000EF2200000000000000000000000000000000000000000000
                000000004C000000000000000000000001000000E0D057007335CF11AE690800
                2B2E126208000000000000004C0000000114020000000000C000000000000046
                8000000000000000000000000000000000000000000000000000000000000000
                00000000000000000100000000000000000000000000000000000000}
            end
          end
        end
        inherited tcViews: TTabControl
          Width = 408
          ExplicitWidth = 408
        end
      end
    end
  end
  object ilMain: TImageList
    Left = 262
    Top = 96
  end
  object alMain: TActionList
    Images = ilMain
    Left = 204
    Top = 96
    object actSaveSnippet: TAction
      Category = 'File'
      Caption = 'Save Annotated Source...'
      Hint = 
        'Save annotated source code|Save the annotated source code of the' +
        ' selected snippet or category to a file'
      ImageIndex = 18
      ShortCut = 24654
      OnExecute = actSaveSnippetExecute
      OnUpdate = actSaveSnippetUpdate
    end
    object actAddSnippet: TAction
      Category = 'Snippets'
      Caption = 'New Snippet...'
      Hint = 
        'Create new snippet|Create a new snippet and add it to the databa' +
        'se'
      ImageIndex = 27
      ShortCut = 16429
      OnExecute = actAddSnippetExecute
    end
    object actDeleteSnippet: TAction
      Category = 'Snippets'
      Caption = 'Delete Snippet'
      Hint = 'Delete snippet|Delete the selected snippet from the database'
      ImageIndex = 26
      ShortCut = 16430
      OnExecute = actDeleteSnippetExecute
      OnUpdate = ActEditDeleteSnippetUpdate
    end
    object actSaveUnit: TAction
      Category = 'File'
      Caption = 'Save Unit...'
      Hint = 
        'Save unit|Generate and save a Pascal unit containing the snippet' +
        's in the current selection'
      ImageIndex = 14
      ShortCut = 16469
      OnExecute = actSaveUnitExecute
      OnUpdate = ActNonEmptyDBUpdate
    end
    object actWelcome: TAction
      Category = 'View'
      Caption = 'Welcome Page'
      Hint = 'Welcome page|Display the welcome page'
      ImageIndex = 5
      ShortCut = 32804
      OnExecute = actWelcomeExecute
      OnUpdate = ActNonEmptyDBUpdate
    end
    object actEditSnippet: TAction
      Category = 'Snippets'
      Caption = 'Edit Snippet...'
      Hint = 'Edit snippet|Edit the selected snippet'
      ImageIndex = 28
      ShortCut = 16497
      OnExecute = actEditSnippetExecute
      OnUpdate = ActEditDeleteSnippetUpdate
    end
    object actExit: TFileExit
      Category = 'File'
      Caption = 'Exit'
      Hint = 'Exit|Close the application'
      ImageIndex = 0
    end
    object actAbout: TAction
      Category = 'Help'
      Caption = 'About...'
      Hint = 'About box|Display the program'#39's about box'
      ImageIndex = 2
      OnExecute = actAboutExecute
    end
    object actCopy: TAction
      Category = 'Edit'
      Caption = 'Copy'
      Hint = 'Copy|Copy the selected text to the clipboard'
      ShortCut = 16451
      OnExecute = actCopyExecute
      OnUpdate = actCopyUpdate
    end
    object actFindCompiler: TAction
      Category = 'Search'
      Caption = 'Find Compiler(s)...'
      Hint = 
        'Find compiler(s)|Search for all snippets with a given compiler c' +
        'ompatibility'
      ImageIndex = 4
      ShortCut = 24646
      OnExecute = actFindCompilerExecute
      OnUpdate = ActNonEmptyDBUpdate
    end
    object actFindText: TAction
      Category = 'Search'
      Caption = 'Find Text...'
      Hint = 'Find text|Search for all snippets containing specified text'
      ImageIndex = 3
      ShortCut = 16454
      OnExecute = actFindTextExecute
      OnUpdate = ActNonEmptyDBUpdate
    end
    object actGoBack: TAction
      Category = 'View'
      Caption = 'Previous'
      Hint = 'Previous item|Display the previous item in the history list'
      ImageIndex = 10
      ShortCut = 32805
      OnExecute = actGoBackExecute
      OnUpdate = actGoBackUpdate
    end
    object actGoForward: TAction
      Category = 'View'
      Caption = 'Next'
      Hint = 'Next item|Display the next item in the history list'
      ImageIndex = 11
      ShortCut = 32807
      OnExecute = actGoForwardExecute
      OnUpdate = actGoForwardUpdate
    end
    object actCopySnippet: TAction
      Category = 'Edit'
      Caption = 'Copy Annotated Source'
      Hint = 
        'Copy annotated source code|Copy the annotated source code of the' +
        ' selected snippet or category to the clipboard'
      ShortCut = 16462
      OnExecute = actCopySnippetExecute
      OnUpdate = actCopySnippetUpdate
    end
    object actSelectAll: TAction
      Category = 'Edit'
      Caption = 'Select All'
      Hint = 'Select all text|Select all text in the Details pane'
      ShortCut = 16449
      OnExecute = actSelectAllExecute
      OnUpdate = actSelectAllUpdate
    end
    object actViewGroupByTags: TAction
      Category = 'View'
      AutoCheck = True
      Caption = 'Group Snippets By Tags'
      Checked = True
      GroupIndex = 100
      Hint = 
        'Group Snippets By Tags|Display snippets in overview pane grouped' +
        ' by their associated tags'
      OnExecute = ActOverviewGroupingExecute
      OnUpdate = ActOverviewGroupingUpdate
    end
    object actViewGroupByLanguage: TAction
      Category = 'View'
      Caption = 'Group Snippets By Source Code Language'
      Hint = 
        'Group Snippets By Source Code Language|Display snippets in overv' +
        'iew pane grouped by their source code language'
      OnExecute = ActOverviewGroupingExecute
      OnUpdate = ActOverviewGroupingUpdate
    end
    object actViewGroupAlphabetically: TAction
      Category = 'View'
      AutoCheck = True
      Caption = 'Group Snippets Alphabetically'
      GroupIndex = 100
      Hint = 
        'Group Snippets Alphabetically|Display snippets in overview pane ' +
        'grouped by the initial letter of their title'
      OnExecute = ActOverviewGroupingExecute
      OnUpdate = ActOverviewGroupingUpdate
    end
    object actViewGroupBySnippetKinds: TAction
      Category = 'View'
      Caption = 'Group Snippets By Kind'
      GroupIndex = 100
      Hint = 
        'Group Snippets By Kind|Display snippets in overview pane grouped' +
        ' by kind'
      OnExecute = ActOverviewGroupingExecute
      OnUpdate = ActOverviewGroupingUpdate
    end
    object actHelpQuickStart: TAction
      Category = 'Help'
      Caption = 'QuickStart Guide'
      Hint = 'QuickStart guide|Display the QuickStart help page'
      OnExecute = actHelpQuickStartExecute
    end
    object actCompilers: TAction
      Category = 'Tools'
      Caption = 'Configure Compilers...'
      Hint = 
        'Configure compilers|Configure the program to use installed Delph' +
        'i or Free Pascal compilers'
      ImageIndex = 15
      OnExecute = actCompilersExecute
    end
    object actHelpCompChecks: TAction
      Category = 'Help'
      Caption = 'About Compiler Checks'
      Hint = 'About compiler checks|Display help about test-compiling snippets'
      OnExecute = actHelpCompChecksExecute
    end
    object actHelpContents: TAction
      Category = 'Help'
      Caption = 'Contents'
      Hint = 'Help contents|Display the help contents page'
      ImageIndex = 1
      ShortCut = 112
      OnExecute = actHelpContentsExecute
    end
    object actPreferences: TAction
      Category = 'Tools'
      Caption = 'Preferences...'
      Hint = 'Preferences|Customise the program'
      ImageIndex = 16
      OnExecute = actPreferencesExecute
    end
    object actLicense: TAction
      Category = 'Help'
      Caption = 'License'
      Hint = 'View license|View the end user license agreement'
      ImageIndex = 35
      OnExecute = actLicenseExecute
    end
    object actHomePage: TBrowseURL
      Category = 'Help'
      Caption = 'CodeSnip Home Page'
      Hint = 
        'Codesnip home page|Display CodeSnip'#39's web page in the default we' +
        'b browser'
      ImageIndex = 6
    end
    object actFindTags: TAction
      Category = 'Search'
      Caption = 'Find Tags...'
      Hint = 'Find tags|Search for all snippets with specified tags'
      ImageIndex = 49
      ShortCut = 24660
      OnExecute = actFindTagsExecute
    end
    object actFindXRefs: TAction
      Category = 'Search'
      Caption = 'Find Cross Refs...'
      Hint = 
        'Find X-refs|Search for cross references to and/or from the selec' +
        'ted snippet'
      ImageIndex = 20
      ShortCut = 16466
      OnExecute = actFindXRefsExecute
      OnUpdate = actFindXRefsUpdate
    end
    object actSelectSnippets: TAction
      Category = 'Search'
      Caption = 'Select Snippets...'
      Hint = 'Select snippets|Manually select the snippets to be displayed'
      ImageIndex = 19
      ShortCut = 24659
      OnExecute = actSelectSnippetsExecute
      OnUpdate = ActNonEmptyDBUpdate
    end
    object actFindClear: TAction
      Category = 'Search'
      Caption = 'Show All'
      Hint = 'Show all snippets|Clear the last search and display all snippets'
      ImageIndex = 9
      ShortCut = 24641
      OnExecute = actFindClearExecute
      OnUpdate = actFindClearUpdate
    end
    object actTestCompile: TAction
      Category = 'Compile'
      Caption = 'Test Compile Snippet...'
      Hint = 
        'Test compile|Test compile the currently selected snippet using a' +
        'll configured compilers'
      ImageIndex = 12
      ShortCut = 120
      OnExecute = actTestCompileExecute
      OnUpdate = actTestCompileUpdate
    end
    object actViewCompErrs: TAction
      Category = 'Compile'
      Caption = 'View Compile Errors...'
      Hint = 
        'View compile errors|Display any errors and warnings resulting fr' +
        'om the last test compilation'
      ImageIndex = 34
      OnExecute = actViewCompErrsExecute
      OnUpdate = actViewCompErrsUpdate
    end
    object actViewTestUnit: TAction
      Category = 'Compile'
      Caption = 'View Test Unit...'
      Hint = 
        'View test unit|Display the unit used to test compile the selecte' +
        'd snippet'
      ImageIndex = 22
      OnExecute = actViewTestUnitExecute
      OnUpdate = actViewTestUnitUpdate
    end
    object actTestBug: TAction
      ShortCut = 57410
      OnExecute = actTestBugExecute
    end
    object actNextTab: TAction
      Caption = 'actNextTab'
      ShortCut = 16393
      OnExecute = actNextTabExecute
    end
    object actPreviousTab: TAction
      Caption = 'actPreviousTab'
      ShortCut = 24585
      OnExecute = actPreviousTabExecute
    end
    object actPrint: TAction
      Category = 'File'
      Caption = 'Print...'
      Hint = 'Print|Print the selected snippet or category'
      ImageIndex = 24
      ShortCut = 16464
      OnExecute = actPrintExecute
      OnUpdate = actPrintUpdate
    end
    object actPrivacy: TAction
      Category = 'Help'
      Caption = 'Privacy Statement'
      Hint = 'View privacy statement|View the program'#39's privacy statement'
      OnExecute = actPrivacyExecute
    end
    object actBackupDatabase: TAction
      Category = 'Database'
      Caption = 'Backup Database...'
      Hint = 'Backup Database|Backs up the entire database to a file'
      ImageIndex = 33
      OnExecute = actBackupDatabaseExecute
    end
    object actRestoreDatabase: TAction
      Category = 'Database'
      Caption = 'Restore Database...'
      Hint = 'Restore Database|Restores the database from a backup file'
      ImageIndex = 32
      OnExecute = actRestoreDatabaseExecute
    end
    object actSaveDatabase: TAction
      Category = 'Database'
      Caption = 'Save Database'
      Hint = 'Save Database|Save all changes to the database'
      ImageIndex = 25
      ShortCut = 16467
      OnExecute = actSaveDatabaseExecute
      OnUpdate = actSaveDatabaseUpdate
    end
    object actSubmit: TAction
      Category = 'Snippets'
      Caption = 'Submit Snippets...'
      Hint = 
        'Submit|Submit one or more snippets for possible inclusion in the' +
        ' main database'
    end
    object actExportCode: TAction
      Category = 'Snippets'
      Caption = 'Export Snippets...'
      Hint = 'Export snippets|Export one or more snippets to a file'
      OnExecute = actExportCodeExecute
      OnUpdate = actExportCodeUpdate
    end
    object actImportCode: TAction
      Category = 'Snippets'
      Caption = 'Import Snippets...'
      Hint = 'Import snippets|Import one or more snippets from a file'
      OnExecute = actImportCodeExecute
    end
    object actCopyInfo: TAction
      Category = 'Edit'
      Caption = 'Copy Information'
      Hint = 
        'Copy information|Copy information about the selected snippet to ' +
        'the clipboard'
      ShortCut = 16457
      OnExecute = actCopyInfoExecute
      OnUpdate = actCopyInfoUpdate
    end
    object actViewDependencies: TAction
      Category = 'View'
      Caption = 'Dependencies...'
      Hint = 
        'View dependencies|Display the names of snippets that depend on, ' +
        'or are required, by the selected snippet'
      ImageIndex = 31
      ShortCut = 16452
      OnExecute = actViewDependenciesExecute
      OnUpdate = actViewDependenciesUpdate
    end
    object actCollapseTree: TAction
      Category = 'View'
      Caption = 'Collapse All Sections'
      Hint = 'Collapse all sections|Collapse all sections in the Overview pane'
      ImageIndex = 47
      OnExecute = ActTreeStateChangeExecute
      OnUpdate = ActTreeStateChangeUpdate
    end
    object actExpandTree: TAction
      Category = 'View'
      Caption = 'Expand All Sections'
      Hint = 'Expand all sections|Expand all sections in the Overview pane'
      ImageIndex = 46
      OnExecute = ActTreeStateChangeExecute
      OnUpdate = ActTreeStateChangeUpdate
    end
    object actExpandNode: TAction
      Category = 'View'
      Caption = 'Expand Section'
      Hint = 'Expand section|Expand the current section of the Overview pane'
      ImageIndex = 30
      OnExecute = ActTreeStateChangeExecute
      OnUpdate = ActTreeStateChangeUpdate
    end
    object actCollapseNode: TAction
      Category = 'View'
      Caption = 'Collapse Section'
      Hint = 
        'Collapse section|Collapse the current section of the Overview pa' +
        'ne'
      ImageIndex = 29
      OnExecute = ActTreeStateChangeExecute
      OnUpdate = ActTreeStateChangeUpdate
    end
    object actCopySource: TAction
      Category = 'Edit'
      Caption = 'Copy Source Code'
      Hint = 
        'Copy source code|Copy the source code of the selected snippet to' +
        ' the clipboard'
      ShortCut = 24643
      OnExecute = actCopySourceExecute
      OnUpdate = actCopySourceUpdate
    end
    object actNewDetailsTab: TAction
      Category = 'View'
      Caption = 'New Tab'
      Hint = 'New tab|Create a new, empty, tab the Details pane'
      ImageIndex = 39
      ShortCut = 16468
      OnExecute = actNewDetailsTabExecute
    end
    object actCloseDetailsTab: TAction
      Category = 'View'
      Caption = 'Close Tab'
      Hint = 'Close tab|Close the currently selected tab in the Details pane'
      ImageIndex = 38
      ShortCut = 16499
      SecondaryShortCuts.Strings = (
        'Ctrl+W')
      OnExecute = actCloseDetailsTabExecute
      OnUpdate = actCloseDetailsTabsUpdate
    end
    object actDuplicateSnippet: TAction
      Category = 'Snippets'
      Caption = 'Duplicate Snippet'
      Hint = 
        'Duplicate snippet|Duplicate the selected snippet and add it to t' +
        'he database'
      ImageIndex = 37
      ShortCut = 24644
      OnExecute = actDuplicateSnippetExecute
      OnUpdate = actDuplicateSnippetUpdate
    end
    object actSaveSelection: TAction
      Category = 'File'
      Caption = 'Save Selection...'
      Hint = 
        'Save current selection|Save information about the current snippe' +
        't selection to a file'
      ShortCut = 41043
      OnExecute = actSaveSelectionExecute
      OnUpdate = actSaveSelectionUpdate
    end
    object actLoadSelection: TAction
      Category = 'File'
      Caption = 'Load Selection...'
      Hint = 
        'Load saved selection|Restore the snippet selection recorded in a' +
        ' previously saved file'
      ShortCut = 41036
      OnExecute = actLoadSelectionExecute
      OnUpdate = ActNonEmptyDBUpdate
    end
    object actCloseUnselectedDetailsTabs: TAction
      Category = 'View'
      Caption = 'Close All Other Tabs'
      Hint = 
        'Close all other tabs|Close all tabs in the Details pane except f' +
        'or the current tab'
      ShortCut = 24691
      OnExecute = actCloseUnselectedDetailsTabsExecute
      OnUpdate = actCloseDetailsTabsUpdate
    end
    object actCloseAllDetailsTabs: TAction
      Category = 'View'
      Caption = 'Close All Tabs'
      Hint = 'Close all tabs|Close all tabs in the Details pane'
      OnExecute = actCloseAllDetailsTabsExecute
      OnUpdate = actCloseDetailsTabsUpdate
    end
    object actFavourites: TAction
      Category = 'Snippets'
      Caption = 'Show Favourites...'
      Hint = 'Show favourites|Display the Favourites dialogue box'
      ImageIndex = 48
      ShortCut = 115
      OnExecute = actFavouritesExecute
    end
    object actToggleFavourite: TAction
      Category = 'Snippets'
      Caption = 'Favourite'
      Hint = 
        'Favourite|Toggle the selected snippet'#39's status as a favourite on' +
        ' and off'
      ShortCut = 41030
      SecondaryShortCuts.Strings = (
        'Ctrl+*')
      OnExecute = actToggleFavouriteExecute
      OnUpdate = actToggleFavouriteUpdate
    end
    object actMoveDatabase: TAction
      Category = 'Database'
      Caption = 'Move Database...'
      Hint = 'Move Database|Move the database to a new directory'
      OnExecute = actMoveDatabaseExecute
    end
    object actSWAGImport: TAction
      Category = 'Snippets'
      Caption = 'Import Snippets From SWAG...'
      Hint = 
        'Import snippets from SWAG|Import one or more snippets from the o' +
        'nline SWAG database'
      OnExecute = actSWAGImportExecute
    end
    object actConfigDiffProg: TAction
      Category = 'Tools'
      Caption = 'Configure Diff Viewer Program...'
      Hint = 
        'Configure Diff Viewer|Display dialogue box where an externl diff' +
        ' viewer program can be configured'
      OnExecute = actConfigDiffProgExecute
    end
    object actBlog: TBrowseURL
      Category = 'Help'
      Caption = 'CodeSnip News Blog'
      Hint = 
        'Codesnip blog|Display the CodeSnip news blog in the default web ' +
        'browser'
      ImageIndex = 6
    end
  end
  object mnuMain: TMainMenu
    Images = ilMain
    Left = 233
    Top = 96
    object miFile: TMenuItem
      Caption = 'File'
      object miSaveSnippet: TMenuItem
        Action = actSaveSnippet
      end
      object miSaveUnit: TMenuItem
        Action = actSaveUnit
      end
      object miSpacer6: TMenuItem
        Caption = '-'
      end
      object miSaveSelection: TMenuItem
        Action = actSaveSelection
      end
      object miLoadSelection: TMenuItem
        Action = actLoadSelection
      end
      object miSpacer17: TMenuItem
        Caption = '-'
      end
      object miPrint: TMenuItem
        Action = actPrint
      end
      object miSpacer1: TMenuItem
        Caption = '-'
      end
      object miExit: TMenuItem
        Action = actExit
      end
    end
    object miEdit: TMenuItem
      Caption = 'Edit'
      object miCopy: TMenuItem
        Action = actCopy
      end
      object miCopyInfo: TMenuItem
        Action = actCopyInfo
      end
      object miCopySnippet: TMenuItem
        Action = actCopySnippet
      end
      object miSourceCode: TMenuItem
        Action = actCopySource
      end
      object miSpacer16: TMenuItem
        Caption = '-'
      end
      object miSelectAll: TMenuItem
        Action = actSelectAll
      end
    end
    object miView: TMenuItem
      Caption = 'View'
      object miGoBack: TMenuItem
        Action = actGoBack
      end
      object miGoForward: TMenuItem
        Action = actGoForward
      end
      object miWelcome: TMenuItem
        Action = actWelcome
      end
      object miSpacer2: TMenuItem
        Caption = '-'
        GroupIndex = 2
      end
      object miViewGroupByTags: TMenuItem
        Action = actViewGroupByTags
        AutoCheck = True
        GroupIndex = 100
        RadioItem = True
      end
      object miViewGroupByLanguage: TMenuItem
        Action = actViewGroupByLanguage
        GroupIndex = 100
      end
      object miViewGroupAlphabetically: TMenuItem
        Action = actViewGroupAlphabetically
        AutoCheck = True
        GroupIndex = 100
        RadioItem = True
      end
      object miViewGroupBySnippetKinds: TMenuItem
        Action = actViewGroupBySnippetKinds
        GroupIndex = 100
      end
      object miSpacer4: TMenuItem
        Caption = '-'
        GroupIndex = 100
      end
      object miNewDetailsTab: TMenuItem
        Action = actNewDetailsTab
        GroupIndex = 100
      end
      object miCloseDetailsTab: TMenuItem
        Action = actCloseDetailsTab
        GroupIndex = 100
      end
      object miCloseAllDetailsTabs: TMenuItem
        Action = actCloseAllDetailsTabs
        GroupIndex = 100
      end
      object miSpacer10: TMenuItem
        Caption = '-'
        GroupIndex = 200
      end
      object miViewDependencies: TMenuItem
        Action = actViewDependencies
        GroupIndex = 200
      end
      object miSpacer15: TMenuItem
        Caption = '-'
        GroupIndex = 200
      end
      object miCollapseNode: TMenuItem
        Action = actCollapseNode
        GroupIndex = 200
      end
      object miExpandNode: TMenuItem
        Action = actExpandNode
        GroupIndex = 200
      end
      object miCollapseTree: TMenuItem
        Action = actCollapseTree
        GroupIndex = 200
      end
      object miExpandTree: TMenuItem
        Action = actExpandTree
        GroupIndex = 200
      end
    end
    object miSearch: TMenuItem
      Caption = 'Search'
      object miFindText: TMenuItem
        Action = actFindText
      end
      object miFindTags: TMenuItem
        Action = actFindTags
      end
      object miFindCompiler: TMenuItem
        Action = actFindCompiler
      end
      object miFindXRefs: TMenuItem
        Action = actFindXRefs
      end
      object miSelectSnippets: TMenuItem
        Action = actSelectSnippets
      end
      object miSpacer8: TMenuItem
        Caption = '-'
      end
      object miFindClear: TMenuItem
        Action = actFindClear
      end
    end
    object miSnippets: TMenuItem
      Caption = 'Snippets'
      object miAddSnippet: TMenuItem
        Action = actAddSnippet
      end
      object miEditSnippet: TMenuItem
        Action = actEditSnippet
      end
      object miDuplicateSnippet: TMenuItem
        Action = actDuplicateSnippet
      end
      object miDeleteSnippet: TMenuItem
        Action = actDeleteSnippet
      end
      object miSpacer12: TMenuItem
        Caption = '-'
      end
      object miExportCode: TMenuItem
        Action = actExportCode
      end
      object miImportCode: TMenuItem
        Action = actImportCode
      end
      object miSWAGImport: TMenuItem
        Action = actSWAGImport
      end
      object miSpacer14: TMenuItem
        Caption = '-'
      end
      object miToggleFavourite: TMenuItem
        Action = actToggleFavourite
      end
      object miFavourites: TMenuItem
        Action = actFavourites
      end
    end
    object miDatabase: TMenuItem
      Caption = 'Database'
      object miSaveDatabase: TMenuItem
        Action = actSaveDatabase
      end
      object miSpacer11: TMenuItem
        Caption = '-'
      end
      object miBackupDatabase: TMenuItem
        Action = actBackupDatabase
      end
      object miRestoreDatabase: TMenuItem
        Action = actRestoreDatabase
      end
      object miSpacer13: TMenuItem
        Caption = '-'
      end
      object miMoveDatabase: TMenuItem
        Action = actMoveDatabase
      end
    end
    object miCompile: TMenuItem
      Caption = 'Compile'
      object miTestCompile: TMenuItem
        Action = actTestCompile
      end
      object miViewCompErrs: TMenuItem
        Action = actViewCompErrs
      end
      object miViewTestUnit: TMenuItem
        Action = actViewTestUnit
        GroupIndex = 200
      end
    end
    object miTools: TMenuItem
      Caption = 'Tools'
      object miPreferences: TMenuItem
        Action = actPreferences
      end
      object miCompilers: TMenuItem
        Action = actCompilers
      end
      object miConfigDiffProg: TMenuItem
        Action = actConfigDiffProg
      end
    end
    object miHelp: TMenuItem
      Caption = 'Help'
      object miHelpContents: TMenuItem
        Action = actHelpContents
      end
      object miHelpQuickStart: TMenuItem
        Action = actHelpQuickStart
      end
      object miHelpCompChecks: TMenuItem
        Action = actHelpCompChecks
      end
      object miPrivacy: TMenuItem
        Action = actPrivacy
      end
      object miLicense: TMenuItem
        Action = actLicense
      end
      object miSpacer3: TMenuItem
        Caption = '-'
      end
      object miHomePage: TMenuItem
        Action = actHomePage
      end
      object miBlog: TMenuItem
        Action = actBlog
      end
      object miSpacer5: TMenuItem
        Caption = '-'
      end
      object miAbout: TMenuItem
        Action = actAbout
      end
    end
  end
  object appEvents: TApplicationEvents
    OnHint = appEventsHint
    Left = 291
    Top = 96
  end
end
