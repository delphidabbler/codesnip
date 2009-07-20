inherited PreferencesDlg: TPreferencesDlg
  Left = 425
  Top = 138
  Caption = 'Preferences'
  ClientHeight = 421
  ClientWidth = 462
  ExplicitWidth = 468
  ExplicitHeight = 447
  PixelsPerInch = 96
  TextHeight = 13
  inherited pnlBody: TPanel
    Width = 446
    Height = 377
    ExplicitWidth = 446
    ExplicitHeight = 377
    object pcMain: TPageControl
      Left = 0
      Top = 0
      Width = 446
      Height = 377
      ActivePage = tsPrintingPrefs
      Align = alClient
      TabOrder = 0
      OnChange = pcMainChange
      OnChanging = pcMainChanging
      ExplicitHeight = 393
      object tsGeneralPrefs: TTabSheet
        Caption = 'General'
        ImageIndex = 3
        inline frmGeneralPrefs: TGeneralPrefsFrame
          Left = 4
          Top = 4
          Width = 393
          Height = 310
          TabOrder = 0
          TabStop = True
          ExplicitLeft = 4
          ExplicitTop = 4
          ExplicitWidth = 393
          ExplicitHeight = 310
          inherited gpMeasurement: TGroupBox
            inherited lblUnits: TLabel
              Width = 149
              ExplicitWidth = 149
            end
          end
        end
      end
      object tsSourceCodePrefs: TTabSheet
        Caption = 'Source Code'
        inline frmSourceCodePrefs: TSourcePrefsFrame
          Left = 4
          Top = 4
          Width = 393
          Height = 310
          TabOrder = 0
          TabStop = True
          ExplicitLeft = 4
          ExplicitTop = 4
          ExplicitHeight = 310
          inherited gbSourceCode: TGroupBox
            inherited lblCommentStyle: TLabel
              Width = 85
              ExplicitWidth = 85
            end
          end
          inherited gbFileFormat: TGroupBox
            inherited lblSnippetFileType: TLabel
              Width = 71
              ExplicitWidth = 71
            end
          end
        end
      end
      object tsHiliterPrefs: TTabSheet
        Caption = 'Syntax Highlighter'
        ImageIndex = 1
        inline frmHiliterPrefs: THiliterPrefsFrame
          Left = 4
          Top = 4
          Width = 393
          Height = 325
          TabOrder = 0
          TabStop = True
          ExplicitLeft = 4
          ExplicitTop = 4
          ExplicitHeight = 325
          inherited gbElements: TGroupBox
            inherited lblElements: TLabel
              Width = 46
              ExplicitWidth = 46
            end
            inherited lblColour: TLabel
              Width = 33
              ExplicitWidth = 33
            end
            inherited lblExample: TLabel
              Width = 43
              ExplicitWidth = 43
            end
          end
          inherited gbDocFont: TGroupBox
            inherited lblFontName: TLabel
              Width = 53
              ExplicitWidth = 53
            end
            inherited lblFontSize: TLabel
              Width = 45
              ExplicitWidth = 45
            end
          end
        end
      end
      object tsPrintingPrefs: TTabSheet
        Caption = 'Printing'
        ImageIndex = 2
        inline frmPrintingPrefs: TPrintingPrefsFrame
          Left = 4
          Top = 4
          Width = 393
          Height = 310
          TabOrder = 0
          TabStop = True
          ExplicitLeft = 4
          ExplicitTop = 4
          ExplicitWidth = 393
          ExplicitHeight = 310
          inherited gpMargins: TGroupBox
            inherited lblLeft: TLabel
              Width = 21
              ExplicitWidth = 21
            end
            inherited lblRight: TLabel
              Width = 28
              ExplicitWidth = 28
            end
            inherited lblBottom: TLabel
              Width = 36
              ExplicitWidth = 36
            end
          end
        end
      end
    end
  end
  inherited btnOK: TButton
    Left = 151
    Top = 303
    OnClick = btnOKClick
    ExplicitLeft = 151
    ExplicitTop = 303
  end
end
