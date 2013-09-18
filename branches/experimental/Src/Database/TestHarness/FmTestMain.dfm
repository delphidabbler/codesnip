object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 670
  ClientWidth = 873
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object pcMain: TPageControl
    Left = 0
    Top = 0
    Width = 873
    Height = 670
    ActivePage = tsSnippetEdit
    Align = alClient
    TabOrder = 0
    object tsSnippetEdit: TTabSheet
      Caption = 'Basic Snippet Editor'
      object lblDesc: TLabel
        Left = 16
        Top = 72
        Width = 57
        Height = 13
        Caption = 'Description:'
        FocusControl = edDesc
      end
      object lblLanguage: TLabel
        Left = 16
        Top = 283
        Width = 51
        Height = 13
        Caption = 'Language:'
        FocusControl = cbLanguage
      end
      object lblSnippets: TLabel
        Left = 16
        Top = 342
        Width = 45
        Height = 13
        Caption = 'Snippets:'
        FocusControl = lvSnippets
      end
      object lblIDDesc: TLabel
        Left = 16
        Top = 3
        Width = 15
        Height = 13
        Caption = 'ID:'
      end
      object lblSource: TLabel
        Left = 416
        Top = 17
        Width = 65
        Height = 13
        Caption = 'Source Code:'
        FocusControl = edSource
      end
      object edTitle: TLabeledEdit
        Left = 16
        Top = 41
        Width = 385
        Height = 21
        EditLabel.Width = 24
        EditLabel.Height = 13
        EditLabel.Caption = 'Title:'
        TabOrder = 0
      end
      object edDesc: TMemo
        Left = 16
        Top = 91
        Width = 385
        Height = 182
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Courier New'
        Font.Style = []
        ParentFont = False
        ScrollBars = ssVertical
        TabOrder = 1
      end
      object cbLanguage: TComboBox
        Left = 16
        Top = 302
        Width = 177
        Height = 21
        Style = csDropDownList
        TabOrder = 2
        Items.Strings = (
          'Text'
          'HTML'
          'JavaScript'
          'Pascal'
          'PHP'
          'XHTML')
      end
      object lvSnippets: TListView
        Left = 16
        Top = 361
        Width = 729
        Height = 264
        Columns = <
          item
            Caption = 'ID'
            Width = 220
          end
          item
            Caption = 'Title'
            Width = 360
          end
          item
            Caption = 'Modified'
            Width = 120
          end>
        TabOrder = 3
        ViewStyle = vsReport
        OnDblClick = lvSnippetsDblClick
      end
      object btnAdd: TButton
        Left = 760
        Top = 361
        Width = 75
        Height = 25
        Caption = 'Add'
        TabOrder = 4
        OnClick = btnAddClick
      end
      object btnUpdate: TButton
        Left = 760
        Top = 392
        Width = 75
        Height = 25
        Caption = 'Update'
        TabOrder = 5
        OnClick = btnUpdateClick
      end
      object btnDelete: TButton
        Left = 760
        Top = 423
        Width = 75
        Height = 25
        Caption = 'Delete'
        TabOrder = 6
        OnClick = btnDeleteClick
      end
      object stID: TStaticText
        Left = 48
        Top = 3
        Width = 81
        Height = 17
        Caption = '<No snippet>'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 7
      end
      object btnNew: TButton
        Left = 308
        Top = 10
        Width = 93
        Height = 25
        Caption = 'New Snippet'
        TabOrder = 8
        OnClick = btnNewClick
      end
      object edSource: TMemo
        Left = 416
        Top = 35
        Width = 433
        Height = 288
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Courier New'
        Font.Style = []
        ParentFont = False
        ScrollBars = ssBoth
        TabOrder = 9
        WordWrap = False
      end
      object btnSave: TButton
        Left = 760
        Top = 479
        Width = 75
        Height = 25
        Caption = 'Save'
        TabOrder = 10
        OnClick = btnSaveClick
      end
    end
  end
end
