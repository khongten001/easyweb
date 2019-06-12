object Form57: TForm57
  Left = 0
  Top = 0
  Width = 473
  Height = 287
  Color = clBtnFace
  PixelsPerInch = 96
  TextHeight = 13
  object EWNavBar1: TEWNavBar
    Left = 0
    Top = 0
    Width = 457
    Height = 41
    Items = <
      item
        DropdownItems.Strings = (
          'Item1'
          'Item 2')
        Text = 'Home'
      end
      item
        Text = 'Pages 1'
      end
      item
        Text = 'Page 2'
      end
      item
        DropdownItems.Strings = (
          'Default'
          'Light'
          'Dark'
          'Primary')
        Text = 'NavBar Style'
      end>
    Title = 'EasyWeb'
    Style = nbsPrimary
    OnBrandClick = EWNavBar1BrandClick
    OnItemClick = EWNavBar1ItemClick
  end
  object EWLabel1: TEWLabel
    Left = 16
    Top = 63
    Width = 353
    Height = 41
    Font.Style = []
    Font.Color = clBlack
    Text = ''
  end
end
