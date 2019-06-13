object Form57: TForm57
  Left = 0
  Top = 0
  Width = 610
  Height = 362
  Color = clWhite
  PixelsPerInch = 96
  TextHeight = 13
  object EWNavBar1: TEWNavBar
    Left = 0
    Top = 0
    Width = 594
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
          'Primary'
          'Success')
        Text = 'NavBar Style'
      end>
    Title = 'EasyWeb'
    SearchOptions.ButtonText = 'Search'
    SearchOptions.Placeholder = 'Search'
    SearchOptions.Visible = True
    Style = nbsSuccess
    OnBrandClick = EWNavBar1BrandClick
    OnItemClick = EWNavBar1ItemClick
    OnSearch = EWNavBar1Search
    ExplicitWidth = 663
  end
  object EWLabel1: TEWLabel
    Left = 16
    Top = 63
    Width = 353
    Height = 41
    Font.Size = 0
    Font.Style = []
    Font.Color = clBlack
    Text = ''
  end
end
