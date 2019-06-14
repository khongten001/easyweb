unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, EWForm, EWBase, EWButtons, EWNavBar,
  EWLabel, Vcl.Imaging.jpeg, EWTypes, EWLayout, EWProgressBars, EWImages,
  EWEdits;
type
  TForm57 = class(TEwForm)
    EWNavBar1: TEWNavBar;
    EWLayout1: TEWLayout;
    EWLabel1: TEWLabel;
    EWButton1: TEWButton;
    EWButton2: TEWButton;
    EWLayout2: TEWLayout;
    EWImage1: TEWImage;
    EWComboBox1: TEWComboBox;
    procedure EWButton1Click(Sender: TObject);
    procedure EWNavBar1ItemClick(Sender: TObject; AItem: TCollectionItem;
      ADropDownIndex: Integer);
    procedure EWNavBar1BrandClick(Sender: TObject);
    procedure EWImage1MouseEnter(Sender: TObject);
    procedure EWImage1MouseLeave(Sender: TObject);
    procedure EWImage1Click(Sender: TObject);
    procedure EWImage1RightClick(Sender: TObject);
    procedure EWImage1DblClick(Sender: TObject);
    procedure EWNavBar1Search(Sender: TObject; ASearch: string);
    procedure EWComboBox1Change(Sender: TObject);
    procedure EWButton2Click(Sender: TObject);
    procedure EWComboBox1MouseLeave(Sender: TObject);
    procedure EWComboBox1MouseEnter(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{%CLASSGROUP 'Vcl.Controls.TControl'}


{$R *.dfm}


procedure TForm57.EWButton1Click(Sender: TObject);
begin
  EWLabel1.Font.Variant := fvSmallCaps;

end;

procedure TForm57.EWButton2Click(Sender: TObject);
begin
  EWComboBox1.ItemIndex := 2;
end;

procedure TForm57.EWComboBox1Change(Sender: TObject);
begin
  EWLabel1.Text := EWComboBox1.ItemIndex.ToString;
end;

procedure TForm57.EWComboBox1MouseEnter(Sender: TObject);
begin
EWLabel1.Text := 'Enter';
end;

procedure TForm57.EWComboBox1MouseLeave(Sender: TObject);
begin
EWLabel1.Text := 'Exit';

end;

procedure TForm57.EWImage1Click(Sender: TObject);
begin
  EWLabel1.Text := 'Image Clicked';
end;

procedure TForm57.EWImage1DblClick(Sender: TObject);
begin
  EWLabel1.Text := 'You dobule-clicked on the image';
end;

procedure TForm57.EWImage1MouseEnter(Sender: TObject);
begin
  EWLabel1.Text := 'Image entered';
end;

procedure TForm57.EWImage1MouseLeave(Sender: TObject);
begin
  EWLabel1.Text := 'Image exited';
end;

procedure TForm57.EWImage1RightClick(Sender: TObject);
begin
  EWLabel1.Text := 'You right-clicked on the image';

end;

procedure TForm57.EWNavBar1BrandClick(Sender: TObject);
begin
  EWLabel1.Text := 'You clicked on the Logo/Title item';
end;

procedure TForm57.EWNavBar1ItemClick(Sender: TObject; AItem: TCollectionItem;
  ADropDownIndex: Integer);
begin
  if ADropDownIndex > -1 then
    EWLabel1.Text := 'You clicked: "'+TEWNavBarItem(AItem).DropdownItems[ADropDownIndex]+ '" (index '+AItem.Index.ToString+', sub-index '+ADropDownIndex.ToString+')'
  else
    EWLabel1.Text := 'You clicked: "'+TEWNavBarItem(AItem).Text+ '" (index '+AItem.Index.ToString+')';

  if AItem.Index = 3 then
  begin
    case ADropDownIndex of
      0: EWNavBar1.Style := nbsDefault;
      1: EWNavBar1.Style := nbsLight;
      2: EWNavBar1.Style := nbsDark;
      3: EWNavBar1.Style := nbsPrimary;
      4: EWNavBar1.Style := nbsSuccess;
      5: EWNavBar1.Style := nbsWarning;
      6: EWNavBar1.Style := nbsDanger;
    end;
  end;
end;

procedure TForm57.EWNavBar1Search(Sender: TObject; ASearch: string);
begin
  if Trim(ASearch) <> '' then
    EWLabel1.Text := 'You searched for: '+ASearch;
end;

initialization

TForm57.SetAsMainForm;

end.