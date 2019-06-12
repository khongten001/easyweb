unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, EWForm, EWButtons, EWBase, EWLayout,
  Vcl.Imaging.jpeg, EWImages, EWProgressBars, EWEdits, EWNavBar;
type
  TForm53 = class(TEwForm)
    EWLayout1: TEWLayout;
    ewButton1: TEWButton;
    ewButton2: TEWButton;
    EWButton3: TEWButton;
    ewButton4: TEWButton;
    EWButton5: TEWButton;
    EWLayout2: TEWLayout;
    EWImage1: TEWImage;
    EWImage2: TEWImage;
    EWImage3: TEWImage;
    EWLayout3: TEWLayout;
    EWProgressBar1: TEWProgressBar;
    EWProgressBar2: TEWProgressBar;
    EWProgressBar3: TEWProgressBar;
    EWProgressBar4: TEWProgressBar;
    procedure ewButton1Click(Sender: TObject);
    procedure EWButton3Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{%CLASSGROUP 'Vcl.Controls.TControl'}


{$R *.dfm}


procedure TForm53.ewButton1Click(Sender: TObject);
begin
  EWImage1.Visible := not EWImage1.Visible;
end;

procedure TForm53.EWButton3Click(Sender: TObject);
begin
  EWLayout1.Margin := 12;

end;

initialization

TForm53.SetAsMainForm;

end.