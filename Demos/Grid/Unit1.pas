unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, EWForm, EWButtons, EWBase, EWLayout,
  Vcl.Imaging.jpeg, EWImages, EWProgressBars, EWEdits, EWNavBar, SessionDataUnit;
type
  TForm53 = class(TEwForm)
    EWLayoutGrid1: TEWLayoutGrid;
    EWButton6: TEWButton;
    EWButton7: TEWButton;
    EWButton8: TEWButton;
    EWButton9: TEWButton;
    EWButton10: TEWButton;
    EWLayoutGrid2: TEWLayoutGrid;
    EWImage1: TEWImage;
    EWImage2: TEWImage;
    EWImage3: TEWImage;
    EWLayoutGrid3: TEWLayoutGrid;
    EWProgressBar1: TEWProgressBar;
    EWProgressBar4: TEWProgressBar;
    EWProgressBar2: TEWProgressBar;
    EWProgressBar3: TEWProgressBar;
  private
    function GetSessionData: TEWSessionData;
    { Private declarations }
  public
    property SessionData: TEWSessionData read GetSessionData;
    { Public declarations }
  end;

implementation

{%CLASSGROUP 'Vcl.Controls.TControl'}

{$R *.dfm}

function TForm53.GetSessionData: TEWSessionData;
begin
  Result := (Session.DataModule as TEWSessionData);
end;

initialization

TForm53.SetAsMainForm;

end.