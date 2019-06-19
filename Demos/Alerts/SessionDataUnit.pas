unit SessionDataUnit;
interface

uses  
  System.SysUtils, System.Classes;

type  
  TEWSessionData = class(TDataModule) 
  private  
    { Private declarations }
  public  
    { Public declarations } 
  end;

implementation

uses EWSession;

{%CLASSGROUP 'Vcl.Controls.TControl'}

{$R *.dfm}

initialization

TEWSession.RegisterSessionData(TEWSessionData);

end.