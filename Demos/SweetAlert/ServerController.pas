unit ServerController;
interface

uses  
  System.SysUtils, System.Classes, EWServerControllerBase, IdContext, 
  IdCustomHTTPServer, IdBaseComponent, IdComponent, IdCustomTCPServer,
  IdHTTPServer, ewBase;

type  
  TEWServerController = class(TewBaseServerController) 
  private  
    { Private declarations }
  public  
    { Public declarations } 
  end;

implementation

{%CLASSGROUP 'Vcl.Controls.TControl'}

{$R *.dfm}

initialization

TEWServerController.Initialize;

end.