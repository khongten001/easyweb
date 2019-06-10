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

{$R *.dfm}


initialization

TEWServerController.Initialize;

end.