unit ewIdeWizard;


interface

uses
  Classes, Windows, DesignIntf, ToolsApi, DesignEditors, EWIdeHelpers;

type

  { TEWProjectnCreator }

  TEWProjectnCreator = class(TEWBaseProjectCreator)
  protected
    function NewProjectSource(const ProjectName: string): IOTAFile; override;
    function GetCreatorType: string; override;
  public

  end;

  { TEWApplicationWizard }

  TEWApplicationWizard = class(TEWBaseWizard, IOTAProjectWizard)
  protected
    procedure Execute; override;
    function GetIDString: string; override;
    function GetGlyph: Cardinal; override;
    function GetComment: string; override;
    function GetName: string; override;
  end;

 TEWFormWizard = class(TEWBaseWizard, IOTAProjectWizard)
  protected
    procedure Execute; override;
    function GetIDString: string; override;
    function GetGlyph: Cardinal; override;
    function GetComment: string; override;
    function GetName: string; override;
  end;

  { THostFormCreator }

  THostFormCreator = class(TEWFormCreator)
  public
    constructor Create(AMainForm: Boolean);
  end;

  THostDataModuleCreator = class(TEWFormCreator)
  public
    constructor Create;
    function GetImplFileName: string; override;
  end;

implementation

uses
  {$WARNiNGS OFF}
  ExptIntf,
  {$WARNINGS ON}
  SysUtils,
  Dialogs,
  System.IOUtils;

const
  C_EW_APP_WIZARD_COMMENT = 'Creates a new blank EasyWeb project';
  C_EW_APP_WIZARD_NAME    = 'EasyWeb Project';

  C_EW_FORM_WIZARD_COMMENT = 'Creates a new EasyWeb form';
  C_EW_FORM_WIZARD_NAME    = 'EasyWeb Form';


{ TEWProjectnCreator }

function TEWProjectnCreator.GetCreatorType: string;
begin
  Result := sApplication;
end;


function TEWProjectnCreator.NewProjectSource(
  const ProjectName: string): IOTAFile;
var
  ASource: string;
begin
  ASource := 'program '+ProjectName+';'+#13#10+#13#10+
'uses '+#13#10+
'  Vcl.Forms,'+#13#10+
 '  ewStartup,'+#13#10+
 '  EWServerControllerBase;'+#13#10+
#13#10+
#13#10+
'{$R *.res}'+#13#10+
#13#10+
'begin'+#13#10+
#13#10+
  '  Application.Initialize; '+#13#10+
  '  Application.MainFormOnTaskbar := True; '+#13#10+
  '  EWRun;'+#13#10+
#13#10+
'end.';

Result :=  StringToIOTAFile(ASource);
end;

{ TEWApplicationWizard }

procedure TEWApplicationWizard.Execute;
var
  AModuleServices: IOTAModuleServices;
  AApp: TEWProjectnCreator;
begin
  if Supports(BorlandIDEServices, IOTAModuleServices, AModuleServices) then
  begin
    AApp := TEWProjectnCreator.Create();
    AModuleServices.CreateModule(AApp);
    AModuleServices.CreateModule(THostDataModuleCreator.Create());
    AModuleServices.CreateModule(THostFormCreator.Create(True));
  end;
end;

function TEWApplicationWizard.GetComment: string;
begin
  Result := C_EW_APP_WIZARD_COMMENT;
end;

function TEWApplicationWizard.GetGlyph: Cardinal;
begin
  Result := LoadIcon(HInstance, 'HOSTAPP');
end;

function TEWApplicationWizard.GetIDString: string;
begin
  //Result := '{02A3E09A-F15F-49DD-8EA2-C604683A682A}';
  Result := '{BBCA7091-2A2B-4232-8FA0-B1A6C57C2C35}';
end;

function TEWApplicationWizard.GetName: string;
begin
  Result := C_EW_APP_WIZARD_NAME;
end;

{ THostFormCreator }

constructor THostFormCreator.Create;
var
  AFormTemplate, AImplTemplate, AIntfTemplate, AInitialization: string;
begin
  AInitialization := '';
  if AMainForm then
    AInitialization := 'initialization '+#13#10+#13#10+
                       'T%FormIdent%.SetAsMainForm;'+#13#10+#13#10;

  AFormTemplate := 'object %FormIdent%: T%FormIdent%' +#13#10+
  'Left = 0' +#13#10+
  'Top = 0' +#13#10+
  'Width = 321' +#13#10+
  'Height = 240' +#13#10+
  'PixelsPerInch = 96' +#13#10+
  'TextHeight = 13' +#13#10+
'end';

  AImplTemplate :=
  'unit %ModuleIdent%;'+#13#10+#13#10+
  'interface'+#13#10+#13#10+
  'uses'+#13#10+
  '  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,'+#13#10+
  '  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, EWForm;'+#13#10+

  'type'+#13#10+
  '  T%FormIdent% = class(TEwForm)'+#13#10+
  '  private'+#13#10+
  '    { Private declarations }'+#13#10+
  '  public'+#13#10+
  '    { Public declarations }'+#13#10+
  '  end;'+#13#10+#13#10+
  ''+#13#10+#13#10+
  'implementation'+#13#10+
#13#10+#13#10+
'{%CLASSGROUP ''Vcl.Controls.TControl''}'+#13#10+
#13#10+#13#10+
  '{$R *.dfm}'+#13#10+
  ''+#13#10+#13#10+

  AInitialization+
  'end. ';

  AIntfTemplate := '';
  inherited Create(AFormTemplate, AImplTemplate, AIntfTemplate);
end;

{ THostDataModuleCreator }

constructor THostDataModuleCreator.Create;
var
  AFormTemplate, AImplTemplate, AIntfTemplate: string;
begin

  AFormTemplate := 'object EWServerController: TEWServerController '+#13+
  '  OldCreateOrder = True '+#13#10+
  '  Height = 150 '+#13#10+
  '  Width = 215 '+#13#10+
  'end';

  AImplTemplate := 'unit ServerController;'+#13+

'interface'+#13#10+
#13#10+
'uses  '+#13#10+
'  System.SysUtils, System.Classes, EWServerControllerBase, IdContext, '+#13#10+
'  IdCustomHTTPServer, IdBaseComponent, IdComponent, IdCustomTCPServer,'+#13#10+
'  IdHTTPServer, ewBase;'+#13+
#13#10+
'type  '+#13#10+
'  TEWServerController = class(TewBaseServerController) '+#13#10+
'  private  '+#13#10+
'    { Private declarations }'+#13#10+
'  public  '+#13#10+
'    { Public declarations } '+#13#10+
'  end;'+#13#10+
#13#10+#13#10+
#13#10+
#13#10+


'implementation'+#13#10+
#13#10+

'function EWServerController: TEWServerController;'+#13#10+
'begin' +#13#10+
'  Result := TEWServerController(GlobalServerController);' +#13#10+
'end; '+#13#10+#13#10+
//'{%CLASSGROUP ''Vcl.Controls.TControl''}'+#13#10+
#13#10+
'{$R *.dfm}'+#13#10+
#13#10+#13#10+
'initialization'+#13#10+#13#10+
'TEWServerController.Initialize;'+#13#10+#13#10+
'end.';

  AIntfTemplate := '';
  inherited Create(AFormTemplate, AImplTemplate, AIntfTemplate);

end;

function THostDataModuleCreator.GetImplFileName: string;
begin
  Result := IncludeTrailingPathDelimiter(TPath.GetDocumentsPath)+'Embarcadero\Studio\Projects\ServerController.pas';

end;

{ TEWFormWizard }

procedure TEWFormWizard.Execute;
var
  AModuleServices: IOTAModuleServices;
begin
  if Supports(BorlandIDEServices, IOTAModuleServices, AModuleServices) then
  begin
    AModuleServices.CreateModule(THostFormCreator.Create(False));
  end;
end;

function TEWFormWizard.GetComment: string;
begin
  Result := C_EW_FORM_WIZARD_COMMENT;
end;

function TEWFormWizard.GetGlyph: Cardinal;
begin
  Result := LoadIcon(HInstance, 'HOSTAPP');
end;

function TEWFormWizard.GetIDString: string;
begin
  Result := '{81745449-6557-4BFE-BAC7-D6C0A7EAD7F7}';
end;

function TEWFormWizard.GetName: string;
begin
  Result := C_EW_FORM_WIZARD_NAME;
end;

end.

