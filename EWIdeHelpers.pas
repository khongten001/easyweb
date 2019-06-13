unit EWIdeHelpers;

interface

uses
  Classes, Windows, DesignIntf, ToolsApi, DesignEditors ;

type
  TEWBaseWizard = class(TNotifierObject,
    IOTARepositoryWizard,
    IOTARepositoryWizard60,
    IOTARepositoryWizard80,
    IOTARepositoryWizard160,
    IOTAWizard)
  protected
    function GetIDString: string; virtual;
    function GetName: string; virtual; abstract;
    function GetState: TWizardState;
    procedure Execute; virtual; abstract;

    function GetAuthor: string;
    function GetComment: string; virtual;
    function GetGlyph: Cardinal; virtual;
    function GetPage: string;
    function GetDesigner: string;
    function GetFrameworkTypes: TArray<string>;
    function GetPlatforms: TArray<string>;
    function GetGalleryCategory: IOTAGalleryCategory; virtual;
    function GetPersonality: string; virtual;
    property GalleryCategory: IOTAGalleryCategory read GetGalleryCategory;
    property Personality: string read GetPersonality;
  end;

  TEWBaseCreator = class(TInterfacedObject, IOTACreator)
  protected
    // IOTACreator
    function GetCreatorType: string; virtual; abstract;
    function GetExisting: Boolean; virtual;
    function GetFileSystem: string; virtual;
    function GetOwner: IOTAModule; virtual;
    function GetUnnamed: Boolean; virtual;
  end;

  TEWBaseFormCreator = class(TEWBaseCreator, IOTAModuleCreator)
  private
    FFormTemplate: string;
    FImplTemplate: string;
    function CreateOTAFile(const ATemplate, AModuleIdent, AFormIdent, AAncestorIdent: string): IOTAFile;
  protected
    function ExpandTemplate(const ATemplate, AModuleIdent, AFormIdent, AAncestorIdent: string): string; virtual;
    function GetCreatorType: string; override;
    function GetAncestorName: string; virtual;
    function GetFormName: string; virtual;
    function GetImplFileName: string; virtual;
    function GetIntfFileName: string; virtual;
    function GetMainForm: Boolean; virtual;
    function GetShowForm: Boolean; virtual;
    function GetShowSource: Boolean; virtual;
    function NewFormFile(const FormIdent, AncestorIdent: string): IOTAFile; virtual;
    function NewImplSource(const ModuleIdent, FormIdent, AncestorIdent: string): IOTAFile; virtual;
    function NewIntfSource(const ModuleIdent, FormIdent, AncestorIdent: string): IOTAFile; virtual;
    procedure FormCreated(const FormEditor: IOTAFormEditor); virtual;
  public
    constructor Create(const AFormTemplate, AImplTemplate: string);
    property FormTemplate: string read FFormTemplate;
    property ImplTemplate: string read FImplTemplate;
  end;

  TEWBaseProjectCreator = class(TEWBaseCreator,
    IOTAProjectCreator50,
    IOTAProjectCreator80,
    IOTAProjectCreator160,
    //IOTAProjectCreator190,
    IOTAProjectCreator)
  protected
    function GetOwner: IOTAModule; override;
    function GetFileName: string; virtual;
    function GetOptionFileName: string; // deprecated;
    function GetShowSource: Boolean; virtual;
    function NewOptionSource(const ProjectName: string): IOTAFile; // deprecated;
    function NewProjectSource(const ProjectName: string): IOTAFile; virtual;
    procedure NewDefaultModule; // deprecated;
    procedure NewProjectResource(const Project: IOTAProject); virtual;
    procedure NewDefaultProjectModule(const Project: IOTAProject); virtual;
    function GetProjectPersonality: string;
    function GetFrameworkType: string;
    function GetPlatforms: TArray<string>;
    function GetSupportedPlatforms: TArray<string>;
    function GetPreferredPlatform: string;
    procedure SetInitialOptions(const NewProject: IOTAProject);
  end;

implementation

uses
  {$WARNiNGS OFF}
  ExptIntf,
  {$WARNINGS ON}
  PlatformAPI,
  SysUtils;


{ TEWBaseWizard }

function TEWBaseWizard.GetIDString: string;
begin
  //Result := ClassName;
  Result := '{2CD0D11B-BDCA-4CF2-938E-5376D54285E0}';
end;

function TEWBaseWizard.GetState: TWizardState;
begin
  Result := [wsEnabled];
end;

function TEWBaseWizard.GetAuthor: string;
begin
  Result := 'Graham Murt';
end;

function TEWBaseWizard.GetComment: string;
begin
  Result := '';
end;

function TEWBaseWizard.GetGlyph: Cardinal;
begin
  Result := 0;
end;

function TEWBaseWizard.GetPage: string;
begin
  Result := 'EasyWeb';
end;

function TEWBaseWizard.GetDesigner: string;
begin
  Result := dVCL;
end;

function TEWBaseWizard.GetFrameworkTypes: TArray<string>;
begin
  Result := TArray<string>.Create(sFrameworkTypeVCL);
end;

function  TEWBaseWizard.GetGalleryCategory: IOTAGalleryCategory;
begin
  result := nil;
end;

function  TEWBaseWizard.GetPersonality: string;
begin
  result :=  sDelphiPersonality;
end;

function TEWBaseWizard.GetPlatforms: TArray<string>;
begin
  Result := TArray<string>.Create(cWin32Platform, cWin64Platform);
end;

{ TEWBaseCreator }

function TEWBaseCreator.GetExisting: Boolean;
begin
  Result := False;
end;

function TEWBaseCreator.GetFileSystem: string;
begin
  Result := '';
end;

function TEWBaseCreator.GetOwner: IOTAModule;
begin
  Result := GetActiveProject;
end;

function TEWBaseCreator.GetUnnamed: Boolean;
begin
  Result := True;
end;

{ TEWBaseFormCreator }

constructor TEWBaseFormCreator.Create(const AFormTemplate, AImplTemplate: string);
begin
  inherited Create;
  FFormTemplate := AFormTemplate;
  FImplTemplate := AImplTemplate;
end;

function TEWBaseFormCreator.ExpandTemplate(
  const ATemplate, AModuleIdent, AFormIdent, AAncestorIdent: string): string;
begin
  Result := ATemplate;
  Result := StringReplace(Result, '%FormIdent%', AFormIdent, [rfReplaceAll]);
  Result := StringReplace(Result, '%AncestorIdent%', AAncestorIdent, [rfReplaceAll]);
  Result := StringReplace(Result, '%ModuleIdent%', AModuleIdent, [rfReplaceAll]);
end;

function TEWBaseFormCreator.GetCreatorType: string;
begin
  Result := sForm;
end;

function TEWBaseFormCreator.GetAncestorName: string;
begin
  Result := '';
end;

function TEWBaseFormCreator.GetFormName: string;
begin
  Result := '';
end;

function TEWBaseFormCreator.GetImplFileName: string;
begin
  Result := '';
end;

function TEWBaseFormCreator.GetIntfFileName: string;
begin
  Result := '';
end;

function TEWBaseFormCreator.GetMainForm: Boolean;
begin
  Result := False;
end;

function TEWBaseFormCreator.GetShowForm: Boolean;
begin
  Result := True;
end;

function TEWBaseFormCreator.GetShowSource: Boolean;
begin
  Result := True;
end;

function TEWBaseFormCreator.NewFormFile(const FormIdent, AncestorIdent: string): IOTAFile;
begin

  Result := CreateOTAFile(FormTemplate, '', FormIdent, AncestorIdent);
end;

function TEWBaseFormCreator.NewImplSource(const ModuleIdent, FormIdent, AncestorIdent: string): IOTAFile;
begin
  Result := CreateOTAFile(ImplTemplate, ModuleIdent, FormIdent, AncestorIdent);
end;

function TEWBaseFormCreator.NewIntfSource(const ModuleIdent, FormIdent, AncestorIdent: string): IOTAFile;
begin
  Result := CreateOTAFile('', ModuleIdent, FormIdent, AncestorIdent);
end;

procedure TEWBaseFormCreator.FormCreated(const FormEditor: IOTAFormEditor);
begin
end;

function TEWBaseFormCreator.CreateOTAFile(const ATemplate, AModuleIdent, AFormIdent, AAncestorIdent: string): IOTAFile;
begin
  if ATemplate <> '' then
    Result := TOTAFile.Create(ExpandTemplate(ATemplate, AModuleIdent, AFormIdent, AAncestorIdent))
  else
    Result := nil;
end;

{ TEWBaseProjectCreator }

function TEWBaseProjectCreator.GetOwner: IOTAModule;
begin
  Result := nil;
end;

function TEWBaseProjectCreator.GetFileName: string;
begin
  Result := '';
end;

function TEWBaseProjectCreator.GetOptionFileName: string;
begin
  Result := '';
end;

function TEWBaseProjectCreator.GetShowSource: Boolean;
begin
  Result := True;
end;

function TEWBaseProjectCreator.GetSupportedPlatforms: TArray<string>;
begin
  Result := TArray<string>.Create(cWin32Platform, cWin64Platform);
end;

function TEWBaseProjectCreator.NewOptionSource(const ProjectName: string): IOTAFile;
begin
  Result := nil;
end;

function TEWBaseProjectCreator.NewProjectSource(const ProjectName: string): IOTAFile;
begin
  Result := nil;
end;

procedure TEWBaseProjectCreator.NewDefaultModule;
begin
  // do nothing
end;

procedure TEWBaseProjectCreator.NewProjectResource(const Project: IOTAProject);
begin
  // do nothing
end;

procedure TEWBaseProjectCreator.NewDefaultProjectModule(const Project: IOTAProject);
begin
  // do nothing
end;


function TEWBaseProjectCreator.GetProjectPersonality: string;
begin
  Result := sDelphiPersonality;
end;


function TEWBaseProjectCreator.GetFrameworkType: string;
begin
  Result := sFrameworkTypeVCL;
end;

function TEWBaseProjectCreator.GetPlatforms: TArray<string>;
begin
  Result := TArray<string>.Create(cWin32Platform, cWin64Platform);
end;

function TEWBaseProjectCreator.GetPreferredPlatform: string;
begin
  Result := cWin32Platform;
end;

procedure TEWBaseProjectCreator.SetInitialOptions(const NewProject: IOTAProject);
begin
  // do nothing
end;

end.
