unit EWImages;

interface

uses Windows, Classes, EWIntf, EWBase, EWTypes, VCL.Graphics;

type
  TEWImage = class(TEWBaseObject, IEWImage)
  private
    FUrl: string;
    FImageShape: TEWImageShape;
    FPicture: TPicture;
    FTimeStamp: Cardinal;
    procedure SetUrl(const Value: string);
    procedure SetImageShape(const Value: TEWImageShape);
    procedure SetPicture(const Value: TPicture);
  protected
    procedure Changed; override;
    function GetClass: string;
    function GetHtml: string; override;
    procedure Paint; override;
    function GetPreloadUrl: string;
    procedure BuildCss(AProperties: TStrings); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Align;
    property ImageShape: TEWImageShape read FImageShape write SetImageShape default isDefault;
    property Picture: TPicture read FPicture write SetPicture;
    property URL: string read FUrl write SetUrl;
  end;

implementation

uses Types, SysUtils;

{ TEWImage }


procedure TEWImage.BuildCss(AProperties: TStrings);
begin
  inherited;
end;

procedure TEWImage.Changed;
begin
  inherited;
  FTimeStamp := GetTickCount;
end;

constructor TEWImage.Create(AOwner: TComponent);
begin
  inherited;
  FPicture := TPicture.Create;
  FImageShape := isDefault;
end;

destructor TEWImage.Destroy;
begin
  FPicture.Free;
  inherited;
end;

function TEWImage.GetClass: string;
begin
  case FImageShape of
    isDefault: Result := '';
    isRoundedCorners: Result := 'rounded';
    isCircle: Result := 'rounded-circle';
    isThumbnail: Result := 'img-thumbnail';
  end;
end;

function TEWImage.GetHtml: string;
var
  ASrc: string;
begin
  inherited;
  ASrc := FUrl;

  if Assigned(FPicture.Graphic) then
  begin
    if FPicture.Graphic.Empty = False then
     ASrc := '/images?s='+SessionID+'&img='+Name+'&ts='+FTimeStamp.ToString;

  end;
  Result := '';
  Result := '<img  id="'+Name+'" '+GetCss+' style="width:100%;height:auto;"  rel="prefetch" '+
            'class= "'+GetClass+'" src="'+ASrc+'">';
end;

function TEWImage.GetPreloadUrl: string;
begin
  Result := '/images?s='+SessionID+'&img='+Name;
end;

procedure TEWImage.Paint;
var
  r: TRect;
  AStr: string;
begin
  inherited;
  r := ClientRect;
  AStr := DesignTimeCaption;
  Canvas.TextRect(r, AStr, [tfCenter, tfVerticalCenter, tfSingleLine]);
end;


procedure TEWImage.SetImageShape(const Value: TEWImageShape);
begin
  if FImageShape <> Value then
  begin
    FImageShape := Value;
    Changed;
  end;
end;

procedure TEWImage.SetPicture(const Value: TPicture);
begin
  FPicture.Assign(Value);
  if Assigned(FPicture.Graphic) then
    if FPicture.Graphic.Empty = False then
      FUrl := '';
  Changed;
end;

procedure TEWImage.SetUrl(const Value: string);
begin
  if FUrl <> Value then
  begin
    FUrl := Value;
    if FUrl <> '' then
      FPicture.Assign(nil);
    Changed;
  end;
end;

end.
