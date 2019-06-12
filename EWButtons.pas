unit EWButtons;

interface

uses Windows, Classes, EWIntf, EWBase, EWTypes;

type
  TEWButton = class(TewBaseObject, IEWButton)
  private
    FButtonType: TewButtonType;
    FBorderRadius: integer;
    FText: string;
    function GetButtonType: TewButtonType;
    function GetButtonTypeStr: string;
    procedure SetButtonType(const Value: TewButtonType);
    function GetText: string;
    procedure SetText(const Value: string);
    procedure SetBorderRadius(const Value: integer);
  protected
    function GetHtml: string; override;
    function DesignTimeCaption: string; override;
    procedure Paint; override;
    procedure BuildCss(AProperties: TStrings); override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Align;
    property ButtonType: TewButtonType read GetButtonType write SetButtonType default btSecondary;
    property Text: string read GetText write SetText;
    property BorderRadius: integer read FBorderRadius write SetBorderRadius default 0;
  end;

  TEWDropDown = class(TEWButton, IEWBaseObjectItemClickable)
  private
    FItems: TStrings;
    FItemIndex: integer;
    FOnItemClick: TEWClickItemEvent;
    function GetItems: TStrings;
    function GetOnItemClick: TEWClickItemEvent;
    procedure SetOnItemClick(Value: TEWClickItemEvent);
    procedure SetItems(const Value: TStrings);
    function GetItemIndex: integer;
    procedure SetItemIndex(Value: integer);
    procedure OnItemsChanged(Sender: TObject);
  protected

    procedure GetEventListners(AListners: TStrings); override;
    function GetHtml: string; override;
    procedure BuildCss(AProperties: TStrings); override;
    property ItemIndex: integer read GetItemIndex write SetItemIndex;
    procedure DoEvent(AParams: TStrings); override;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Items: TStrings read GetItems write SetItems;
    property OnItemClick: TEWClickItemEvent read GetOnItemClick write SetOnItemClick;
  end;

  TEWButtonGroup = class(TEWButton, IEWBaseObjectItemClickable)
  private
    FItems: TStrings;
    FOnItemClick: TEWClickItemEvent;
    FItemIndex: integer;
    function GetItems: TStrings;
    function GetOnItemClick: TEWClickItemEvent;
    procedure SetOnItemClick(Value: TEWClickItemEvent);
    procedure SetItems(const Value: TStrings);
    function GetItemIndex: integer;
    procedure SetItemIndex(Value: integer);
  protected
    procedure DoClick(AParams: TStrings); override;
    function GetHtml: string; override;
    procedure DoItemClick(ASender: TObject; AItem: string; AIndex: integer);
    procedure GetEventListners(AListners: TStrings); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Items: TStrings read GetItems write SetItems;
    property ItemIndex: integer read GetItemIndex write SetItemIndex;
    property OnItemClick: TEWClickItemEvent read GetOnItemClick write SetOnItemClick;
  end;

implementation

uses Types, VCL.Graphics, SysUtils;


{ TEWButton }

procedure TEWButton.BuildCss(AProperties: TStrings);
begin
  inherited;
  if FBorderRadius > 0 then AProperties.Values['corner-radius'] := BorderRadius.ToString+'px';
end;

constructor TEWButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FButtonType := btSecondary;
end;

function TEWButton.DesignTimeCaption: string;
begin
  Result := Text;
end;

function TewButton.GetButtonType: TewButtonType;
begin
  Result := FButtonType;
end;

function TEWButton.GetButtonTypeStr: string;
begin
  case FButtonType of
    btBasic:      Result := 'btn';
    btDefault:    Result := 'btn btn-default';
    btPrimary:    Result := 'btn btn-primary';
    btSecondary:  Result := 'btn btn-secondary';
    btSuccess:    Result := 'btn btn-success';
    btDanger:     Result := 'btn btn-danger';
    btWarning:    Result := 'btn btn-warning';
    btInfo:       Result := 'btn btn-info';
    btLight:      Result := 'btn btn-light';
    btDark:       Result := 'btn btn-dark';
    btLink:       Result := 'btn btn-link';
  end;
end;

function TEWButton.GetHtml: string;
begin
  inherited;
  Result := '<div name="' + Name + '" id="'+Name+'" '+GetCss+'><button style="height:100%;width:100%;"  type="button" class="' +GetButtonTypeStr + '">' + FText + '</button></div>';
end;

function TEWButton.GetText: string;
begin
  Result := FText;
  if Result = '' then
    Result := Name;
end;

procedure TEWButton.Paint;
var
  ARect: TRect;
  AText: string;
begin
  Canvas.Brush.Color := clSilver;
  Canvas.Font.Color := clWhite;
  Canvas.Font.Size := 11;
  Canvas.Font.Name := 'Arial';
  if FButtonType = btPrimary then Canvas.Brush.Color := $FF7B00;
  if FButtonType = btSecondary then Canvas.Brush.Color := $7D756C;
  if FButtonType = btSuccess then Canvas.Brush.Color := $45A728;
  if FButtonType = btDanger then Canvas.Brush.Color := $4535DC;
  if FButtonType = btWarning then Canvas.Brush.Color := $07C1FF;
  if FButtonType = btInfo then Canvas.Brush.Color := $B8A217;
  if FButtonType = btDark then Canvas.Brush.Color := $403A34;

  if FButtonType = btLight then
  begin
    Canvas.Brush.Color := $FAF9F8;
    Canvas.Font.Color := clBlack;
  end;
  if FButtonType = btLink then
  begin
    Canvas.Brush.Color := $FFFFFF;
    Canvas.Font.Color := clWebDodgerBlue;
  end;

  if FButtonType = btSuccess then
  begin
    Canvas.Brush.Color := clWebForestGreen;
    Canvas.Font.Color := clWhite;
  end;

  Canvas.Pen.Style := psClear;
  Canvas.RoundRect(ClientRect, 6, 6);
  ARect := ClientRect;
  AText := Text;

  Canvas.TextRect(ARect, AText, [tfCenter, tfVerticalCenter, tfSingleLine]);
end;

procedure TEWButton.SetButtonType(const Value: TewButtonType);
begin
  if FButtonType <> Value then
  begin
    FButtonType := Value;
    Changed;
  end;
end;

procedure TEWButton.SetBorderRadius(const Value: integer);
begin
  if FBorderRadius <> Value then
  begin
    FBorderRadius := Value;
    Changed;
  end;
end;

procedure TEWButton.SetText(const Value: string);
begin
  if FText <> Value then
  begin
    FText := Value;
    Changed;
  end;
end;

{ TEWDropDown }

procedure TEWDropDown.BuildCss(AProperties: TStrings);
begin
  inherited;

end;

constructor TEWDropDown.Create(AOwner: TComponent);
begin
  inherited;
  FItems := TStringList.Create;
  FItemIndex := -1;
  TStringLisT(FItems).OnChange := OnItemsChanged;
end;

destructor TEWDropDown.Destroy;
begin
  FItems.Free;
  inherited;
end;

procedure TEWDropDown.DoEvent(AParams: TStrings);
var
  AIndex: integer;
begin
  inherited;
  if Assigned(FOnItemClick) then
  begin
    AIndex := StrToInt(AParams.Values['index']);
    FOnItemClick(Self, FItems[AIndex], AIndex);
  end;
end;

procedure TEWDropDown.GetEventListners(AListners: TStrings);
var
  ICount: integer;
begin
  inherited;
  if Assigned(FOnItemClick) then
  begin
    for ICount := 0 to FItems.Count-1 do
      AddObjectEvent(Name+'Item'+ICount.ToString, 'click', ['index='+ICount.ToString], AListners, '');
  end;
end;

function TEWDropDown.GetHtml: string;
var
  ICount: integer;
begin
  inherited;
  Result := '<div id="'+Name+'" '+GetCss+'button class="' +
    GetButtonTypeStr +
    ' dropdown-toggle" style="width:100%;height:100%;" type="button" data-toggle="dropdown" aria-haspopup="true" aria-expanded="false">'
    + '  ' + Text + '</button>'+
    '<div class="dropdown-menu" aria-labelledby="dropdownMenuButton">';
  for ICount := 0 to FItems.Count - 1 do
    Result := Result + '<a class="dropdown-item" id="'+Name+'Item'+ICount.ToString+'" href="#">' + FItems[ICount] + '</a>';
  Result := Result + '</div></div>';
end;

function TEWDropDown.GetItemIndex: integer;
begin
  Result := FItemIndex;
end;

function TEWDropDown.GetItems: TStrings;
begin
  Result := FItems;
end;

function TEWDropDown.GetOnItemClick: TEWClickItemEvent;
begin
  Result := FOnItemClick;
end;

procedure TEWDropDown.OnItemsChanged(Sender: TObject);
begin
  Changed;
end;

procedure TEWDropDown.SetItemIndex(Value: integer);
begin
  //
end;

procedure TEWDropDown.SetItems(const Value: TStrings);
begin
  FItems.Assign(Value);
  Changed;
end;

procedure TEWDropDown.SetOnItemClick(Value: TEWClickItemEvent);
begin
  FOnItemClick := Value;
end;

{ TEWButtonGroup }



constructor TEWButtonGroup.Create(AOwner: TComponent);
begin
  inherited;
  FItems := TStringList.Create;
end;

destructor TEWButtonGroup.Destroy;
begin
  FItems.Free;
  inherited;
end;

procedure TEWButtonGroup.DoClick(AParams: TStrings);
var
  AIndex: integer;
begin
  inherited;
  AIndex := StrToIntDef(AParams.Values['index'], -1);
  if AIndex <> FItemIndex then
  begin
    FItemIndex := AIndex;
    Changed;
  end;
  if (AIndex >= 0) and (Assigned(FOnItemClick)) then
    FOnItemClick(Self, Items[AIndex], AIndex);

end;

procedure TEWButtonGroup.DoItemClick(ASender: TObject; AItem: string;
  AIndex: integer);
begin
  FItemIndex := AIndex;
  Changed;
  if FItemIndex <> AIndex then
  begin
    if Assigned(FOnItemClick) then
      FOnItemClick(ASender, AItem, AIndex);
  end;
end;

procedure TEWButtonGroup.GetEventListners(AListners: TStrings);
var
  ICount: integer;
begin
  inherited;
  for ICount := 0 to FItems.Count-1 do
    AddObjectEvent(Name+'Item'+ICount.ToString, 'click', ['index='+ICount.ToString], AListners, '');
    //AddClickItemEvent(ICount, AListners);
end;

function TEWButtonGroup.GetHtml: string;
var
  ICount: integer;
  AActive: string;
begin
  inherited;
  Result :='<div ' + GetCss +' id="'+Name+'" style="width:100%;height:100%;" class="btn-group" role="group" aria-label="Basic example">';
  for ICount := 0 to FItems.Count - 1 do
  begin
    AActive := '';
    if ICount = FItemIndex then
      AActive := ' active ';
    Result := Result + #13#10+'  <button type="button" id="'+Name+'Item'+ICount.ToString+'" style="flex:1;box-shadow: none" class="' + GetButtonTypeStr + ''+ AActive +'">' + FItems[ICount] +'</button>';
  end;

  Result := Result + '</div>';
end;

function TEWButtonGroup.GetItemIndex: integer;
begin
  Result := FItemIndex;
end;

function TEWButtonGroup.GetItems: TStrings;
begin
  Result := FItems;
end;

function TEWButtonGroup.GetOnItemClick: TEWClickItemEvent;
begin
  Result := FOnItemClick;
end;

procedure TEWButtonGroup.SetItemIndex(Value: integer);
begin
  FItemIndex := Value;
end;

procedure TEWButtonGroup.SetItems(const Value: TStrings);
begin
  FItems.Assign(Value);
end;

procedure TEWButtonGroup.SetOnItemClick(Value: TEWClickItemEvent);
begin
  FOnItemClick := Value;
end;


end.

