unit EWNavBar;

interface

uses Classes, EWIntf, EWBase, VCL.Graphics, EWTypes;

type
  TEWNavBar = class;

  TEWNavBarStyle = (nbsDefault, nbsLight, nbsPrimary, nbsSuccess, nbsDark, nbsWarning, nbsDanger);

  TEWNavBarSearchEvent = procedure(Sender: TObject; ASearch: string) of object;



  TEWNavBarSearchOptions = class(TPersistent)
  private
    FNavBar: TEWNavBar;
    FVisible: Boolean;
    FPlaceHolder: string;
    FButtonText: string;
    procedure SetButtonText(const Value: string);
    procedure SetPlaceholder(const Value: string);
    procedure SetVisible(const Value: Boolean);
    procedure Changed;
  public
    constructor Create(ANavBar: TEWNavBar);
    procedure Assign(ASource: TPersistent); override;
  published
    property ButtonText: string read FButtonText write SetButtonText;
    property Placeholder: string read FPlaceHolder write SetPlaceholder;
    property Visible: Boolean read FVisible write SetVisible default False;
  end;

  TEWNavBarItem = class(TCollectionItem)
  private
    FNavBar: TEWNavBar;
    FText: string;
    FEnabled: Boolean;
    FDropdownItems: TStrings;
    procedure SetText(const Value: string);
    procedure Changed;
    procedure SetDropDownItems(const Value: TStrings);
    procedure SetEnabled(const Value: Boolean);
  public
    function GetHtml: string;
    procedure Assign(Source: TPersistent); override;
  published
    constructor Create(Collection: TCollection); override;
    property DropdownItems: TStrings read FDropdownItems write SetDropDownItems;
    property Text: string read FText write SetText;
    property Enabled: Boolean read FEnabled write SetEnabled default True;
  end;

  TEWNavBarItemCollection = class(TCollection)
  private
    FNavBar: TEWNavBar;
  protected
    //nction GetOwner: TPersistent; override;
    function GetItem(Index: Integer): TEWNavBarItem;
    procedure SetItem(Index: Integer; Value: TEWNavBarItem);
  public
    constructor Create(ANavBar: TEWNavBar);
    function Add: TEWNavBarItem;
    function Insert( Index: Integer ): TEWNavBarItem;
    property Items[index: Integer]: TEWNavBarItem read GetItem write SetItem; default;
  end;

  TEWNavBar = class(TEWBaseObject, IEWNavBar)
  private
    FItems: TEWNavBarItemCollection;
    FTitle: string;
    FStyle: TEWNavBarStyle;
    FOnItemClick: TEWNavItemClickEvent;
    FOnBrandClick: TNotifyEvent;
    FOnSearch: TEWNavBarSearchEvent;
    FSearchOptions: TEWNavBarSearchOptions;
    function GetStyleClass: string;
    procedure SetItems(const Value: TEWNavBarItemCollection);
    procedure SetTitle(const Value: string);
    procedure SetStyle(const Value: TEWNavBarStyle);
    procedure SetSearchOptions(const Value: TEWNavBarSearchOptions);
  protected
    procedure DoItemClick(ASender: TObject; AData: string);
    procedure GetEventListners(AListners: TStrings); override;
    procedure BuildCss(AProperties: TStrings); override;
    function GenerateHtml: string; override;
    procedure Paint; override;
    procedure DoEvent(AParams: TStrings); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Items: TEWNavBarItemCollection read FItems write SetItems;
    property Title: string read FTitle write SetTitle;
    property SearchOptions: TEWNavBarSearchOptions read FSearchOptions write SetSearchOptions;
    property Style: TEWNavBarStyle read FStyle write SetStyle default nbsDefault;
    property OnBrandClick: TNotifyEvent read FOnBrandClick write FOnBrandClick;
    property OnItemClick: TEWNavItemClickEvent read FOnItemClick write FOnItemClick;
    property OnSearch: TEWNavBarSearchEvent read FOnSearch write FOnSearch;

  end;

implementation

uses Types, SysUtils, VCL.Controls, Json;

{ TEWNavBar }

procedure TEWNavBar.BuildCss(AProperties: TStrings);
begin
 inherited;
end;

constructor TEWNavBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FItems := TEWNavBarItemCollection.Create(Self);
  FSearchOptions := TEWNavBarSearchOptions.Create(Self);
  Align := alTop;
end;

destructor TEWNavBar.Destroy;
begin
  FItems.Free;
  FSearchOptions.Free;
  inherited;
end;

procedure TEWNavBar.DoEvent(AParams: TStrings);
var
  AIndex: integer;
  ASubIndex: integer;
begin
  inherited;
  if AParams.Values['action'] = 'search' then
  begin

    if Assigned(FOnSearch) then
      FOnSearch(Self, AParams.Values['value']);
    Exit;
  end;

  AIndex := StrToIntDef(AParams.Values['index'], -1);
  ASubIndex := StrToIntDef(AParams.Values['sub-index'], -1);
  if (AIndex = -1) and (Assigned(FOnBrandClick)) then
    FOnBrandClick(Self)
  else
  begin
    if Assigned(FOnItemClick) then
      FOnItemClick(Self, FItems[AIndex], ASubIndex);
  end;
end;

procedure TEWNavBar.DoItemClick(ASender: TObject; AData: string);
var
  AJson: TJSONObject;
  AIndex: integer;
  ASubIndex: integer;
begin
  AJson := TJSONObject.ParseJSONValue(AData) as TJSONObject;
  try
    AIndex := StrToIntDef(AJson.GetValue('index').Value, -1);
    ASubIndex := -1;
    if AJson.GetValue('sub-index') <> nil then
      ASubIndex := StrToIntDef(AJson.GetValue('sub-index').Value, -1);
    if Assigned(FOnItemClick) then
      FOnItemClick(Self, FItems.Items[AIndex], ASubIndex);
  finally
    AJson.Free;
  end;
end;

procedure TEWNavBar.GetEventListners(AListners: TStrings);
var
  AItem: TCollectionItem;
  ICount: integer;
begin
  inherited;
  if Assigned(FOnBrandClick) then
    AddObjectEvent(Name+'Brand', 'click', [], AListners, '');

  if Assigned(FOnSearch) then
  begin
    AddObjectEvent(Name+'SearchButton', 'click', ['action=search'], AListners, 'document.getElementById("'+Name+'SearchInput").value');
    AddObjectEvent(Name+'SearchInput', 'search', ['action=search'], AListners, 'document.getElementById("'+Name+'SearchInput").value');
  end;

  for AItem in FItems do
  begin
    // first add the nav item click...
    AddObjectEvent(Name+'Item'+AItem.Index.ToString, 'click', ['index='+AItem.Index.ToString], AListners, '');

    // now any drop-down menu item clicks...
    for ICount := 0 to TEWNavBarItem(AItem).DropdownItems.Count-1 do
    begin
      AddObjectEvent(Name+'Item'+AItem.ID.ToString+'_SubItem'+ICount.ToString,
                     'click',
                     ['index='+AItem.Index.ToString, 'sub-index='+ICount.ToString],
                     AListners, '');
    end;
  end;

end;

function TEWNavBar.GenerateHtml: string;
var
  AItem: TCollectionItem;
  ASearchBtnStyle: string;
begin
  inherited;
  ASearchBtnStyle := 'light';
  if (FStyle in [nbsDefault, nbsLight]) then ASearchBtnStyle := 'dark';


  Result := '<nav id="'+Name+'" class="navbar navbar-expand-md justify-content-between '+GetStyleClass+'">'+
  '<a id="'+Name+'Brand'+'" class="navbar-brand" href="#">'+FTitle+'</a>'+
  '<button class="navbar-toggler" type="button" data-toggle="collapse" data-target="#'+Name+'Toggler" aria-controls="'+Name+'Toggler" aria-expanded="false" aria-label="Toggle navigation">'+
  '  <span class="navbar-toggler-icon"></span>'+
  '</button>'+

  '<div class="collapse navbar-collapse" id="'+Name+'Toggler">'+

  '<ul class="navbar-nav mr-auto">';
      for AItem in Fitems do
    Result :=Result + TEWNavBarItem(AItem).GetHtml;
    Result := Result + '</ul>';
  if FSearchOptions.Visible then
  begin
    Result := Result + '<form class="form-inline" onsubmit="return false;">'+
      '<input id="'+Name+'SearchInput" class="form-control mr-sm-2" type="search" placeholder="'+FSearchOptions.Placeholder+'" aria-label="Search">'+
      '<button id="'+Name+'SearchButton" class="btn btn-outline-'+ASearchBtnStyle+' my-2 my-sm-0" type="button">'+FSearchOptions.ButtonText+'</button>'+
    '</form>';
  end;
  Result := Result + '<div></nav>';
end;

function TEWNavBar.GetStyleClass: string;
begin
  case FStyle of
    nbsDefault: Result := '';
    nbsLight: Result := 'navbar-light';
    nbsPrimary: Result := 'navbar-dark bg-primary';
    nbsDark: Result := 'navbar-dark bg-dark';
    nbsSuccess: Result := 'navbar-dark bg-success';
    nbsWarning: Result := 'navbar-light bg-warning';
    nbsDanger: Result := 'navbar-dark bg-danger';
  end;
end;

procedure TEWNavBar.Paint;
var
  r: TRect;
  t: TRect;
  s: string;
  ICount: Integer;
  ALastFont: string;
  AFontColor: TColor;
  ABackground: TColor;
begin
  r := ClientRect;
  ABackground := clNone;
  AFontColor := clBlack;
  case FStyle of
    nbsDefault: ABackground := clNone;
    nbsLight: ABackground := clNone;
    nbsPrimary: ABackground := clWebDodgerBlue;
    nbsDark: ABackground := clDkGray;
    nbsSuccess: ABackground := clGreen;
    nbsWarning: ABackground := clWebGold;
    nbsDanger: ABackground := clMaroon;
  end;

  case FStyle of
    nbsDefault: AFontColor := clWebDodgerBlue;
    nbsLight: AFontColor := clGray;
    nbsPrimary: AFontColor := clWhite;
    nbsDark: AFontColor := clWhite;
    nbsSuccess: AFontColor := clWhite;
    nbsWarning: ABackground := clWhite;
    nbsDanger: ABackground := clWhite;
  end;

  Canvas.Font.Color := AFontColor;
  Canvas.Brush.Color := ABackground;

  if ABackground = clNone then
    Canvas.Brush.Style := bsClear;

  Canvas.FillRect(r);

  Canvas.Brush.Style := bsClear;

  canvas.Font.Size := 14;
  t := Rect(0, 0, Canvas.TextWidth(FTitle)+16, Height);

  Canvas.TextRect(t, FTitle, [tfSingleLine, tfVerticalCenter, tfCenter]);
  OffsetRect(t, t.Width, 0);

  canvas.Font.Size := 10;
  for ICount := 0 to FItems.Count-1 do
  begin
    s := TEWNavBarItem(FItems.Items[ICount]).Text;
    t.Width := Canvas.TextWidth(s)+32;
    Canvas.TextRect(t, s, [tfSingleLine, tfVerticalCenter, tfCenter]);
    if TEWNavBarItem(FItems.Items[ICount]).FDropdownItems.Count > 0 then
    begin
      r := t;
      r.Left := r.Right-16;
      ALastFont := Canvas.Font.Name;
      Canvas.Font.Name := 'Webdings';

      s := '6';
      Canvas.TextRect(r, s, [tfSingleLine, tfVerticalCenter, tfCenter]);
      Canvas.Font.Name := ALastFont;
    end;
    OffsetRect(t, t.Width, 0);
  end;
end;

procedure TEWNavBar.SetItems(const Value: TEWNavBarItemCollection);
begin
  FItems.Assign(Value);
end;

procedure TEWNavBar.SetSearchOptions(const Value: TEWNavBarSearchOptions);
begin
  FSearchOptions.Assign(Value);
end;

procedure TEWNavBar.SetStyle(const Value: TEWNavBarStyle);
begin
  if FStyle <> Value then
  begin
    FStyle := Value;
    Changed;
  end;
end;

procedure TEWNavBar.SetTitle(const Value: string);
begin
  if FTitle <> Value then
  begin
    FTitle := Value;
    Changed;
  end;
end;

{ TEWNavBarItem }

procedure TEWNavBarItem.Assign(Source: TPersistent);
begin
  inherited;
  if (Source is TEWNavBarItem) then
  begin
    Text := (Source as TEWNavBarItem).Text;
    Changed;
  end;
end;

procedure TEWNavBarItem.Changed;
begin
  FNavBar.Changed;
end;

constructor TEWNavBarItem.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FDropdownItems := TStringList.Create;
  FNavBar := (Collection as TEWNavBarItemCollection).FNavBar;
  FEnabled := True;
end;


function TEWNavBarItem.GetHtml: string;
var
  ICount: integer;
  AItem: string;
begin
  if FDropdownItems.Count > 0 then
  begin
    Result := '<li class="nav-item dropdown">'+
              '<a class="nav-link dropdown-toggle" href="#" id="navbardrop" data-toggle="dropdown">'+
              FText+
              '</a>'+
              '<div class="dropdown-menu">';
    for ICount := 0 to FDropdownItems.Count-1 do
    begin
      AItem := FDropdownItems[ICount];
      Result := Result + '<a id="'+FNavBar.Name+'Item'+ID.ToString+'_SubItem'+ICount.ToString+'" class="dropdown-item" href="#">'+AItem+'</a>';
    end;
    Result := Result + '</div></li>';
  end
  else
    Result := '<li class="nav-item"><a id="'+FNavBar.Name+'Item'+ID.ToString+'" class="nav-link" href="#">'+FText+'</a></li>'
end;

procedure TEWNavBarItem.SetDropDownItems(const Value: TStrings);
begin
  FDropdownItems.Assign(Value);
  Changed;
end;

procedure TEWNavBarItem.SetEnabled(const Value: Boolean);
begin
  if FEnabled <> Value then
  begin
    FEnabled := Value;
    Changed;
  end;
end;

procedure TEWNavBarItem.SetText(const Value: string);
begin
  if FText <> Value then
  begin
    FText := Value;
    Changed;
  end;
end;

{ TEWNavBarItemCollection }

function TEWNavBarItemCollection.Add: TEWNavBarItem;
begin
  Result := TEWNavBarItem.Create(Self);
end;

constructor TEWNavBarItemCollection.Create(ANavBar: TEWNavBar);
begin
  inherited Create(TEWNavBarItem);
  FNavBar := ANavBar;
end;

function TEWNavBarItemCollection.GetItem(Index: Integer): TEWNavBarItem;
begin
  Result := inherited Items[index] as TEWNavBarItem;
end;
                {
function TEWNavBarItemCollection.GetOwner: TPersistent;
begin
  Result := FNavBar;
end;        }

function TEWNavBarItemCollection.Insert(Index: Integer): TEWNavBarItem;
begin
  Result := inherited insert( index ) as TEWNavBarItem;
end;

procedure TEWNavBarItemCollection.SetItem(Index: Integer;
  Value: TEWNavBarItem);
begin
  inherited SetItem(index, Value);
end;

{ TEWNavBarSearchOptions }

procedure TEWNavBarSearchOptions.Assign(ASource: TPersistent);
begin
  inherited;
  Visible := (ASource as TEWNavBarSearchOptions).Visible;
  PlaceHolder := (ASource as TEWNavBarSearchOptions).Placeholder;
  ButtonText := (ASource as TEWNavBarSearchOptions).ButtonText;
end;

procedure TEWNavBarSearchOptions.Changed;
begin
  FNavBar.Changed;
end;

constructor TEWNavBarSearchOptions.Create(ANavBar: TEWNavBar);
begin
  inherited Create;
  FNavBar := ANavBar;
  FVisible := False;
  FPlaceHolder := 'Search';
  FButtonText := 'Search';
end;

procedure TEWNavBarSearchOptions.SetButtonText(const Value: string);
begin
  if FButtonText <> Value then
  begin
    FButtonText := Value;
    Changed;
  end;
end;

procedure TEWNavBarSearchOptions.SetPlaceholder(const Value: string);
begin
  if FPlaceHolder <> Value then
  begin
    FPlaceHolder := Value;
  end;
end;

procedure TEWNavBarSearchOptions.SetVisible(const Value: Boolean);
begin
  if FVisible <> Value then
  begin
    FVisible := Value;
    Changed;
  end;
end;

end.
