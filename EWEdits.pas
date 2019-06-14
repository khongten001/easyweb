unit EWEdits
;

interface

uses Classes, VCL.Controls, EWIntf, EWBase, EWTypes;

type
  TEWBaseEdit = class(TEWBaseObject, IEWInput)
  private
    FText: string;
    FPlaceHolder: string;
    FOnExit: TNotifyEvent;
    FOnEnter: TNotifyEvent;
    FOnKeyDown: TEWKeyEvent;
    FOnKeyUp: TEWKeyEvent;

    function GetText: string;
    procedure SetText(const Value: string);
  protected
    procedure GetEventListners(AListners: TStrings); override;
    procedure DoOnEnter;
    procedure DoOnExit;
    function DesignTimeCaption: string; override;

    procedure DoEvent(AParams: TStrings); override;

    procedure DoOnKeyDown(AParams: TStrings);
    procedure DoOnKeyPress(AParams: TStrings);
    procedure DoOnKeyUp(AParams: TStrings);
    procedure DoOnChange(AParams: TStrings); override;
    function GetPlaceHolder: string;
    procedure SetPlaceHolder(const AText: string);
    procedure Paint; override;
    property OnChange;
    property OnKeyDown: TEWKeyEvent read FOnKeyDown write FOnKeyDown;
    property OnKeyUp: TEWKeyEvent read FOnKeyUp write FOnKeyUp;
  public
  published
    property Align;
    property PlaceHolder: string read GetPlaceHolder write SetPlaceHolder;
    property Text: string read GetText write SetText;
    property OnEnter: TNotifyEvent read FOnEnter write FOnEnter;
    property OnExit: TNotifyEvent read FOnExit write FOnExit;

  end;

  TEWEdit = class(TEWBaseEdit)
  private
    FInputType: TEWInputType;
    function GetInputTypeStr: string;
    procedure SetInputType(const Value: TEWInputType);
    function GetTag: string;
  protected
    procedure GetEventListners(AListners: TStrings); override;
    function GenerateHtml: string; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property InputType: TEWInputType read FInputType write SetInputType default itText;
    property OnKeyDown;
    property OnKeyUp;
    property OnChange;
  end;

  TEWMemo = class(TEWBaseEdit)
  private


  protected
    procedure GetEventListners(AListners: TStrings); override;
    function GenerateHtml: string; override;
  published

  end;

  TEWComboBox = class(TEWBaseEdit)
  private
    FItems: TStrings;
    FItemIndex: integer;
    procedure DoChange;
    procedure SetItems(const Value: TStrings);
    procedure SetItemIndex(const Value: integer);
  protected
    procedure GetEventListners(AListners: TStrings); override;
    function GenerateHtml: string; override;
    procedure DoEvent(AParams: TStrings); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

  published
    property Items: TStrings read FItems write SetItems;
    property ItemIndex: integer read FItemIndex write SetItemIndex;
    property OnChange;
  end;

implementation

uses System.Types, VCL.Graphics, SysUtils;

{ TEWBaseEdit }

function TEWBaseEdit.DesignTimeCaption: string;
begin
  Result := Text;
  if Result = '' then
    Result := FPlaceHolder;
  if Result = '' then
    Result := Name;
end;

procedure TEWBaseEdit.DoEvent(AParams: TStrings);
var
  AEvent: string;
begin
  inherited DoEvent(AParams);
  AEvent := AParams.Values['event'];
  if AEvent = 'focus' then DoOnEnter;
  if AEvent = 'blur' then DoOnExit;
  if AEvent = 'keydown' then DoOnKeyDown(AParams);
  if AEvent = 'keyup' then DoOnKeyUp(AParams);
end;

procedure TEWBaseEdit.DoOnChange(AParams: TStrings);
begin
  FText := AParams.Values['value'];
  inherited DoOnChange(AParams);
end;

procedure TEWBaseEdit.DoOnEnter;
begin
  if Assigned(FOnEnter) then
    FOnEnter(Self);
end;

procedure TEWBaseEdit.DoOnExit;
begin
  if Assigned(FOnEnter) then
    FOnExit(Self);
end;

procedure TEWBaseEdit.DoOnKeyDown(AParams: TStrings);
begin
  if Assigned(FOnKeyDown) then
    FOnKeyDown(Self, StrToInt(AParams.Values['value']));
end;

procedure TEWBaseEdit.DoOnKeyPress(AParams: TStrings);
begin
  FText := AParams.Values['value'];
  if Assigned(OnChange) then
    OnChange(Self);
end;

procedure TEWBaseEdit.DoOnKeyUp(AParams: TStrings);
begin
  if Assigned(FOnKeyUp) then
    FOnKeyUp(Self, StrToInt(AParams.Values['value']));
end;

procedure TEWBaseEdit.GetEventListners(AListners: TStrings);
begin
  inherited;
  if Assigned(FOnKeyDown) then AddOnKeyDownEvent(AListners);
  if Assigned(FOnKeyUp) then AddOnKeyUpEvent(AListners);
  if Assigned(FOnEnter) then AddEnterEvent(AListners);
  if Assigned(FOnExit) then AddExitEvent(AListners);
end;



constructor TEWEdit.Create(AOwner: TComponent);
begin
  inherited;
  FInputType := itText;
end;

procedure TEWEdit.GetEventListners(AListners: TStrings);
begin
  inherited;
  if Assigned(OnChange) then AddOnKeyPressEvent(AListners);
end;

function TEWEdit.GenerateHtml: string;
begin
  Result := '<'+GetTag+' id="' + Name + '"'+GetCss+' type="'+GetInputTypeStr+'"'+
            'class="form-control" placeholder="' + FPlaceHolder + '" '+
            'value="' + FText + '" ">';
end;

function TEWEdit.GetInputTypeStr: string;
begin
  case FInputType of
    itDate: Result := 'date';
    itDateTime: Result := 'datetime-local';
    itEmail: Result := 'email';
    itHidden: Result := 'hidden';
    itMonthYear: Result := 'month';
    itNumber: Result := 'number';
    itPassword: Result := 'password';
    itText: Result := 'text';
    itTime: Result := 'time';
    itUrl: Result := 'url';
  end;
end;

function TEWBaseEdit.GetPlaceHolder: string;
begin
  Result := FPlaceHolder;
end;

function TEWBaseEdit.GetText: string;
begin
  Result := FText;
end;

procedure TEWBaseEdit.Paint;
var
  ARect: TRect;
  AText: string;
begin
  Canvas.Pen.Style := psSolid;
  Canvas.Pen.Color := clSilver;
  Canvas.RoundRect(ClientRect, 8, 8);
  Canvas.Font.Size := 11;
  ARect := ClientRect;
  InflateRect(ARect, -6, -6);
  AText := DesignTimeCaption;
  Canvas.TextRect(ARect, AText, [tfVerticalCenter, tfSingleLine]);
end;

function TEWEdit.GetTag: string;
begin
  Result := 'input';
end;

procedure TEWEdit.SetInputType(const Value: TEWInputType);
begin
  if FInputType <> Value then
  begin
    FInputType := Value;
    Changed;
  end;
end;

procedure TEWBaseEdit.SetPlaceHolder(const AText: string);
begin
  if AText <> FPlaceHolder then
  begin
    FPlaceHolder := AText;
    Changed;
  end;
end;

procedure TEWBaseEdit.SetText(const Value: string);
begin
  if FText <> Value then
  begin
    FText := Value;
    Changed;
    if Assigned(OnChange) then
      OnChange(Self);
  end;
end;


{ TEWMemo }

procedure TEWMemo.GetEventListners(AListners: TStrings);
begin
  inherited;
  if Assigned(OnChange) then AddOnKeyPressEvent(AListners);
end;

function TEWMemo.GenerateHtml: string;
begin
  Result := '<textarea id="' + Name + '"'+GetCss+' '+
            'class="form-control" cols="80" rows="5">'+
            FText+
            '</textarea>';
end;


{ TEWComboBox }

constructor TEWComboBox.Create(AOwner: TComponent);
begin
  inherited;
  FItems := TStringList.Create;
end;

destructor TEWComboBox.Destroy;
begin
  FItems.Free;
  inherited;
end;

procedure TEWComboBox.DoChange;
begin
  if Assigned(OnChange) then
    OnChange(Self);
end;

procedure TEWComboBox.DoEvent(AParams: TStrings);
var
  AEvent: string;
begin
  inherited DoEvent(AParams);
  AEvent := AParams.Values['event'];
  if AEvent = 'change' then
  begin
    FItemIndex := StrToIntDef(AParams.Values['value'], -1);
    DoChange;
  end;
end;

procedure TEWComboBox.GetEventListners(AListners: TStrings);
begin
  inherited;
  if Assigned(OnChange) then AddObjectEvent(Name, 'change', [], AListners, 'document.getElementById(''' + Name +''').selectedIndex');
end;

function TEWComboBox.GenerateHtml: string;
var
  ICount: integer;
  ASelected: string;
begin
  inherited;
  Result := '<select class="form-control" id="'+Name+'" '+GetCss+'>';
  for ICount := 0 to FItems.Count-1 do
  begin
    ASelected := '';
    if ICount = FItemIndex then
      ASelected := ' selected';
    Result := Result + '<option'+ASelected+'>'+FItems[ICount]+'</option>';
  end;
  Result := Result + '</select>';
end;

procedure TEWComboBox.SetItemIndex(const Value: integer);
begin
  if FItemIndex <> Value then
  begin
    FItemIndex := Value;
    DoChange;
    Changed;
  end;
end;

procedure TEWComboBox.SetItems(const Value: TStrings);
begin
  FItems.Assign(Value);
  Changed;
end;

end.
