unit EWLayout;

interface

uses Classes, EWIntf, EWBase, Graphics, EWTypes;

type
  TEWLayout = class(TEWBaseObject, IEWSpacer)
  protected
    function DesignTimeCaption: string; override;
    function GetHtml: string; override;
  end;

implementation

uses Types, SysUtils;

{ TBsLabel }

function TEWLayout.DesignTimeCaption: string;
begin
  inherited;
  Result := '';
end;


function TEWLayout.GetHtml: string;
var
  ICount: integer;
begin
  inherited;
  Result := '<div id="'+Name+'" '+GetCss+'>';
  for ICount := 0 to ControlCount-1 do
  begin

  end;

  Result := Reuslt + '</div>';
end;

end.
