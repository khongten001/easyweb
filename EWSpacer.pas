unit EWSpacer;

interface

uses Classes, EWIntf, EWBase, Graphics, EWTypes;

type
  TEWSpacer = class(TEWBaseObject, IEWSpacer)
  protected
    function DesignTimeCaption: string; override;
    function GetHtml: string; override;
  end;

implementation

uses Types, SysUtils;

{ TBsLabel }

function TEWSpacer.DesignTimeCaption: string;
begin
  inherited;
  Result := '';
end;


function TEWSpacer.GetHtml: string;
begin
  inherited;
  Result := '<div class="'+Name+'" id="'+Name+'" '+GetCss+'></div>';
end;

end.
