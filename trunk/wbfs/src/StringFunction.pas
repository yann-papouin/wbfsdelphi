unit StringFunction;

interface

uses
  Variants;

function SFRight(substr: string; s: string): string;
function SFRightRight(substr: string; s: string): string;
function SFLeft(substr: string; s: string): string;
function SFLeftFromLast(substr: string; s: string): string;
function SFCountSubstr(substr: string; s: string): integer;
function SFNRight(substr: string; s: string;n:integer): string;
function SFNLeft(substr: string; s: string;n:integer): string;
function SFLeftNRight(substr: string; s: string;n:integer): string;

function VariantToString(AVar: OleVariant): string;

implementation

function SFRight(substr: string; s: string): string;            // Right
begin
  if pos(substr,s)=0 then result:='' else
    result:=copy(s, pos(substr, s)+length(substr), length(s)-pos(substr, s)+length(substr));
end;

function SFRightRight(substr: string; s: string): string;      // RightLast
{============================================================================}
{ fonction qui renvoie la sous chaine de caractere situee a droite de la sous}
{ chaine substr situee la plus a droite                                      }
{ ex: si substr = '\' et S= 'truc\tr\essai.exe droiteDroite renvoie essai.exe}
{============================================================================}
begin
  Repeat
    S:=SFRight(substr,s);
  until pos(substr,s)=0;
  result:=S;
end;

function SFLeft(substr: string; s: string): string;           // Left
{============================================================================}
{ fonction qui renvoie la sous chaine de caractere situee à gauche de la sous}
{ chaine substr                                                              }
{ ex: si substr = '\' et S= 'truc\tr\essai.exe' gauche renvoie truc          }
{============================================================================}
begin
  result:= copy(s, 1, pos(substr, s)-1);
  //result:= substr+S; //copy(s, 1, pos(substr, s)-1);
end;

function SFLeftFromLast(substr: string; s: string): string;    // LeftLast
{============================================================================}
{ fonction qui renvoie la sous chaine de caractere situee a gauche de la     }
{ derniere sous chaine substr                                                }
{ ex: si substr = '\' et S= 'truc\tr\essai.exe' gauche renvoie truc\tr       }
{============================================================================}
var
 s1:string;
 i:integer;
begin
  s1:='';
  for i:=1 to SFCountSubstr(substr, s)-1 do
  begin
    s1:=s1+SFLeft(substr,s)+substr;
    s:=SFRight(substr,s);
  end;
  s1:=s1+SFLeft(substr,s);
  result:=s1;
end;

function SFCountSubstr(substr: string; s: string): integer;     //Count
{==================================================================================}
{ renvoie le nombre de fois que la sous chaine substr est presente dans la chaine S}
{==================================================================================}
begin
  result:=0;
  while pos(substr,s)<>0 do
  begin
    S:=SFRight(substr,s);
    inc(result);
  end;
end;

function SFNRight(substr: string; s: string;n:integer): string;   //RightN
{==============================================================================}
{ renvoie ce qui est a droite de la n ieme sous chaine substr de la chaine S   }
{==============================================================================}
var i:integer;
begin
  for i:=1 to n do
  begin
    S:=SFRight(substr,s);
  end;
  result:=S;
end;

function SFNLeft(substr: string; s: string;n:integer): string;   //LeftN
{==============================================================================}
{ renvoie ce qui est a gauche de la n ieme sous chaine substr de la chaine S   }
{==============================================================================}
var i:integer;
begin
  for i:=1 to SFCountSubstr(substr,s) - n +1 do
  begin
    S:=SFLeftFromLast(substr,s);
  end;
  result:=S;
end;

function SFLeftNRight(substr: string; s: string;n:integer): string; //Extract
{==============================================================================}
{ renvoie ce qui est a gauche de la droite de la n ieme sous chaine substr     }
{ de la chaine S                                                               }
{ ex : GaucheNDroite('\','c:machin\truc\essai.exe',1) renvoie 'truc'           }
{ Permet d'extraire un a un les elements d'une chaine séparés par un separateur}
{==============================================================================}
var i:integer;
begin
  S:=S+substr;
  for i:=1 to n do
  begin
    S:=copy(s, pos(substr, s)+length(substr), length(s)-pos(substr, s)+length(substr));
  end;
  result:=copy(s, 1, pos(substr, s)-1);
end;


function VariantToString(AVar: OleVariant): string;
var
  i: integer;
  V: olevariant;
begin
  Result := '';
  if VarType(AVar) = (varVariant or varByRef) then
     V := Variant(TVarData(AVar).VPointer^)
  else V := AVar;

  if VarType(V) = (varByte or varArray) then
      try
        for i:=VarArrayLowBound(V,1) to VarArrayHighBound(V,1) do
           Result := Result + Chr(Byte(V[i]));
      except;
      end
    else Result := V;
end;

end.
