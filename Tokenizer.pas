unit Tokenizer;

interface

type
  TDynStrArray = array of string;
  TTokenizer = class
  private
    dsa: TDynStrArray;
    procedure Add(var Arr: TDynStrArray; Value: string);
    function Is1CharOp(S: string): Boolean;
    function Is2CharOp(S: string): Boolean;
  public
    function Tokenize(Str: string): TDynStrArray;
    function TokenizeFile(Filename: string): TDynStrArray;
  end;
  
implementation

uses
  SysUtils, Classes;
  
procedure TTokenizer.Add(var Arr: TDynStrArray; Value: string);
begin
  SetLength(Arr, Length(Arr) + 1);
  Arr[Length(Arr) - 1] := Value;
end;

function TTokenizer.Is1CharOp(S: string): Boolean;
begin
  Result := Pos(S, '+-*/%<>:;()=,.[]') <> 0
end;

function TTokenizer.Is2CharOp(S: string): Boolean;
begin
  Result := Pos(S, ':=|<=|>=|<>') <> 0
end;
  
function TTokenizer.Tokenize(Str: string): TDynStrArray;
var
  Next2Chars: string[2];
  InQuote: Boolean;
  i: Integer;
  Tks: TDynStrArray;
  LastTknPos: Integer;
  Tkn: string;
begin
  i := 1;
  InQuote := False;
  while (Str <> '') do begin                                                    

    Next2Chars := Copy(Str, i, 2);
    if (Next2Chars = '//') then begin

      while ((i < Length(Str)) and (Next2Chars <> #13#10) and (Str[i] <> #13)) do begin
        Inc(i);
        Next2Chars := Copy(Str, i, 2);
      end;
      Delete(Str, 1, i);
      i := 0;
    
    end else if (Str[i] = '''') then begin

      Tkn := Trim(Copy(Str, 1, i - 1));
      if (Tkn <> '') then
        Add(Tks, Tkn);
      Delete(Str, 1, i - 1);
      i := 2;
      Next2Chars := Copy(Str, i, 2);  
      while (i < Length(Str)) do begin
        if (Str[i] = '''') and (Next2Chars <> '''''') then begin
          Break;
        end else begin
          Inc(i);
          Next2Chars := Copy(Str, i, 2);         
        end;
      end;
      if (i < Length(Str)) then begin
        Tkn := Trim(Copy(Str, 1, i));
        if (Tkn <> '') then
          Add(Tks, Tkn);
        Delete(Str, 1, i);
        InQuote := False;
        i := 0;
      end;
    
    end else if (Is2CharOp(Next2Chars)) then begin
    
      Tkn := Trim(Copy(Str, 1, i - 1));
      if (Tkn <> '') then
        Add(Tks, Tkn);
      Add(Tks, Next2Chars);  
      Delete(Str, 1, i + 1);
      i := 0;
      
    end else if (Is1CharOp(Str[i])) then begin
    
      Tkn := Trim(Copy(Str, 1, i - 1));
      if (Tkn <> '') then
        Add(Tks, Tkn);
      Add(Tks, Str[i]);
      Delete(Str, 1, i);
      
      i := 0;
      
    end else if ((Str[i] = ' ') or (Next2Chars = #13#10) or (Str[i] = #13)) then begin
    
      Tkn := Trim(Copy(Str, 1, i - 1));
      if (Tkn <> '') then
        Add(Tks, Tkn);
      if (Str[i] = ' ') then
        Delete(Str, 1, i)
      else  
        Delete(Str, 1, i + 1);
      i := 0;

    end;
    Inc(i);
    
    if (i > Length(Str)) then begin
      Break;
    end;
  end;
  
  if (Str <> '') then
    Add(Tks, Str);
  Result := Tks;
end;

function TTokenizer.TokenizeFile(Filename: string): TDynStrArray;
var
  List: TStringList;
begin
  if (FileExists(Filename)) then begin
    List := TStringList.Create;
    try
      List.LoadFromFile(Filename);
      Result := Tokenize(List.Text);
    finally
      List.Free;
    end;
  end;
end;

end.

