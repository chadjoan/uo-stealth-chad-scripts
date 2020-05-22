Program TickTest;
var
	t0 : Integer;
    t1 : Integer;
begin
	t0 := GetTickCount;
    Wait(1000);
    t1 := GetTickCount;
	ClientPrint('t0 == '+IntToStr(t0));
    ClientPrint('t1 == '+IntToStr(t1));
    ClientPrint('dt == '+IntToStr(t1-t0));
    Exit;
 //Body of Script
end.