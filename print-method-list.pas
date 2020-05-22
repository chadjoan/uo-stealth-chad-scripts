Program PrintMethodList;
type
	ArrayOfString = array of String;
var
	ret : ArrayOfString;
begin
	PrintScriptMethodsList('method_list.txt', true);
end.