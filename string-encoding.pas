Program StringEncoding;
uses
	viking_file_io,
	viking_types,
	viking_testing,
	viking_utf;

var
	fd             : TFileStream;
	defaultString  : String;
	zAnsiString    : AnsiString;
	zUnicodeString : UnicodeString;
begin
	defaultString := 'Hello world!';
	AddToSystemJournal('Properties for "String" containing text "'+ defaultString +'":');
	AddToSystemJournal('Length(str)    = '+IntToStr(Length(defaultString)));
	AddToSystemJournal('SizeOf(str[1]) = '+IntToStr(SizeOf(defaultString[1])));
	AddToSystemJournal('');
	
	zAnsiString := 'Hello world!';
	AddToSystemJournal('Properties for "AnsiString" containing text "'+ zAnsiString +'":');
	AddToSystemJournal('Length(str)    = '+IntToStr(Length(zAnsiString)));
	AddToSystemJournal('SizeOf(str[1]) = '+IntToStr(SizeOf(zAnsiString[1])));
	AddToSystemJournal('');
	
	zUnicodeString := 'Hello world!';
	AddToSystemJournal('Properties for "UnicodeString" containing text "'+ zUnicodeString +'":');
	AddToSystemJournal('Length(str)    = '+IntToStr(Length(zUnicodeString)));
	AddToSystemJournal('SizeOf(str[1]) = '+IntToStr(SizeOf(zUnicodeString[1])));
	AddToSystemJournal('');
	
	AddToSystemJournal('Now to test string conversion:');
	zAnsiString := defaultString;
	AddToSystemJournal('After "zAnsiString := defaultString": "'+zAnsiString+'"');
	zAnsiString := zUnicodeString;
	AddToSystemJournal('After "zAnsiString := UnicodeString": "'+zAnsiString+'"');
	AddToSystemJournal('');
	
	zAnsiString := 'Hello world!';
	AddToSystemJournal('Properties for "AnsiString" containing text "'+ zAnsiString +'":');
	AddToSystemJournal('Length(str)    = '+IntToStr(Length(zAnsiString)));
	AddToSystemJournal('SizeOf(str[1]) = '+IntToStr(SizeOf(zAnsiString[1])));
	AddToSystemJournal('');
	//fd := OpenForAppending('TestFile.txt');

end.