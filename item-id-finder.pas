Program ItemIdFinder;
uses
	viking_types;
(*
type
	// Signed integers
	int8   = Shortint;
	int16  = Smallint;
	int32  = Integer;
	//int64  = Int64; // Already declared in RemObjects PascalScript
	
	// Unsigned integers
	uint8  = Byte;
	uint16 = Word;
	uint32 = Cardinal;
	//uint64 = UInt64; // TODO: Not sure how to define this in PascalScript

	int8_array  = Array of int8;
	int16_array = Array of int16;
	int32_array = Array of int32;
	int64_array = Array of int64;

	uint8_array  = Array of uint8;
	uint16_array = Array of uint16;
	uint32_array = Array of uint32;
	//uint64_array = Array of uint64;

	string_array = Array of String;
*)
/// timeout_ is in milliseconds. 
function waitForClientTarget(desiredState_ : boolean; timeout_ : int64) : boolean;
var
	approxElapsed : int64; // To get more accuracy we have to watch the wall clock somehow.
begin
	approxElapsed := 0;
	while ClientTargetResponsePresent() <> desiredState_ do
	begin
		if (timeout_ > 0) and (approxElapsed >= timeout_) then
			break;

		wait(100);
		approxElapsed := approxElapsed + 100;
	end;
	if (timeout_ > 0) and (approxElapsed >= timeout_) then
		Result := false
	else
		Result := true;
end;

function int32_array_to_hex_string(theArray_ : int32_array; printWidth_ : int32) : String;
var
	idx    : index_t;
	len    : index_t;
	res    : String;
begin
	len := length(theArray_);
	if len <= 0 then
	begin
		Result := '[]';
		Exit;
	end;

	res := '[$'+IntToHex(theArray_[0], printWidth_);
	for idx := 1 to len-1 do
	begin
		res := res + ',$';
		res := res + IntToHex(theArray_[idx], printWidth_);
	end;
	res := res + ']';
	Result := res;
end;

var
	idx         : index_t;
	itemTypeIds : int32_array;
	itemType    : Word;
	itemId      : Cardinal;
	output      : String;
begin
	idx := 0;
	SetLength(itemTypeIds, 20);
	
	itemId := -1;

	while true do
	begin
		ClientRequestObjectTarget();
		if not waitForClientTarget(false, 1000) then
		begin
			ClientPrint('Item ID finder: Couldn''t bring up targeting cursor (timed out); trying again.');
			continue; 
		end;

		//ClientPrint('before LastTarget() = $'+ IntToHex(LastTarget(),8));
		//ClientPrint('before       itemId = $'+ IntToHex(itemId,8));
		ClientPrint('Click on an object to retrieve its type ID, or cancel targeting to finish.');
		waitForClientTarget(true, 0);
		//ClientPrint('middle LastTarget() = $'+ IntToHex(LastTarget(),8));
		//ClientPrint('middle       itemId = $'+ IntToHex(itemId,8));
		wait(300); // Might not be necessary. But then I'd have to test more. *Shrug*
		//ClientPrint('after LastTarget() = $'+ IntToHex(LastTarget(),8));
		//ClientPrint('after       itemId = $'+ IntToHex(itemId,8));

		if LastTarget() = 0 then
		begin
			ClientPrint('It seems we''re done. Here''s the list of Item Type IDs:');
			SetLength(itemTypeIds, idx);
			output := int32_array_to_hex_string(itemTypeIds, 4);
			ClientPrint(output);
			AddToSystemJournal('Item IDs selected: '+output);
			break;
		end
		else
		begin
			if idx >= length(itemTypeIds) then
				SetLength(itemTypeIds, length(itemTypeIds) * 2);

			itemId := LastTarget();
			itemType := GetType(itemId);
			itemTypeIds[idx] := int32(itemType);
			idx := idx + 1;
			ClientPrint('Item Type ID is '+ IntToHex(itemType,4));
		end;
	end;
end.