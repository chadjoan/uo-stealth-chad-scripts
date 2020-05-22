Program ServUOSkillsTrainer;

// Forward-declared constants that are needed by the configuration section.
const
	// A list of skills, for your reference.
	// Set SKILL_TO_TRAIN to one of these values.
	SKILL_ANATOMY     = 'Anatomy';
	SKILL_ANIMAL_LORE = 'Animal Lore';
	SKILL_EVAL_INT    = 'Evaluating Intelligence';
	SKILL_HIDING      = 'Hiding';

	// Example target types.
	TYPE_EAGLE          = $0005;
	TYPE_MAGPIE         = $0006;
	TYPE_CROW           = $0006;
	TYPE_RAVEN          = $0006;
	TYPE_TROPICAL_BIRD  = $0006;
	TYPE_DIRE_WOLF      = $0017;
	TYPE_GREY_WOLF      = $0019;
	TYPE_GORILLA        = $001D;
	TYPE_MONGBAT        = $0027;
	TYPE_SNAKE          = $0034;
	TYPE_COUGER         = $003F;
	TYPE_MOUNTAIN_GOAT  = $0058;
	TYPE_BROWN_BEAR     = $00A7;
	TYPE_HORSE          = $00C8;
	TYPE_CAT            = $00C9;
	TYPE_PIG            = $00CB;
	TYPE_HORSE_DKBROWN  = $00CC;
	TYPE_RABBIT         = $00CD;
	TYPE_SHEEP          = $00CF;
	TYPE_CHICKEN        = $00D0;
	TYPE_GOAT           = $00D1;
	TYPE_BLACK_BEAR     = $00D3;
	TYPE_GRIZZY_BEAR    = $00D4;
	TYPE_PANTHER        = $00D6;
	TYPE_DOG            = $00D9;
	TYPE_FOREST_OSTARD  = $00DB; // yellow?
	TYPE_LLAMA          = $00DC;
	TYPE_RIDABLE_LLAMA  = $00DC;
	TYPE_COW_BLACK      = $00D8;
	TYPE_SHEEP_SHORN    = $00DF;
	TYPE_TIMBER_WOLF    = $00E1;
	TYPE_HORSE_WHITE    = $00E2;
	TYPE_HORSE_BROWN    = $00E4;
	TYPE_COW_BROWN      = $00E7;
	TYPE_BULL           = $00E8;
	TYPE_GREAT_HART     = $00EA;
	TYPE_HIND           = $00ED;
	TYPE_RAT            = $00EE;
	TYPE_BOAR           = $0122;
	TYPE_HUMAN_MALE     = $0190;
	TYPE_HUMAN_FEMALE   = $0191;
	TYPE_CHICKEN_LIZARD = $02CC;


// ===== Configuration section ======
const
	// Which skill to use when macroing.
	SKILL_TO_TRAIN = SKILL_HIDING;

	// How long to wait between attempts to use the skill.
	// Measured in milliseconds (e.g. 1000 = one second).
	SKILL_COOLDOWN = 1000;

	// If the script detects the target cursor being up when it didn't invoke
	// the skill being trained, this is how long it will wait for the cursor
	// to be used.
	// Make this too short, and this script could steal targeting from other
	// scripts or from your own actions.
	// Make this too long, and the script will stall for that long whenever 
	// the script actually did initiate the cursor (ex: because there was lag
	// and the cursor took long enough to appear that it seemed like the skill
	// just didn't get used).
	TARGET_CURSOR_TIMEOUT = 4000;

	// Sometimes you might be able to equip a weapon and melee your targets
	// to death. If the spawn rate is fast enough, this is a viable strategy
	// for ensuring a continuous supply of fresh targets that have not
	// triggered ServUO's anti-macro detection algorithm. This script will
	// not murder things right away, but will wait until it has received all
	// possible gains from a target before murdering it. The point is to
	// be sociopathic, not sadistic ;)
	// To avoid friendly-fire situations, the macro will only murder things
	// that are on the MURDERABLE_TYPES list, and only when they have gray
	// (attackable) notoriety.
	COMMIT_MURDER = false;

	// Comma-separated list of mobile (NPC+creature) types that are allowed
	// to be murdered when COMMIT_MURDER=true. 
	// You can find the type of what you want to murder by visiting one of it
	// in-game and looking through all of the entries in Stealth's "World" tab
	// (each row is an 8-digit hex number like 0xAB01CD02) to find the one that
	// matches your example victim, then look for the "Type" field in the
	// "Object Info" panel to the right of the list of those hex numbers.
	// The type values should be some 4-digit hex number (albeit with Pascal's
	// dollar-sign notation instead of the C-style 0x notation).
	// For example, male human NPCs would have a Type of $0190.
	MURDERABLE_TYPES = '$00CF'; // Ex: '$00CF,$0058';

	// This is the number of times you can gain at a location
	// before the server's anti-macro code kicks in.
	// See ServUO's Scripts/Misc/SkillCheck.cs
	// Set this to 0 to have your character trace out the route it will take
	// (albeit with 1 skill use at each waypoint).
	GAIN_ALLOWANCE = 3;

	// Number of seconds that we must wait before a location or target becomes
	// eligible for gains again (after we have gained enough times at that
	// spot/target to trigger the server's anti-macro code).
	GAIN_COOLDOWN_SECONDS = 300;

	// This overrides the behavior of the "GAIN_ALLOWANCE" constant and dictates
	// that targets shall be switched after this many skill /uses/ (regardless
	// of success, failure, or skill gain). A value of 0 turns this feature off.
	// This is mostly useful for debugging, unless the shard does anti-macro
	// code differently from default ServUO.
	USE_ALLOWANCE = 0;

	// How far (in tiles) your character can use the skill being trained.
	SIGHT_RANGE = 8;

	// Cooldown for simple skills like hiding, in milliseconds.
	SIMPLE_SKILL_COOLDOWN = 10000;

	// ROUTE_LEN:
	//
	// Number of points in the anti-anti-macro route for movement based
	// skills like hiding and (TODO) magery.
	// This number should be based on the GAIN_ALLOWANCE, GAIN_COOLDOWN_SECONDS,
	// and SIMPLE_SKILL_COOLDOWN constants.
	// For example, default ServUO has these set like so:
	//     GAIN_ALLOWANCE = 3
	//     GAIN_COOLDOWN_SECONDS = 300
	//     SIMPLE_SKILL_COOLDOWN = 10000
	//
	// We assume the worst-case scenario for optimization (though it's great
	// for the player) : every skill use (regardless failure/success) causes
	// a skill gain. So with a GAIN_ALLOWANCE of 3 and a cooldown of 10 seconds,
	// it will take at least 30 seconds to exhaust a location's gain potential.
	// We need enough locations in our route to do that 30 seconds over and over
	// until we've reached GAIN_COOLDOWN_SECONDS (300) without wrapping around
	// into locations we already visited. That means 300/30, which is 10.
	// Hence for those parameters, the ROUTE_LEN should be 10.
	// The formulaic form goes like this:
	// ROUTE_LEN = GAIN_COOLDOWN_SECONDS / (GAIN_ALLOWANCE *
	//                   (SIMPLE_SKILL_COOLDOWN ms * 1 second / 1000 ms)) 
	//
	// Normally I would have the computer do this for us, but there's another
	// factor involved: if you change this number, you'll have to edit the
	// route that's defined further down in the script. Manual calculation
	// also gives you the option to use a smaller route but risk losing gains,
	// or use a larger route just to make life harder on yourself.
	ROUTE_LEN = 10;

// ===== END Configuration section ======

type
	Vector2D = record
		x : Integer; // + = east,  - = west
		y : Integer; // + = south, - = north
	end;
	
	ArrayOfCardinal = array of Cardinal;
	ArrayOfWord     = array of Word;

function Vec2D(_x : Integer; _y : Integer) : Vector2D;
var
	result_Vec2D : Vector2D;
begin
	result_Vec2D.x := _x;
	result_Vec2D.y := _y;
	Result := result_Vec2D;
end;

function vec2D_add(_a : Vector2D; _b : Vector2D) : Vector2D;
var
	x, y : Integer;
begin
	x := _a.x + _b.x;
	y := _a.y + _b.y;
	Result := Vec2D(x,y);
end;

function vec2D_toString(_vec : Vector2D) : String;
begin
	Result := '(' + 
IntToStr(_vec.x) + ',' + IntToStr(_vec.y) + ')';
end;

function IntToStrWithPlus(_int : Integer) : String;
begin
	if _int >= 0 then
		Result := '+' + IntToStr(_int)
	else
		Result := IntToStr(_int);
end;

function wordArrayContains(_array : ArrayOfWord; _value : Word) : boolean;
var
	idx : Integer;
begin
	for idx := Low(_array) to High(_array) do
	begin
		if _value = _array[idx] then
		begin
			Result := true;
			Exit;
		end;
	end;
	Result := false; 
end;

function charToStrEsc(_ch : Char) : String;
var
	codePoint : Integer;
begin
	codePoint := Ord(_ch);
	if (codePoint < 32) then // non-printable character.
		Result := '\x'+IntToHex(codePoint,2)
	else
		Result := _ch;
end;

function isWhitespace(_ch : Char) : Boolean;
begin
	Result := ((_ch = ' ') OR (_ch = chr(9)) OR (_ch = chr(10)) OR (_ch = chr(13)));
end;

function skipWhitespace(_text : String; _pos : Integer; _tlen : Integer ) : Integer;
var
	pos : Integer;
begin
	pos := _pos;
	while pos <= _tlen do
	begin
		if not isWhitespace(_text[pos]) then
			break;

		pos := pos + 1;
	end;
	Result := pos;
end;

function tokenizeHex(_text : String; _pos : Integer; _tlen : Integer ) : String;
var
	pos    : Integer;
	hexStr : String;
	ch     : Char;
begin
	pos := _pos;

	// Skip the Pascal-style $ hex prefix, if present.
	if (pos <= _tlen) AND (_text[pos] = '$') then
		pos := pos + 1
	else
	// Skip the C-style 0x hex prefix, if present.
	if  (pos+0 <= _tlen) AND (_text[pos+0] = '0')
	AND (pos+1 <= _tlen) AND (_text[pos+1] = 'x') then
		pos := pos + 2;

	while (pos <= _tlen) do
	begin
		ch := _text[pos];

		if (('0' <= ch) AND (ch <= '9'))
		OR (('A' <= ch) AND (ch <= 'F'))
		OR (('a' <= ch) AND (ch <= 'f'))
		then
		begin
			hexStr := hexStr + ch;
			pos := pos + 1;
			continue;
		end;

		if isWhitespace(ch) OR (ch = ',') OR (Ord(ch) = 0) then
			break;

		RaiseException(erException, 'Error: Invalid character "'+charToStrEsc(ch)+'"'
			+' at position '+IntToStr(pos)+' in text "'+_text+'".');
	end;
	Result := hexStr;
end; 

function parseWordArray(_text : String) : ArrayOfWord;
var
	pos    : Integer;
	tlen   : Integer;
	count  : Integer;
	idx    : Integer;
	elems  : ArrayOfWord;
	hexStr : String;
	ch     : Char;
begin
	count := 0;
	tlen  := length(_text);
	for pos := 1 to tlen do
		if _text[pos] = ',' then
			count := count + 1;

	// Ex: '1234,ABCD,7890' -> 2 commas -> 3 elements
	SetLength(elems, count+1);

	idx := 0;
	pos := 1;
	while pos <= tlen do
	begin
		pos := skipWhitespace(_text, pos, tlen);

		hexStr := tokenizeHex(_text, pos, tlen);
		pos := pos + length(hexStr)+1;
		elems[idx] := StrToInt('$'+hexStr);
		idx := idx + 1;

		pos := skipWhitespace(_text, pos, tlen);

		if (pos <= tlen) then
		begin
			ch := _text[pos];
			if (Ord(ch) = 0) then
				break // Null terminator character.
			else if (_text[pos] = ',') then
				pos := pos + 1
			else
				RaiseException(erException,
					'Error: Unexpected character "'+charToStrEsc(ch)+'"'+
					' at position '+IntToStr(pos)+' in text "'+_text+'".');
		end;
	end;

	Result := elems;
end;

// Used for testing the sorting algorithm.
procedure mkCardinalArray(var _array : TCardinalDynArray; e1, e2, e3, e4 : Cardinal);
begin
	_array[0] := e1;
	_array[1] := e2;
	_array[2] := e3;
	_array[3] := e4;
end;

function cmpCardinalArray(
	var _array1 : TCardinalDynArray; var _array2 : TCardinalDynArray) : Integer;
var
	idx, offset : Integer;
begin
	if length(_array1) > length(_array2) then
		Result := 1
	else if length(_array1) < length(_array2) then
		Result := -1
	else
	begin
		offset := Low(_array2) - Low(_array1);
		for idx := Low(_array1) to High(_array1) do
		begin
			if _array1[idx] > _array2[idx+offset] then
				Result := 1
			else if _array1[idx] < _array2[idx+offset] then
				Result := -1
			else
				Result := 0;
		end;
	end;
end;

function cardinalArrayToString(var _toPrint : TCardinalDynArray) : String;
var
	idx     : Integer;
	buffer  : String;
begin
	buffer := '[';
	if length(_toPrint) > 0 then
		buffer := buffer + '$' + IntToHex(_toPrint[0],8);
	for idx := 1 to length(_toPrint)-1 do
	begin
		buffer := buffer + ',$' + IntToHex(_toPrint[idx],8);
	end;
	buffer := buffer + ']';
	Result := buffer;
end;

/// Use only on small arrays.
/// O(n^2) runtime, but it's easy to write and good on small arrays.
/// _lo and _hi specify that only a range of elements within the array is
/// to be sorted.
/// The version without _lo and _hi will default to sorting the entire array.
procedure bubbleSortCardinalEx(var _toSort : TCardinalDynArray; _lo, _hi : Integer);
var
	outerIdx   : Integer;
	innerIdx   : Integer;
	lo, hi     : Integer;
	minElemIdx : Integer;
	minElem    : Cardinal;

begin
	for outerIdx := _lo to _hi-1 do
	begin
		//findMinElem();
		// Discover which element should be placed at outerIdx.
		minElem    := _toSort[outerIdx];
		minElemIdx := outerIdx;

		for innerIdx := outerIdx+1 to _hi do
		begin
			if _toSort[innerIdx] > minElem then
				continue;

			minElem    := _toSort[innerIdx];
			minElemIdx := innerIdx;
		end;

		// If the minimum value is already at outerIdx ...
		if outerIdx = minElemIdx then
			continue; // ... then it is already placed correctly.

		// Swap to place next lowest element.
		_toSort[minElemIdx] := _toSort[outerIdx];
		_toSort[outerIdx]   := minElem;
	end;
end;

procedure bubbleSortCardinal(var _toSort : TCardinalDynArray);
begin
	bubbleSortCardinalEx(_toSort, Low(_toSort), High(_toSort));
end;

procedure _assert( _expr : boolean; _which : Integer );
begin
	if _expr then
		Exit
	else
		RaiseException(erException, 'Assertion #'+ IntToStr(_which)+ ' triggered');
end;

procedure testBubbleSortCardinal();
var
	actual   : TCardinalDynArray;
	expected : TCardinalDynArray;
begin
	AddToSystemJournal('testBubbleSortCardinal:');
	SetLength(actual,4);
	SetLength(expected,4);

	mkCardinalArray(actual,   4, 3, 2, 1);

	bubbleSortCardinalEx(actual, 1, 0); // 0-length slice.
	mkCardinalArray(expected, 4, 3, 2, 1);
	_assert( cmpCardinalArray(actual, expected) = 0, 1 );

	bubbleSortCardinalEx(actual, 1, 1); // Sort single-element array.
	mkCardinalArray(expected, 4, 3, 2, 1); // Should cause no change.
	_assert( cmpCardinalArray(actual, expected) = 0, 2 );

	bubbleSortCardinalEx(actual, 1, 2); // Sort two-element array.
	mkCardinalArray(expected, 3, 4, 2, 1);
	_assert( cmpCardinalArray(actual, expected) = 0, 3 );

	bubbleSortCardinal(actual); // Sort four-element array.
	mkCardinalArray(expected, 1, 2, 3, 4);
	_assert( cmpCardinalArray(actual, expected) = 0, 4 );

	mkCardinalArray(actual,   4, 3, 2, 1);
	mkCardinalArray(expected, 1, 2, 3, 4);
	bubbleSortCardinal(actual);
	_assert( cmpCardinalArray(actual, expected) = 0, 5 );

	AddToSystemJournal('  Sorting test passed.');
end;

// Abs function that doesn't return a berping floating point number.
function absI32(n : Integer) : Integer;
begin
	if n < 0 then
    	Result := -n
    else
    	Result := n;
end;

function isAnimal(_objType : Word) : boolean;
begin
	Result := (
		(_objType = TYPE_EAGLE          ) OR
		(_objType = TYPE_MAGPIE         ) OR
		(_objType = TYPE_CROW           ) OR
		(_objType = TYPE_RAVEN          ) OR
		(_objType = TYPE_TROPICAL_BIRD  ) OR
		(_objType = TYPE_DIRE_WOLF      ) OR
		(_objType = TYPE_GREY_WOLF      ) OR
		(_objType = TYPE_GORILLA        ) OR
		(_objType = TYPE_MONGBAT        ) OR
		(_objType = TYPE_SNAKE          ) OR
		(_objType = TYPE_COUGER         ) OR
		(_objType = TYPE_MOUNTAIN_GOAT  ) OR
		(_objType = TYPE_BROWN_BEAR     ) OR
		(_objType = TYPE_HORSE          ) OR
		(_objType = TYPE_CAT            ) OR
		(_objType = TYPE_PIG            ) OR
		(_objType = TYPE_HORSE_DKBROWN  ) OR
		(_objType = TYPE_RABBIT         ) OR
		(_objType = TYPE_SHEEP          ) OR
		(_objType = TYPE_CHICKEN        ) OR
		(_objType = TYPE_GOAT           ) OR
		(_objType = TYPE_BLACK_BEAR     ) OR
		(_objType = TYPE_GRIZZY_BEAR    ) OR
		(_objType = TYPE_PANTHER        ) OR
		(_objType = TYPE_DOG            ) OR
		(_objType = TYPE_FOREST_OSTARD  ) OR
		(_objType = TYPE_LLAMA          ) OR
		(_objType = TYPE_RIDABLE_LLAMA  ) OR
		(_objType = TYPE_COW_BLACK      ) OR
		(_objType = TYPE_SHEEP_SHORN    ) OR
		(_objType = TYPE_TIMBER_WOLF    ) OR
		(_objType = TYPE_HORSE_WHITE    ) OR
		(_objType = TYPE_HORSE_BROWN    ) OR
		(_objType = TYPE_COW_BROWN      ) OR
		(_objType = TYPE_BULL           ) OR
		(_objType = TYPE_GREAT_HART     ) OR
		(_objType = TYPE_HIND           ) OR
		(_objType = TYPE_RAT            ) OR
		(_objType = TYPE_BOAR           ) OR
		(_objType = TYPE_CHICKEN_LIZARD ));
end;

function distanceChebyshev(x1, y1, x2, y2 : Integer) : Integer;
var
	adx, ady : Integer;
begin
	adx := absI32(x2-x1);
	ady := absI32(y2-y1);
	if adx > ady then
		Result := adx
	else
		Result := ady
end;

function targetIsValid(_objId : Cardinal; selfXY : Vector2D) : boolean;
var
	targX, targY, targZ : Integer;
	selfZ    : Integer;
	distance : Integer;
begin
	if not isNPC(_objId) then
	begin
		Result := false;
		Exit;
	end;

	if  (SKILL_TO_TRAIN = SKILL_ANIMAL_LORE)
	AND (not isAnimal(GetType(_objId))) then
	begin
		Result := false;
		//AddToSystemJournal('NPC $'+ IntToHex(_objId,8)+ ' rejected: not an animal.');
		Exit;
	end;

	if isDead(_objId) then
	begin
		Result := false;
		//AddToSystemJournal('NPC $'+ IntToHex(_objId,8)+ ' rejected: too dead.');
		Exit;
	end;

	if _objId = Self() then
	begin
		Result := false;
		//AddToSystemJournal('NPC $'+ IntToHex(_objId,8)+ ' rejected: yourself.');
		Exit;
	end;

	targX := GetX(_objId);
	targY := GetY(_objId);
	distance := distanceChebyshev(selfXY.x, selfXY.y, targX, targY);
	if distance > SIGHT_RANGE then
	begin
		Result := false;
		//AddToSystemJournal('NPC $'+ IntToHex(_objId,8)+ ' rejected: distance of '
		//	+ IntToStr(distance)+ ' is greater than SIGHT_RANGE '
		//	+ IntToStr(SIGHT_RANGE)+ '.');
		Exit;
	end;

	targZ := GetZ(_objId);
	selfZ := GetZ(Self());
	if not CheckLOS(selfXY.x, selfXY.y, selfZ, targX, targY, targZ, WorldNum()) then
	begin
		Result := false;
		//AddToSystemJournal('NPC $'+ IntToHex(_objId,8)+ ' rejected: no line-of-sight (LOS).');
		Exit;
	end;

	Result := true;
end;

const
	// The value to pass to FindType that makes it search all item types.
	OPT_ALL_TYPES = -1;

	// Notoriety explanations
	// See this link for details:
	// http://stealth.od.ua/Doc:Api/FindNotoriety
	NOTO_BLUE_INNOCENT   = 1;
	NOTO_GREEN_ALLY      = 2;
	NOTO_GRAY_ATTACKABLE = 3;
	NOTO_GRAY_CRIMINAL   = 4;
	NOTO_ORANGE_ENEMY    = 5;
	NOTO_RED_MURDERER    = 6;

type
	RouteArrayOfVec2D = array [1..ROUTE_LEN] of Vector2D;

	// State that's common to all trainers.
	RecCommonContext = record
		useCount        : Integer;
		gainCount       : Integer;
		skillBefore     : Double;
		skillAfter      : Double;
		skillCap        : Double;
	end;

	// State that's specific to targeted-skill trainers.
	RecTargetedSkillContext = record
		currentTarget   : Cardinal;
		targetIdx       : Integer;
		targetList      : TCardinalDynArray;
		cursorStallTime : Integer; // milliseconds
		murderTypeList  : ArrayOfWord;
	end;

	// State that's specific to trainers of simple skills like hiding.
	RecSimpleSkillContext = record
		nextPos     : Integer;
		selfX       : Integer;
		selfY       : Integer;
		originPoint : Vector2D;
		destPoint   : Vector2D;
		inCooldown  : Boolean;
		didMove     : Boolean;
		route : RouteArrayOfVec2D;
	end;

procedure computeTargetList(
	var ctx     : RecCommonContext;
	var targCtx : RecTargetedSkillContext
);
var
	selfXY         : Vector2D;
	idx            : Integer;
	srcIdx, dstIdx : Integer;
	targetCount    : Integer;
	foundList      : TCardinalDynArray;
	foundCount     : Integer;

	// Debugging:
	listBefore     : String;
	listAfter      : String;
	msg            : String;
begin
	dstIdx := 1;
	
	selfXY := Vec2D(GetX(Self()), GetY(Self()));

	// Important! : FindType only searches within 2 tiles by default.
	// This is insufficient for ranged-targeted skills, so we must
	// modify the FindDistance global(ish) configuration variable to tell
	// FindType to include a large space. If this isn't done, you'll get
	// mysteriously small target lists...
	// Documentation wording suggests that FindDistance is a RADIUS
	// (e.g.: euclidean distance), and testing confirms this. However,
	// what we *want* is Chessboard/Chebyshev distance. So we compensate
	// by introducing a factor of Sqrt(2). Thus our formula looks like so:
	//   Approximately(Ceil(SIGHT_RANGE * sqrt(2)))
	// This approximation errs on the side of making it larger than necessary.
	// This does include some area outside of our activity box, but not
	// to worry: the targetIsValid check (called later) will exclude those
	// targets that are too far away but must still be included (so that
	// the area of our FindType circle completely covers our activity box).
	FindDistance := ((SIGHT_RANGE * 1414) div 1000)+1;

	// Now that we've dealt with all that, we can call FindType and get a
	// list of targetable things.
	if FindType(OPT_ALL_TYPES, Ground()) <> 0 then
	begin
		foundList := GetFoundItems();
		foundCount := length(foundList);

		for idx := 0 to (foundCount-1) do
			if targetIsValid(foundList[idx], selfXY) then
				targetCount := targetCount + 1;

		listBefore := cardinalArrayToString(targCtx.targetList);

		if length(targCtx.targetList) <> targetCount then
			AddToSystemJournal('TargetList grew or shrunk: now has '
				+ IntToStr(targetCount)+ ' mobiles in it out of '
				+ IntToStr(length(foundList))+ ' total items.'); 

		SetLength(targCtx.targetList, targetCount);
		dstIdx := 0;
		for srcIdx := 0 to (foundCount-1) do
		begin
			if targetIsValid(foundList[srcIdx], selfXY) then
			begin
				targCtx.targetList[dstIdx] := foundList[srcIdx];
				dstIdx := dstIdx + 1;
			end;
		end;
	end;

	if length(targCtx.targetList) <= 0 then
	begin
		targCtx.targetIdx := -1;
		targCtx.currentTarget := -1;
		exit;
	end;

	// By keeping the target list sorted, we ensure that already-visited
	// targets don't get revisited unless we run out of targets.
	// This could still be suboptimal if a bunch of new targets appear
	// with itemIDs just slightly lower than currentTarget, at which point
	// exhaustion will wrap currentTarget around to already-visited
	// IDs that are lower-valued than the newer itemIDs, instead of skipping
	// to the newer IDs. Maybe sorting on appearance time instead of itemID
	// would be preferable, but I'm not completely right now. Eh, what's
	// here should behave in mostly-sane ways and be probably kinda overkill.  
	bubbleSortCardinal(targCtx.targetList);

	// The position of 'currentTarget' in the list might have changed due
	// to mobs entering/leaving. We can adjust for this by simply scanning
	// the list to find the slot with the same ID, then adjust the
	// targetIdx to match that. In the event that the currentTarget left,
	// we also accept the list entry with the next-greatest ID.
	targCtx.targetIdx := length(targCtx.targetList);
	for idx := 0 to length(targCtx.targetList)-1 do
	begin
		if targCtx.currentTarget <= targCtx.targetList[idx] then
		begin
			if ( targCtx.currentTarget < targCtx.targetList[idx] ) then
			begin
				msg := 'Switching from target $'+IntToHex(targCtx.currentTarget,8)
					+' to target $'+IntToHex(targCtx.currentTarget,8)
					+' due to previous target being no longer usable.';
				ClientPrint(msg);
				AddToSystemJournal(msg);
				listAfter := cardinalArrayToString(targCtx.targetList);
				AddToSystemJournal('targetList before: '+listBefore);
				AddToSystemJournal('targetList after : '+listAfter);
			end;

			targCtx.targetIdx := idx;
			targCtx.currentTarget := targCtx.targetList[targCtx.targetIdx];
			exit;
		end;
	end;

	// Wrap around if our currentTarget is no longer in the list and
	// it was the highest/farthest value in the list.
	if targCtx.targetIdx >= length(targCtx.targetList) then
	begin
		targCtx.targetIdx := 0;
		targCtx.currentTarget := targCtx.targetList[targCtx.targetIdx];
	end;
end;

function trainingIsPossible(var ctx : RecCommonContext) : Boolean;
begin
	// Stop doing unnecessary stuff once max is hit.
	// This check is disabled if we are in "USE_ALLOWANCE" mode, because
	// that tends to indicate a debugging/testing session.
	if (USE_ALLOWANCE <= 0) AND (ctx.skillBefore >= ctx.skillCap) then
	begin
		ClientPrint('Skill "'+ SKILL_TO_TRAIN+ '" has been maxed. Idling.');
		Wait(10000);
		Result := false;
		Exit;
	end;

	// No sense trying to use skills if we're taking a dirt nap or are not home.
	if Dead OR (not Connected) then
	begin
		Wait(500);
		Result := false;
		Exit;
	end;

	Result := true;
end;

procedure targetedSkillTrainingLoop(var ctx : RecCommonContext);
var
	targCtx : RecTargetedSkillContext;
	msg     : String;
begin
	targCtx.currentTarget   := -1;
	targCtx.targetIdx       := -1;
	targCtx.cursorStallTime := 0;

	if COMMIT_MURDER then
		targCtx.murderTypeList := parseWordArray(MURDERABLE_TYPES);

	While true do
	begin
		if not trainingIsPossible(ctx) then
			continue;

		// Avoid interference with other scripts and player actions:
		// don't try to use the skill if there's already a target cursor up.
		// (It's a kind of focus-stealing prevention.)
		if TargetPresent then
		begin
			Wait(100);

			targCtx.cursorStallTime := targCtx.cursorStallTime + 100;
			if targCtx.cursorStallTime >= TARGET_CURSOR_TIMEOUT then
			begin
				CancelTarget();
				targCtx.cursorStallTime := 0;
			end;

			continue;
		end;

		// Don't use the skill if there are no targets in the targetList.
		if length(targCtx.targetList) <= 0 then
		begin
			// But still make sure we update the targetList: otherwise we'll
			// never notice when targets enter our field of view!
			computeTargetList(ctx, targCtx);

			// Save some CPU cycles for scripts that are more useful at the moment.
			Wait(500);

			// Try again.
			continue;
		end;

		// What it says on the tin.
		UseSkill(SKILL_TO_TRAIN);
		WaitForTarget(2000);
		if not TargetPresent then
			continue; // Timeout: we tried to invoke the skill but didn't get a cursor.

		// This updates our awareness of our surroundings.
		// TargetToObject should be called very quickly after computeTargetList()
		// (e.g. with no intervening calls to "Wait") so that we don't end up
		// with a stale target list that causes us to target things that left
		// or are out of range, or starves us of newer targets.
		computeTargetList(ctx, targCtx);

		// If all valid targets are gone, then we'll just have to spin for
		// a while until more appear.
		if (targCtx.targetIdx < 0) then
		begin
			CancelTarget();
			Wait(500);
			continue;
		end;

		targCtx.currentTarget := targCtx.targetList[targCtx.targetIdx];

		// Finish using the skill. This is analogous to a human player clicking
		// the target critter/npc/whatever with the targeting cursor.
		TargetToObject(targCtx.currentTarget);

		// Track gains for anti-anti-macro purposes.
		with ctx do
		begin
			skillAfter := GetSkillValue(SKILL_TO_TRAIN);
			if skillBefore < skillAfter then
				gainCount := gainCount + 1;
			skillBefore := skillAfter;

			useCount := useCount + 1;
		end;

		// Anticipate when ServUO's anti-macro code would kick in.
		// If it happens, switch targets and print some messages to explain
		// to ourselves (in the GUI client) what is going on.
		if ((USE_ALLOWANCE >  0) AND (ctx.useCount >= USE_ALLOWANCE))
		OR ((USE_ALLOWANCE <= 0) AND (ctx.gainCount >= GAIN_ALLOWANCE)) then
		begin
			ClientPrint('Gain allowance for target "'+GetName(targCtx.currentTarget)+
				+ '" ($'+ IntToHex(targCtx.currentTarget,8) +') has been exhausted. ('+
				+ IntToStr(ctx.gainCount) + '/' + IntToStr(GAIN_ALLOWANCE)
				+ ').');

			if COMMIT_MURDER
			AND wordArrayContains(targCtx.murderTypeList, GetType(targCtx.currentTarget))
			AND (GetNotoriety(targCtx.currentTarget) = NOTO_GRAY_ATTACKABLE) then
				Attack(targCtx.currentTarget);

			targCtx.targetIdx := targCtx.targetIdx + 1;
			if targCtx.targetIdx >= length(targCtx.targetList) then
				targCtx.targetIdx := 0;
			targCtx.currentTarget := targCtx.targetList[targCtx.targetIdx];

			msg := 'Switching to target #'+ IntToStr(targCtx.targetIdx) + ' = "'
				+ GetName(targCtx.currentTarget) + '" ($'+ IntToHex(targCtx.currentTarget,8)+ ').';
			AddToSystemJournal(msg);
			ClientPrint(msg);

			AddToSystemJournal('targetList: '+cardinalArrayToString(targCtx.targetList));
 
			ctx.gainCount := 0;
		end;

		Wait(SKILL_COOLDOWN);
	end;
end;

procedure simpleSkillTrainingLoop(var ctx : RecCommonContext);
var
	simpleCtx    : RecSimpleSkillContext;
	selfX, selfY : Integer;
	j            : Integer;
	msg          : String;
begin
	simpleCtx.originPoint.x := GetX(Self);
	simpleCtx.originPoint.y := GetY(Self);

	simpleCtx.route[ 1] := Vec2D( 0,  0);
	simpleCtx.route[ 2] := Vec2D( 0, -5);
	simpleCtx.route[ 3] := Vec2D( 0,-10);
	simpleCtx.route[ 4] := Vec2D( 0,-15);
	simpleCtx.route[ 5] := Vec2D( 0,-20);
	simpleCtx.route[ 6] := Vec2D(-5,-20);
	simpleCtx.route[ 7] := Vec2D(-5,-15);
	simpleCtx.route[ 8] := Vec2D(-5,-10);
	simpleCtx.route[ 9] := Vec2D(-5, -5);
	simpleCtx.route[10] := Vec2D(-5,  0);

	simpleCtx.inCooldown := false;
	simpleCtx.nextPos    := 2;

	While true do
	begin
		simpleCtx.didMove := false;

		UseSkill(SKILL_TO_TRAIN);

		j := InJournal('You must wait a few moments to use another skill.');
		if j = HighJournal then
			simpleCtx.inCooldown := true;

		j := InJournal('You fail to hide.');
		if j = HighJournal then
			simpleCtx.inCooldown := false;

		j := InJournal('You have hidden yourself well.');
		if j = HighJournal then
			simpleCtx.inCooldown := false;

		with ctx do
		begin
			skillAfter := GetSkillValue('Hiding');
			if skillBefore < skillAfter then
				gainCount := gainCount + 1;
			skillBefore := skillAfter;
		end;

		if ctx.gainCount >= GAIN_ALLOWANCE then
		begin
			with simpleCtx do
				destPoint := vec2D_add(originPoint, route[nextPos]);
			
			// These assignments are important for correctness:
			// they cast the result of GetX|GetY into an integer type
			// that can hold values > 65535. Without these assignments,
			// the below print statements will output overflowed values.
			selfX := GetX(Self);
			selfY := GetY(Self);

			with simpleCtx do
			begin
				ClientPrint('Gain allowance for ('
					+ IntToStr(originPoint.x) + IntToStrWithPlus(selfX - originPoint.x) + ','
					+ IntToStr(originPoint.y) + IntToStrWithPlus(selfY - originPoint.y)
					+ ') has been exhausted ('+
					+ IntToStr(ctx.gainCount) + '/' + IntToStr(GAIN_ALLOWANCE)
					+ ').');
				ClientPrint('Moving to waypoint #'+ IntToStr(nextPos) + ' = ('
					+ IntToStr(originPoint.x) + IntToStrWithPlus(route[nextPos].x) + ','
					+ IntToStr(originPoint.y) + IntToStrWithPlus(route[nextPos].y) + ')');

				if NewMoveXY(destPoint.x, destPoint.y, true, 0, false) then
				begin 
					didMove := true;
					ctx.gainCount := 0;
					nextPos := nextPos + 1;
					if nextPos > ROUTE_LEN then
						nextPos := 1;
				end;
			end;
		end;

		if simpleCtx.didMove then
			Wait((SIMPLE_SKILL_COOLDOWN * 80) div 100) // 80%
		else if simpleCtx.inCooldown then
			Wait(1000)// Skill cooldown: Retry every second.
		else
		begin
			Wait(SIMPLE_SKILL_COOLDOWN); // Wait 10 seconds.
			simpleCtx.inCooldown := true;
		end;
	end;
end;

function isTargetedSkill(_skillName : String) : Boolean;
begin
	Result := (
		(_skillName = SKILL_ANATOMY    ) OR
		(_skillName = SKILL_ANIMAL_LORE) OR
		(_skillName = SKILL_EVAL_INT   )
	);
end;

function isSimpleSkill(_skillName : String) : Boolean;
begin
	Result := (
		_skillName = SKILL_HIDING
	);
end;

// Entry point for in-game hotkeys or script function execution.
procedure trainer_run();
var
	ctx : RecCommonContext;
	msg : String;
begin
	testBubbleSortCardinal();

	// This needs to be set before CheckLOS can be used, and we will be using it.
	LOSOptions := losTypeRunUO;

	// Initialize the (common) context object.
	ctx.useCount    := 0;
	ctx.gainCount   := 0;
	ctx.skillBefore := GetSkillValue(SKILL_TO_TRAIN);
	ctx.skillCap    := GetSkillCap(SKILL_TO_TRAIN);
	ctx.skillAfter  := ctx.skillBefore;

	// Dispatch into the most appropriate logic for the skill being trained.
	if isTargetedSkill(SKILL_TO_TRAIN) then
		targetedSkillTrainingLoop(ctx)
	else if isSimpleSkill(SKILL_TO_TRAIN) then
		simpleSkillTrainingLoop(ctx)
	else
	begin
		msg := 'There is no code for training the skill "'+ SKILL_TO_TRAIN+ '".';
		ClientPrint(msg);
		AddToSystemJournal(msg);
	end;
end;

// (Main / Entry point) for starting script from GUI -> here.
begin
	trainer_run();
end.