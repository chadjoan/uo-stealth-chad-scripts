program LumberSmack;
uses
	viking_arrays,
	viking_geometry,
	viking_types,
	viking_string_algorithms;

const
	AXE_TYPES =
		'$13FB,$143E,$0F4B,$1443,$0F47,$0F45,$0F4D,$0F49';

	LOG_TYPE = $1BDD;

	// How many trees the script will remember have already been chopped up.
	// This gets scanned every time it needs to find a new tree, so it could get
	// slow if this is a large number. It might take thousands to make life suck, though.
	// tldr: Make this large if the script keeps visiting already-chopped trees.
	TREES_TO_REMEMBER = 20;
	// (It might be possible to optimize this into an O(log2(N)) lookup instead
	// of a full scan, but I don't feel like writing insertion sort (again)
	// right now. Probably not necessary :) .)


	// Number of seconds it takes for a tree to regrow and become choppable again.
	// (right now I'm just guessing)
	REGROWTH_TIME = 300;
	
	// The ones here with numbers in the name were translated into Russian and
	// then back into English.
	// I do not know what the original strings were.
	// These may need to be corrected.
	MsgSuccess    = 'You chop some ordinary logs and put them into your backpack.'; // 'moved'
	Msg2          = 'failed';
	MsgFailed     = 'You hack at the tree for a while, but fail to produce any useable wood.';
	Msg4          = 'You decide not to chop wood';
	MsgNoMoreWood = (* There's *) 'not enough wood here to harvest.';
	Msg6          = 'appears immune';
	Msg7          = 'Try chopping';
	Msg8          = 'reach this';
	MsgMustWait   = 'You must wait to perform another action.';
	MsgTooFarAway = 'That is too far away.';

	// The value to pass to FindType that makes it search all item types.
	OPT_ALL_TYPES = -1;

	SEARCH_RADIUS = 30; // Radius (not diameter!) Of tree search in tiles,regarding character

type
	visited_tree_rec = record
		x          : int32;
		y          : int32;
		visitTime  : int64; // Ticks/millisecs
	end;

	visited_tree_array = Array [0..TREES_TO_REMEMBER-1] of visited_tree_rec;

	ArrayOfTFoundTiles = Array of TFoundTile;



(*function uint16_array_contains(haystack_ :uint16_array;  needle_ :uint16) : boolean;
var
	idx : index_t;
begin
	for idx := Low(haystack_) to High(haystack_) do
	begin
		if needle_ = haystack_[idx] then
		begin
			Result := true;
			Exit;
		end;
	end;
	Result := false; 
end;
*)

var
	axeTypes      : uint16_array;
	treeTiles     : uint16_array;
	treeBlackList : visited_tree_array;

function is_axe(itemType_ :uint16) : boolean;
begin
	Result := uint16_array_contains(axeTypes, itemType_);
end; 

function find_an_axe() : uint32;
var
	axeItemId    : uint32;
	itemList     : uint32_array;
	itemId       : uint32;
	itemIdx      : index_t;
	itemType     : uint16;

	rhandItemId   : uint32;
	lhandItemId   : uint32;
	rhandItemType : uint16;
	lhandItemType : uint16;
begin

	axeItemId := 0;
	if FindTypeEx(OPT_ALL_TYPES, $FFFF, Backpack(), true) <> 0 then
	begin
		itemList := GetFoundItems();
		
		for itemIdx := 0 to length(itemList)-1 do
		begin
			itemId := itemList[itemIdx];
			itemType := GetType(itemId);
			if is_axe(itemType) then
			begin
				axeItemId := itemId;
				break;
			end;
		end;

		SetLength(itemList,0);
	end;

	if axeItemId = 0 then
	begin
		rhandItemId := ObjAtLayer(RhandLayer);
		lhandItemId := ObjAtLayer(LhandLayer);
		rhandItemType := GetType(rhandItemId);
		lhandItemType := GetType(lhandItemId);

		axeItemId := 0;
		if is_axe(rhandItemType) then
			axeItemId := rhandItemId
		else
		if is_axe(lhandItemType) then
			axeItemId := lhandItemId;
	end;
	Result := axeItemId;
end;

// Find an axe. Do communication if we can't.
function ensure_axe_exists() : uint32;
var
	axeItemId    : uint32;
	spamArrestor : boolean;
begin
	spamArrestor := false;
	while true do
	begin
		axeItemId := find_an_axe();
		if axeItemId = 0 then
		begin
			if not spamArrestor then
				ClientPrint('chopper: '
					+'Could not find any axes in your backpack. '
					+'Will wait until there are more.');
			spamArrestor := true;
			wait(500);
			continue;
		end
		else
		begin
			spamArrestor := false;
			break;
		end;
	end;
	Result := axeItemId;
end;

procedure process_logs();
var
	idx           : index_t;
	axeItemId     : uint32;
	logItemId     : uint32;
	itemList      : uint32_array;
begin
	// Convert logs to boards.
	if (FindType(LOG_TYPE, Backpack()) <> 0) then
	begin
		itemList := GetFoundItems();
		for idx := 0 to length(itemList)-1 do
		begin
			axeItemId := ensure_axe_exists();
			logItemId := itemList[idx];

			UseObject(axeItemId);
			if not WaitForTarget (5000) then
				ClientPrint('Tried to use axe to convert logs to boards, but target didn''t come up.');
			If TargetPresent then
			begin
				TargetToObject(logItemId)
				Wait(500);
			end;
		end;
	end;
end;

// Initialize the array of tree tile types
// Taken from http://stealth.od.ua/forum/viewtopic.php?f=2&t=2877
// Translated:
// https://translate.google.com/translate?sl=auto&tl=en&u=http%3A%2F%2Fstealth.od.ua%2Fforum%2Fviewtopic.php%3Ff%3D2%26t%3D2877
procedure init_tree_tiles_array();
begin
	SetLength(treeTiles, 40);

	treeTiles [ 0] := 3274;
	treeTiles [ 1] := 3275;
	treeTiles [ 2] := 3277;
	treeTiles [ 3] := 3280;

	treeTiles [ 4] := 3283;
	treeTiles [ 5] := 3286;
	treeTiles [ 6] := 3288;
	treeTiles [ 7] := 3290;

	treeTiles [ 8] := 3293;
	treeTiles [ 9] := 3296;
	treeTiles [10] := 3299;
	treeTiles [11] := 3302;

	treeTiles [12] := 3320;
	treeTiles [13] := 3323;
	treeTiles [14] := 3326;
	treeTiles [15] := 3329;

	treeTiles [16] := 3393;
	treeTiles [17] := 3394;
	treeTiles [18] := 3395;
	treeTiles [19] := 3396;

	treeTiles [20] := 3415;
	treeTiles [21] := 3416;
	treeTiles [22] := 3417;
	treeTiles [23] := 3418;

	treeTiles [24] := 3419;
	treeTiles [25] := 3438;
	treeTiles [26] := 3439;
	treeTiles [27] := 3440;

	treeTiles [28] := 3441;
	treeTiles [29] := 3442;
	treeTiles [30] := 3460;
	treeTiles [31] := 3461;

	treeTiles [32] := 3462;
	treeTiles [33] := 3476;
	treeTiles [34] := 3478;
	treeTiles [35] := 3480;

	treeTiles [36] := 3482;
	treeTiles [37] := 3484;
	treeTiles [38] := 3492;
	treeTiles [39] := 3496;
end;

// Search for trees
// Taken from http://stealth.od.ua/forum/viewtopic.php?f=2&t=2877
// Translated:
// https://translate.google.com/translate?sl=auto&tl=en&u=http%3A%2F%2Fstealth.od.ua%2Fforum%2Fviewtopic.php%3Ff%3D2%26t%3D2877
function find_trees() : ArrayOfTFoundTiles;
var
	i             : index_t;
	numTreeTypes  : index_t;

	foundTiles    : TFoundTilesArray;
	numFoundTiles : index_t;
	foundTilesIdx : index_t;

	treeList      : ArrayOfTFoundTiles;
	numTrees      : index_t;
	treeListIdx   : index_t;

begin
	numTreeTypes := length(treeTiles);
	numTrees := 0;

	for i := 0 to numTreeTypes-1 do
	begin
		numFoundTiles := GetStaticTilesArray(
			(GetX (Self) - SEARCH_RADIUS), (GetY (Self) - SEARCH_RADIUS),
			(GetX (Self) + SEARCH_RADIUS), (GetY (Self) + SEARCH_RADIUS),
			1, treeTiles[i], foundTiles);

		if numFoundTiles <= 0 then
			continue;

		numTrees := numTrees + numFoundTiles;
		SetLength (treeList, numTrees);
		for foundTilesIdx := 0 to numFoundTiles - 1 do
		begin
			treeList [treeListIdx] := foundTiles [foundTilesIdx];
			treeListIdx := treeListIdx + 1;
		end;
	end;
	AddToSystemJournal ('Number of trees found: ' + IntToStr (numTrees));
	Result := treeList;
end;

function merge_tile_arrays(
	var   intoArray : ArrayOfTFoundTiles;
	const fromArray : ArrayOfTFoundTiles) : ArrayOfTFoundTiles;
var
	i, j: Integer;

	intoLen : index_t;
	fromLen : index_t;
begin
	fromLen := length(fromArray);
	intoLen := length(intoArray);

	if fromLen <= 0 then
	begin
		Result := intoArray;
		Exit;
	end;

	for i := 1 to fromLen - 1 do
	begin
		for j := 0 to intoLen - 1 do
			if (intoArray [j] = fromArray [i]) then
				break;

		if j > intoLen - 1 then
		begin
			intoLen := intoLen + 1;
			SetLength (intoArray, intoLen);
			intoArray[intoLen - 1] := fromArray[i];
		end;
	end;

	Result := intoArray;
end;

// Chop the tree (Edred)
function chop_this_tree(tile, x, y, z : int32): Boolean;
// chop the specified tile. Return false if the advantage or spell is dead.
var
	q, m2, m4, m6, m7, m8, CountFizzle, NextTree: int32;
	
	flags           : uint32;
	CHOP_SUCCESS    : uint32;
	CHOP_FAILED     : uint32;
	NO_MORE_WOOD    : uint32;
	WAIT_FOR_ACTION : uint32;
	TOO_FAR_AWAY    : uint32;

	treeVisit     : visited_tree_rec;
	axeItemId     : uint32;
	timeStart     : TDateTime;
	timeEnd       : TDateTime;
	nagCooldownAt : int64; // ticks (milliseconds since epoch)
	nagInterval   : int64; // milliseconds

	rhandItemId   : uint32;
	lhandItemId   : uint32;
	rhandItemType : uint16;
	lhandItemType : uint16;

begin
	CHOP_SUCCESS    := $0001;
	CHOP_FAILED     := $0002;
	NO_MORE_WOOD    := $0004;
	WAIT_FOR_ACTION := $0008;
	TOO_FAR_AWAY    := $0010;

	Result := true;
	nagInterval := 10000;
	CountFizzle := 0;
	NextTree := 0;
	while (NextTree <= 3) do
	begin
		flags := 0;
		if WarMode = true then
			SetWarMode (false);

		if TargetPresent then
			CancelTarget;

		timeStart := Now;
		
		if Dead then
		begin
			Result := false;
			exit;
		end;

		// Convert logs to boards.
		process_logs();

		// Ensure that an axe is equipped.
		rhandItemId := ObjAtLayer(RhandLayer);
		lhandItemId := ObjAtLayer(LhandLayer);
		rhandItemType := GetType(rhandItemId);
		lhandItemType := GetType(lhandItemId);
		if not (is_axe(rhandItemType) OR is_axe(lhandItemType)) then 
		begin
			axeItemId := ensure_axe_exists();

			// Remove any non-axe things from hands.
			if (rhandItemId <> 0)
			OR (lhandItemId <> 0) then
			begin
				ClientPrint('chopper: Non-axe in hands. Replacing with axe.');
				disarm();
				wait(1000);
			end
			else
				ClientPrint('chopper: No axe in hands. Equipping axe.');


			// Put axe in hands.
			if not Equip(RhandLayer, axeItemId) then
			begin
				wait(1000);
				if not Equip(LhandLayer, axeItemId) then
				begin
					ClientPrint('chopper: Could not equip axe.');
					wait(5000);
					continue;
				end;
			end;
			
			wait(500);
		end
		else if is_axe(rhandItemType) then
			axeItemId := rhandItemId
		else if is_axe(lhandItemType) then
			axeItemId := lhandItemId
		else
		begin
			ClientPrint('chopper: Reached invalid part of code. Stopping.');
			Result := false;
			exit;
		end;

		// choppity chop
		UseObject(axeItemId);
		WaitForTarget (5000);
		If TargetPresent then
		begin
			TargetToTile (tile, x, y, z);
			wait (200);
		end;

		// Scan for journal messages.
		q := 0;
		while true do
		begin;
			q := q + 1;
			//checksave
			timeEnd := Now();

			if (InJournalBetweenTimes (MsgSuccess,    timeStart, timeEnd) <> -1) then
				flags := flags or CHOP_SUCCESS;

			if (InJournalBetweenTimes (MsgFailed,     timeStart, timeEnd) <> -1) then
				flags := flags or CHOP_FAILED;

			if (InJournalBetweenTimes (MsgNoMoreWood, timeStart, timeEnd) <> -1) then
				flags := flags or NO_MORE_WOOD;

			if (InJournalBetweenTimes (MsgMustWait,   timeStart, timeEnd) <> -1) then
				flags := flags or WAIT_FOR_ACTION;

			if (InJournalBetweenTimes (MsgTooFarAway, timeStart, timeEnd) <> -1) then
				flags := flags or TOO_FAR_AWAY;
			
			m2 := InJournalBetweenTimes (Msg2, timeStart, timeEnd);
			m4 := InJournalBetweenTimes (Msg4, timeStart, timeEnd);
			m6 := InJournalBetweenTimes (Msg6, timeStart, timeEnd);
			m7 := InJournalBetweenTimes (Msg7, timeStart, timeEnd);
			m8 := InJournalBetweenTimes (Msg8, timeStart, timeEnd);

			if (flags <> 0)
			or (m2 <> -1) or (m4 <> -1) or (m6 <> -1) or (m7 <> -1) or (m8 <> -1)
			or (FindType(LOG_TYPE, Backpack()) <> 0)
			or Dead or (q> 50) then
				break;

			wait (100);
		end;

		if ((flags and CHOP_FAILED) > 0)
		or (m2 <> - 1) or (m4 <> - 1) then
			CountFizzle := CountFizzle + 1;

		if (flags and TOO_FAR_AWAY) > 0 then
		begin
			treeVisit.x := x;
			treeVisit.y := y;
			treeVisit.visitTime := GetTickCount();
			SetLength(treeBlackList, length(treeBlackList)+1);
			treeBlackList[length(treeBlackList)-1] := treeVisit;
		end;

		if ((flags and NO_MORE_WOOD) > 0)
		or (m6 <> - 1) OR (m7 <> - 1) OR (m8 <> -1)
		OR (CountFizzle = 10) then
			break;

		if ((flags and WAIT_FOR_ACTION) > 0) then
		begin
			wait(1000);
			continue;
		end;

		if Dead then
		begin
			Result := false;
			exit;
		end;

		// Convert logs to boards.
		process_logs();

		// Wait for The Human to deposit The Wood.
		nagCooldownAt := GetTickCount()-1;
		while (Weight > MaxWeight) do
		begin
			if nagCooldownAt < GetTickCount() then
			begin
				ClientPrint('chopper: You are now over your weight limit. Drop some wood to lighten up.');
				nagCooldownAt := GetTickCount() + nagInterval;
			end;
			wait(500);
		end;

		if (q > 50) then
			NextTree := NextTree + 1;
	end;
end;

var
	idx      : index_t;
	jdx      : index_t;
	timeNow  : int64;
	regrowthTimeMs : int64;
	
	alreadyVisited : boolean;
	inBlacklist    : boolean;

	treeList : ArrayOfTFoundTiles;
	numTrees : index_t;
	tree     : TFoundTile;

	nTreesVisited : index_t;
	treesVisited  : visited_tree_array;
	nextVisitIdx  : index_t;
	treeVisit     : visited_tree_rec;

	stillGood : boolean;
begin
	axeTypes := parse_uint16_array(AXE_TYPES);
	init_tree_tiles_array();

	regrowthTimeMs := REGROWTH_TIME*1000;
	nTreesVisited := 0;

	while true do
	begin
		// We either just started or just finished a tree, so our surroundings
		// may have new trees to consider. Let's add them to our list.
		merge_tile_arrays(treeList, find_trees());
		numTrees:= length(treeList);

		// If we started the script in a place with no trees, we can't do much
		// about that, so error out as humanely as possible.
		if numTrees <= 0 then
		begin
			ClientPrint('chopper: There are no trees, or at least none that this script knows to look for.');
			ClientPrint('chopper: Unable to continue. Exiting.');
			break;
		end;

		timeNow := GetTickCount();

		// Look for fresh (not-already-chopped) trees to attack.
		for idx := 0 to numTrees-1 do
		begin
			tree := treeList[idx];
			
			// Any tree will do if we never chopped em'.
			if nTreesVisited = 0 then
				break;

			// Exclude trees that we've visited too recently.
			alreadyVisited := false;
			for jdx := 0 to nTreesVisited-1 do
			begin
				treeVisit := treesVisited[jdx];
				if  (tree.x = treeVisit.x) AND (tree.y = treeVisit.y)
				AND (timeNow < (treeVisit.visitTime + regrowthTimeMs)) then
				begin
					alreadyVisited := true; 
					break; // This tree's (still) had enough. Next!
				end;
			end;
			if alreadyVisited then
				continue;

			// Exclude trees that are inaccessible or not choppable for some reason.
			inBlacklist := false;
			for jdx := 0 to length(treeBlacklist)-1 do
			begin
				treeVisit := treeBlacklist[jdx];
				if  (tree.x = treeVisit.x) AND (tree.y = treeVisit.y) then
				begin
					inBlacklist := true; 
					break; // We couldn't chop this tree at all for some reason.
				end;
			end;
			if inBlacklist then
				continue;

			break; // This tree's good!
		end;

		// No new trees found. But we should have trees around, we just have
		// to wait for them to recharge.
		if idx > numTrees then
		begin
			// Couldn't find any fresh trees to chop.
			wait(1000);
			continue; // Maybe some stale ones will refresh...
		end;

		// Move to the tree and take a bunch of swings at it.
		NewMoveXY(tree.x, tree.y, false, 1, false);
		stillGood :=
			chop_this_tree(tree.tile, tree.x, tree.y, tree.z);

		// Exception handling :p
		if not stillGood then
		begin
			if Dead then
				ClientPrint('chopper: Too dead to continue. Stopping.')
			else
				ClientPrint('chopper: Something went wrong and we can no longer continue chopping trees.');
			break;
		end;

		// Update our visited-trees list so that we can avoid re-chopping the
		// tree we just chopped.
		treeVisit.x := tree.x;
		treeVisit.y := tree.y;
		treeVisit.visitTime := GetTickCount();
		treesVisited[nextVisitIdx] := treeVisit;
		nextVisitIdx := nextVisitIdx + 1;
		if ( nextVisitIdx > nTreesVisited ) then
			nTreesVisited := nextVisitIdx;
		if ( nextVisitIdx >= TREES_TO_REMEMBER ) then
			nextVisitIdx := 0; // Loop back around to the beginning.
	end;
end.
