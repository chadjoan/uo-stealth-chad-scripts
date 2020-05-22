// It doesn't (just) loot Speed, it loots WITH speed.
//
// Specifics:
// - Loots the last opened container, but only if it's not your backpack and
//     and not out of reach.
// - Will remove ALL items from opened container, unless it would put you over
//     your weight limit. (TODO: item blacklist and filter conditions)
// - If the first item would put you over your weight limit, then it will
//     override the weight limiter and loot anyways. This allows you to
//     quickly override the limiter by button-mashing the loot button.
// - The 'char'-scoped global variable SpeedLooter_LastActionTick is used to
//     synchronize the script so that multiple invocations will not interfere
//     with each other.
// - Set the 'char'-scoped global variable SpeedLooter_HaltRequest to
//     a non-empty string to make this script exit once it finishes its last
//     action.
//     TODO: Make a hotkey-able script to do this.
// TODO: recycling of some junk resources when hostiles are not present and
//   a cooldown elapses; ex: use scissors on looted clothing (need to identify
//   if magic or not), then dye the cloth using the most-neutral dye tub in
//   backpack, then cut cloth to get bandages (it is never going to be useful
//   to keep the cloth, because once you get back to town you can buy
//   tremendous amounts of it from vendors).
// 
// TODO: jettison unprocessed junk items before exceeding weight limit
Program SpeedLooter;
uses
	//SpeedLooterLib,
	viking_file_io,
	viking_geometry,
	viking_types,
	viking_testing,
	viking_utf,
	SysUtils;
	//Exception;//,
	//IOUtils;
//	CheckSave;
//,
//	SysUtils; // For the Exception class
	//System.IOUtils; // For File I/O

const
	// How many tiles long your arm is on this shard.
	// Chessboard/Chebyshev distance calculations are assumed. If your shard
	// uniformely uses Euclidean distance or Manhatten distance, then this
	// script might cause your toon to tear a tendon or something.
	armLength = 2;

	// If you wander more than this many tiles away from the container in
	// the middle of looting, the script will stop.
	// Put 0 to make it never stop for this reason.
	apathyDistance = 15;

	// The longest amount of time (in milliseconds/ticks) since another
	// SpeedLooter script's last activity that will still indicate that the
	// other script is "still alive". 
	stallTimeout = 4000;

	// The amount of time to wait (in milliseconds/ticks) between item pulls.
	actionCooldown = 200;

	// The amount of time to wait (in milliseconds/ticks) between checking the
	// distance to the loot target. Only used if the player wandered away during
	// looting: this is how often we check to see if we're back in range.
	distanceCheckPollInterval = 100;

	// Number of ticks per second. It should always be 1000.
	ticksPerSecond = 1000;

	// The value to pass to FindType that makes it search all item types.
	OPT_ALL_TYPES = -1;

	// Error codes to indicate why the script is exiting.
	// These should probably be Exception classes, but I can't get inheritence
	// (or Units/Modules) to work right now, so I'll just throw normal
	// Exceptions and populate them with these constants.
	EC_HALT_REQ     = 1;
	EC_TOO_HEAVY    = 2;
	EC_OUT_OF_REACH = 3;
	EC_ESCAPED      = 4;
	EC_INTERRUPTED  = 5;

function container_distance(_contId : Cardinal) : uint32;
var
	pX, pY       : int32;
	contX, contY : int32;
begin
	pX := GetX(Self);
	pY := GetY(Self);
	contX := GetX(_contId);
	contY := GetY(_contId);
	Result := distance_chebyshev_i32(pX, pY, contX, contY);
end;

function distanceOK(_contId : Cardinal) : Boolean;
begin
	Result := (container_distance(_contId) <= armLength);
end;

procedure waitForOkayDistance(_contId : Cardinal; _t0 : int64);
var
	dist     : uint32;
	tOther   : Int64;
	globuff  : String;
begin
	// Check our distance over and over again until we're within range,
	// we leave the defined mission area, or we get a halt request.
	while true do
	begin
		Wait(distanceCheckPollInterval);

		dist := container_distance(_contId);
		if dist <= armLength then
			Break;

		if (apathyDistance <> 0) AND (dist > apathyDistance) then
			RaiseException(erException, 'EC:'+IntToStr(EC_ESCAPED));
			//raise Exception.Create('EC:'+IntToStr(EC_ESCAPED));
			//raise TExceptionEscaped.Create(IntToStr(dist));

		globuff := GetGlobal('char', 'SpeedLooter_HaltRequest');
		if length(globuff) > 0 then
			RaiseException(erException, 'EC:'+IntToStr(EC_HALT_REQ));
			//raise Exception.Create('EC:'+IntToStr(EC_HALT_REQ));
			//raise TExceptionHaltReq.Create('HALT!');
	end;

	// Look for existing invocations of this script.
	globuff := GetGlobal('char', 'SpeedLooter_LastActionTick');

	// This is unlikely: it means that the _LastActionTick variable
	// was deassigned by another instance of the script that managed to
	// loot an entire something-else and complete before we returned to
	// our looting.
	// Ironically enough, this means it's safe to resume.
	if length(globuff) <= 0 then
	begin
		SetGlobal('char', 'SpeedLooter_LastActionTick', IntToStr(GetTickCount()));
		Exit;
	end;

	// This indicates that another script is executing right now.
	tOther := StrToInt64(globuff);
	if _t0 <> tOther then
		RaiseException(erException, 'EC:'+IntToStr(EC_INTERRUPTED));
		//raise TExceptionInterrupted.Create('...');

	// Otherwise, we're ready to get back to work.
	SetGlobal('char', 'SpeedLooter_LastActionTick', IntToStr(GetTickCount()));
end;

function padLeft(_text, _fillWith : String; _desiredWidth : size_t) : String;
var
	lengthToAdd  : size_t;
	nReps        : Integer;
	whichRep     : Integer;
	overflowLen  : Integer;
	fillUnitLen  : Integer;
	fillerStr    : String;
begin
	// TODO: Needs unittesting.
	lengthToAdd := _desiredWidth - length(_text);
	if lengthToAdd <= 0 then
		Result := _text
	else
	begin
		fillUnitLen := length(_fillWith);
		nReps := lengthToAdd div fillUnitLen;
		overflowLen := fillUnitLen - (lengthToAdd mod fillUnitLen);
		if overflowLen = fillUnitLen then
		begin
			overflowLen := 0;
			nReps := nReps + 1;
		end;

		// Probably a slow implementation, but w/e.
		fillerStr := '';
		for whichRep := 1 to nReps do
			fillerStr := fillerStr + _fillWith;

		if overflowLen > 0 then
			fillerStr := Copy(fillerStr, 1, lengthToAdd);

		Result := fillerStr + _text;
	end;
end;

/// Examples:
///   rountIntToInterval(57, 10) = 60
///   rountIntToInterval(53, 10) = 50
///   rountIntToInterval(55, 10) = 60
///   rountIntToInterval(1234, 100) = 1200
///   rountIntToInterval(0, 9) = 0
///   rountIntToInterval(9, 9) = 9
///   rountIntToInterval(4, 9) = 0
///   rountIntToInterval(5, 9) = 9
/// TODO: unittests!
function roundIntToInterval(val_ : int64; interval_ : int64) : int64;
var
	halfInterval : int64;
	remainder    : int64;
	roundedDown  : int64;
begin
	halfInterval := interval_ div 2;
	if (interval_ mod 2) > 0 then
		halfInterval := halfInterval + 1;

	remainder := val_ mod interval_;
	roundedDown := val_ - remainder;
	if remainder < halfInterval then
		Result := roundedDown
	else
		Result := roundedDown + interval_; // Round up.
end;

type
	// Let's crowdsource {itemType,count,weight} correlations; because so far
	// I haven't found a function in the Stealth API that returns the weight
	// of an object based on its ID (you'll have to "drag" it to find out).
	// If we have an appreciable number of these, we could know without dragging,
	// and thus optimize the exit time for this corner-case. (I know, eyes are rolling.) 
	TWeightData = record
		itemType   : Word;
		stackSize  : uint32;
		weight     : uint32;
	end;

	ArrayOfWeightData = Array of TWeightData;
	
	TLooterContext = record
		contId          : Cardinal;
		lootList        : TCardinalDynArray;
		lootCount       : index_t;
		weightOverride  : Boolean;
		weightArrayLen  : index_t;
		weightDataArray : ArrayOfWeightData;
		success         : Boolean;
	end;

procedure printOutOfReach(const _ctx : TLooterContext);
var
	pX, pY       : int32;
	contX, contY : int32;
begin
	pX := GetX(Self);
	pY := GetY(Self);
	contX := GetX(_ctx.contId);
	contY := GetY(_ctx.contId);
	ClientPrint('SpeedLooter: '+
		+'Container '+IntToStr(_ctx.contId)+' at coords '
		+'('+IntToStr(contX)+','+IntToStr(contY)+')'
		+' is out of reach. Player coords are ('
		+'('+IntToStr(pX)+','+IntToStr(py)+'). Looting aborted.');
end;

procedure lootItem(var _ctx : TLooterContext; _lootIndex : Int64);
var
	lootId        : Cardinal;
	globuff       : String;
	stackSize     : Int64;
    t0            : Int64;   // Timing calculation fodder.
	w0, w1        : Integer; // Weight calculation fodder.
	weightDatum   : TWeightData;
begin
	lootId := _ctx.lootList[_lootIndex];

	globuff := GetGlobal('char', 'SpeedLooter_HaltRequest');
	if length(globuff) > 0 then
		RaiseException(erException, 'EC:'+IntToStr(EC_HALT_REQ));
		//raise TExceptionHaltReq.Create('HALT!');

	// Measure tare weight and target stack size.
	w0 := Weight;
	stackSize := GetQuantity(lootId);

	// Make a heartbeat so that we aren't declared dead and
	// sent to the morgue while the Healthcare industry
	// loots our retirement. Or something like that.
	t0 := GetTickCount();
	SetGlobal('char', 'SpeedLooter_LastActionTick', IntToStr(t0));

	// And if we wandered too far from the fun, go dormant
	// until we're back in range (this also throws exceptions
	// for exit conditions that can happen while dormant).
	waitForOkayDistance(_ctx.contId, t0);

	// Now we know we're in range and in control (well, with
	// reasonably certainty, anyways). Let's GRAB IT.
	if not DragItem(lootId, 0) then
	begin
		// Could not drag item for some reason.
		// Try the next one.
		// TODO: Better error handling. WHY didn't it work?
		// Recovery strat will probably change based on that answer.
		ClientPrint('SpeedLooter: Could not drag item "'
			+ GetName(lootId) + '" with ID $'+ IntToHex(lootId,4)
			+ ' ... skipping.'); 
		Exit;
	end;

	// Grab a weight datapoint.
	w1 := Weight;

	weightDatum.itemType := GetType(lootId);
	weightDatum.stackSize := stackSize;
	weightDatum.weight := w1-w0;

	_ctx.weightDataArray[_ctx.weightArrayLen] := weightDatum;
	_ctx.weightArrayLen := _ctx.weightArrayLen + 1;

	// If we could drag that without exceeding our lift
	// capacity, then we'll try to deposit it into our
	// backpack.
	if (w1 <= MaxWeight) OR _ctx.weightOverride then
	begin
		if DropItem(BackPack(), 0,0,0) then
		begin
			_ctx.lootCount := _ctx.lootCount + 1;
			Wait(actionCooldown);
		end;
		Exit;
	end
	else if (w1 > MaxWeight) AND (_lootIndex = 0) then 
	begin
		_ctx.weightOverride := true;
		ClientPrint('SpeedLooter: Weight limiter overridden.');
		if DropItem(BackPack(), 0,0,0) then
		begin
			_ctx.lootCount := _ctx.lootCount + 1;
			Wait(actionCooldown);
		end;
		Exit;
	end;

	// Disaster recovery : we're morbidly obese.

	// Step 1: Put it back.
	// This will be the correct answer regardless of next steps.
	DropItem(_ctx.contId, 0,0,0);
	Wait(actionCooldown);

	// Step 2: Is it a stack? If no, we're done. Can't grabit.
	if stackSize <= 1 then
		RaiseException(erException, 'EC:'+IntToStr(EC_TOO_HEAVY));
		//raise TExceptionTooHeavy.Create('$'+IntToHex(lootId,4));

	// Step 3: Try to loot again, but only grab the portion
	// of the stack that we can actually hold.

	// But before we try to loot, we realize that some time
	// has passed, so we need to make a heartbeat and
	// check/close distance.
	t0 := GetTickCount();
	SetGlobal('char', 'SpeedLooter_LastActionTick', IntToStr(t0));
	waitForOkayDistance(_ctx.contId, t0);

	// Right. Looting.
	// ... or math.
	//   (partialStackSize * density) = (MaxWeight - Weight)
	//   density = (w1-w0)/fullStackSize
	// And we need to know partialStackSize, because it is
	// an argument for DragItem.
	// So we blender some maths in my brain and get this:
	//   partialStackSize = (MaxWeight - Weight)*fullStackSize/(w1-w0)
	// In the below, the integer division should truncate us
	// under weight instead of over. Good.
	stackSize := ((MaxWeight - Weight) * stackSize) div (w1-w0);

	// Finally. Looting.
	if not DragItem(lootId, stackSize) then
	begin
		// Could not drag item for some reason.
		// Try the next one.
		// TODO: Better error handling. WHY didn't it work?
		// Recovery strat will probably change based on that answer.
		ClientPrint('SpeedLooter: Could not drag item "'
			+ GetName(lootId) + '" with ID $'+ IntToHex(lootId,4)
			+ ' ... skipping.'); 
		Exit;
	end;

	// If we made it this far, then it dragged successfully.
	// Now drop it into the backpack.
	if DropItem(BackPack(), 0,0,0) then
		_ctx.lootCount := _ctx.lootCount + 1;

	// Technically we're successful (well, only sorta), but
	// we have to call it quits. So we'll end with a
	// "too heavy" complaint.
	RaiseException(erException, 'EC:'+IntToStr(EC_TOO_HEAVY));
	//raise TExceptionTooHeavy.Create('$'+IntToHex(lootId,4));
end;

// Main function type stuff starts here.
procedure speedLooter_run();
var
	ctx            : TLooterContext;
	nSecs          : Integer;
	nMilliSecs     : Integer;
	nDeciSecs      : Integer;
	tStart         : Int64;
    tOther         : Int64;
    t0, t1, dt     : Int64;   // Timing calculation fodder.
	lootIndex      : index_t;
	weightArrayIdx : index_t;
	weightDatum    : TWeightData;
	weightFile     : TFileStream;
	globuff        : String;
	emsg           : String;
	ecode          : Integer;
	
	//outputLine     : String;

begin
	// Stop us from trying to loot ourselves. That would /awkward/.
	ctx.contId := LastContainer;
    if ctx.contId = Backpack then
    begin
    	ClientPrint('SpeedLooter: LastContainer is your backpack, cannot loot that.');
    	Exit;
    end;

	// Check to make sure that the target container is within arm's reach.
	if not distanceOK(ctx.contId) then
	begin
    	printOutOfReach(ctx);
		Exit;
	end;

	// Look for existing invocations of this script.
	tStart := GetTickCount();
	globuff := GetGlobal('char', 'SpeedLooter_LastActionTick');

	// if length(globuff) <= 0 then (it wasn't set)
	if length(globuff) > 0 then
	begin
		tOther := StrToInt64(globuff);

		// Has the patient flatlined long enough to call time-of-death?
		if (tStart - tOther) <= stallTimeout then
		begin
			// Nope, they've still got a chance. Can't roll 'em and take their
			// wallet yet. Too bad.
			ClientPrint('SpeedLooter: already running; invocation ignored.');
			Exit;
		end;
	end;

	// Mark our territory.
	SetGlobal('char', 'SpeedLooter_LastActionTick', IntToStr(t0));

	// Clear this. The halt request is a broadcast signal, so this script
	// does not clear it when exiting: it needs to leave that around so that
	// any other lingering instances might be able to pick it up.
	// As a consequence, the onus is on new instances to clear any stale
	// halt requests.
	SetGlobal('char', 'SpeedLooter_HaltRequest', '');

	// Do some more initialization, now that we're serious.
	ctx.weightArrayLen := 0;
	ctx.weightOverride := false;
	ctx.success        := false;
	ctx.lootCount      := 0;

	// Do the deed.
	try
		try // and try again. This extra layer is just to use except+finally together.
		begin
			if FindType(OPT_ALL_TYPES, ctx.contId) > 0 then
			begin
				ctx.lootList := GetFoundItems();

				// Allocate the array for weight data.
				// The final result might be shorter than this, but it won't
				// be longer. So this says "SetLength", but we are really setting
				// /capacity/ here. 'weightArrayLen' will be used to indicate how
				// much of this array was actually filled.
				SetLength(ctx.weightDataArray, length(ctx.lootList));

				// Use a try-finally to avoid leaking resources if anything jumps scope.
				try
					for lootIndex := 0 to length(ctx.lootList)-1 do
						lootItem(ctx, lootIndex);
				finally
					SetLength(ctx.lootList, 0);
				end;

				ctx.success := true;
			end
			else
			begin
				ClientPrint('SpeedLooter: Container was empty or inaccessible. Looting aborted.');
			end;

			// TODO: Have the script go dormant when out of range, but don't stop.
			// TODO: Instead, poll the coordinate distances and start looting again
			// TODO: once the player returns to being within range.
			// (And be sure to SetGlobal('char', 'SpeedLooter_LastActionTick', '')
			// while dormant and then exit use GetGlobal to check if another instance
			// has taken over.)
			// ERR, above seems to be done maybe?
		end;
		except
		begin
		(*on exc : TExceptionHaltReq do
			ClientPrint('SpeedLooter: Halting as requested.');
		on exc : TExceptionTooHeavy do
			ClientPrint('SpeedLooter: Halting due to reaching weight limit.');
		on exc : TExceptionOutOfReach do
    		printOutOfReach();
		on exc : TExceptionEscaped do
			ClientPrint('SpeedLooter: Wandered more than '+IntToStr(apathyDistance)
				+ ' tiles away from the container. Aborting looting op.');
		on exc : TExceptionInterrupted do
			ClientPrint('SpeedLooter: Dormant script exiting to allow current one to proceed.');
*)
		//on exc : Exception do
			emsg := ExceptionToString(ExceptionType, ExceptionParam);

			
			//if not emsg.StartsWith('EC:') then
			if pos('EC:', emsg) <> 0 then
				RaiseLastException();
				//raise exc;

			ecode := StrToInt(Copy(emsg, 4, length(emsg)-3));
			case ecode of
				EC_HALT_REQ :
					ClientPrint('SpeedLooter: Halting as requested.');

				EC_TOO_HEAVY :
					ClientPrint('SpeedLooter: Halting due to reaching weight limit.');

				EC_OUT_OF_REACH :
    				printOutOfReach(ctx);
					
				EC_ESCAPED :
					ClientPrint('SpeedLooter: Wandered more than '+IntToStr(apathyDistance)
						+ ' tiles away from the container. Aborting looting op.');
					
				EC_INTERRUPTED :
					ClientPrint('SpeedLooter: Dormant script exiting to allow current one to proceed.');
			else
				RaiseLastException();
				// raise;
			end;
		end;
		end;
	finally
	begin
		// Notify of completion.
		if ctx.success then
		begin
			t0 := tStart;
			t1 := GetTickCount();
			dt := t1-t0;
			nSecs := dt div ticksPerSecond;
			nMillisecs := dt mod ticksPerSecond;
			nDeciSecs := roundIntToInterval(nMillisecs, 100) div 100;
			ClientPrint('SpeedLooter: Looted '+ IntToStr(ctx.lootCount)
				+ ' items in '+ IntToStr(nSecs)+ '.'
				+ IntToStr(nDeciSecs) + ' seconds.');
		end;

		// Dump weightDataArray to a .csv
		if ctx.weightArrayLen > 0 then
		begin
			(*AssignFile(weightFile, 'item-weights.csv');
			*)
			weightFile :=
				mk_file_stream_open_for_appending('item-weights.csv');
			//weightFile := TStreamWriter.Create('item-weights.csv', true, TEncoding.UTF8);

			// Use a try-finally to avoid leaking file handles if an error occurs.
			try
			begin
				for weightArrayIdx := 0 to ctx.weightArrayLen-1 do
				begin
					weightDatum := ctx.weightDataArray[weightArrayIdx];
					write_utf8     (weightFile, '$'+IntToHex(weightDatum.itemType, 4));
					write_utf8     (weightFile, ','+IntToStr(weightDatum.stackSize));
					write_utf8_line(weightFile, ','+IntToStr(weightDatum.weight));
				end;
			end;
			finally
				weightFile.free();
			end;

			// Use a try-finally to avoid leaking file handles if an error occurs.
			(*try
			begin
				if FileExists('test.txt') then
					Append(weightFile)
				else
				begin
					Rewrite(weightFile);
					Write(weightFile, '"Item ID","Stack Size","Weight"'); 
				end;

				//for weightDatum in weightDataArray do
				for weightArrayIdx := 0 to weightArrayLen-1 do
				begin
					weightDatum = weightDataArray[weightArrayIdx];
					Write(weightFile, 
						'$'+IntToHex(weightDatum.itemType, 4)+','
						+ IntToStr(weightDatum.stackSize)+','
						+ IntToStr(weightDatum.weight));
				end;
			end;
			finally
				CloseFile(weightFile);
			end;
			*)
		end;
		SetLength(ctx.weightDataArray, 0);

		// Clear the global synchronization variable so that subsequent invocations
		// can proceed immediately.
		SetGlobal('char', 'SpeedLooter_LastActionTick', '');

		// Note: The halt request is a broadcast signal, so this script
		// does not clear it when exiting: it needs to leave that around so that
		// any other lingering instances might be able to pick it up.
		// // SetGlobal('char', 'SpeedLooter_HaltRequest', '');
	end;
	end;
end;

begin
	speedLooter_run();
end.
