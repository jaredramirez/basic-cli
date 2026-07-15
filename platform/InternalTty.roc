## A deliberately flat representation of a terminal event at the native ABI.
## Tty converts this into the ergonomic public tag unions.
InternalTty :: [].{
	RawEvent : {
		category : U8,
		code : U16,
		column : U16,
		kind : U8,
		modifiers : U8,
		row : U16,
		text : Str,
	}
}
