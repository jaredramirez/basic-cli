import Host
import IOErr exposing [IOErr]

## Provides functionality to change the behaviour of the terminal.
## This is useful for running an app like vim or a game in the terminal.
Tty :: [].{

	## Enable terminal [raw mode](https://en.wikipedia.org/wiki/Terminal_mode) to disable some default terminal bevahiour.
	##
	## This leads to the following changes:
	## - Input will not be echoed to the terminal screen.
	## - Input will be sent straight to the program instead of being buffered (= collected) until the Enter key is pressed.
	## - Special keys like Backspace and CTRL+C will not be processed by the terminal driver but will be passed to the program.
	enable_raw_mode! : () => Try({}, [TtyErr(IOErr), ..])
	enable_raw_mode! = || widen_tty_err(Host.tty_enable_raw_mode!())

	## Revert terminal to default behaviour
	disable_raw_mode! : () => Try({}, [TtyErr(IOErr), ..])
	disable_raw_mode! = || widen_tty_err(Host.tty_disable_raw_mode!())

	## Return the terminal's current dimensions in character cells.
	size! : () => Try({ columns : U16, rows : U16 }, [TtyErr(IOErr), ..])
	size! = || widen_tty_err(Host.tty_size!())
}

widen_tty_err : Try(a, [TtyErr(IOErr)]) -> Try(a, [TtyErr(IOErr), ..])
widen_tty_err = |result|
	match result {
		Ok(value) => Ok(value)
		Err(TtyErr(err)) => Err(TtyErr(err))
	}
