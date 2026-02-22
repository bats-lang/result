(* result -- linear result and option types *)
(* Must pattern-match to consume. Errors cannot be ignored. *)

#include "share/atspre_staload.hats"

(* ============================================================
   Result -- ok(value) or err(error)
   Both type parameters are vt@ype (support linear types).
   ============================================================ *)

#pub datavtype result(a:vt@ype, e:vt@ype) =
  | ok(a, e) of (a)
  | err(a, e) of (e)

(* Unwrap ok value or return default. Consumes the result.
   Only works when both a and e are t@ype (copyable/droppable). *)
#pub fn{a:t@ype}{e:t@ype}
unwrap_or(r: result(a, e), default_val: a): a

(* Is this an ok? Non-consuming check. *)
#pub fn{a:vt@ype}{e:vt@ype}
is_ok(r: !result(a, e)): bool

(* Is this an err? Non-consuming check. *)
#pub fn{a:vt@ype}{e:vt@ype}
is_err(r: !result(a, e)): bool

(* Discard a result without extracting. Only for t@ype. *)
#pub fn{a:t@ype}{e:t@ype}
discard(r: result(a, e)): void

(* ============================================================
   Option -- some(value) or none (unchanged)
   ============================================================ *)

#pub datavtype option(a:vt@ype) =
  | some(a) of (a)
  | none(a) of ()

#pub fn{a:t@ype}
option_unwrap_or(o: option(a), default_val: a): a

#pub fn{a:vt@ype}
is_some(o: !option(a)): bool

#pub fn{a:vt@ype}
is_none(o: !option(a)): bool

#pub fn{a:t@ype}
option_discard(o: option(a)): void

(* ============================================================
   Implementations
   ============================================================ *)

implement{a}{e}
unwrap_or(r, default_val) =
  case+ r of
  | ~ok(v) => v
  | ~err(_) => default_val

implement{a}{e}
is_ok(r) = let
  val b = case+ r of | ok(_) => true | err(_) => false
in b end

implement{a}{e}
is_err(r) = let
  val b = case+ r of | ok(_) => false | err(_) => true
in b end

implement{a}{e}
discard(r) =
  case+ r of | ~ok(_) => () | ~err(_) => ()

implement{a}
option_unwrap_or(o, default_val) =
  case+ o of
  | ~some(v) => v
  | ~none() => default_val

implement{a}
is_some(o) = let
  val b = case+ o of | some(_) => true | none() => false
in b end

implement{a}
is_none(o) = let
  val b = case+ o of | some(_) => false | none() => true
in b end

implement{a}
option_discard(o) =
  case+ o of | ~some(_) => () | ~none() => ()

(* ============================================================
   Static tests
   ============================================================ *)

fn _test_result_ok(): void = let
  val r : result(int, int) = ok(42)
  val v = unwrap_or<int><int>(r, 0)
in () end

fn _test_result_err(): void = let
  val r : result(int, int) = err(~1)
  val v = unwrap_or<int><int>(r, 0)
in () end

fn _test_result_match(): void = let
  val r : result(int, int) = ok(42)
in
  case+ r of
  | ~ok(v) => ()
  | ~err(code) => ()
end

fn _test_option_some(): void = let
  val o : option(int) = some(99)
  val v = option_unwrap_or<int>(o, 0)
in () end

fn _test_option_none(): void = let
  val o : option(int) = none()
  val v = option_unwrap_or<int>(o, 0)
in () end

fn _test_is_ok(): void = let
  val r : result(int, int) = ok(1)
  val b = is_ok<int><int>(r)
  val () = discard<int><int>(r)
in () end

fn _test_is_some(): void = let
  val o : option(int) = some(1)
  val b = is_some<int>(o)
  val () = option_discard<int>(o)
in () end
