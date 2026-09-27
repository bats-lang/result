#include "share/atspre_staload.hats"
#use result as R

(* Makes, inspects and consumes results and options through every
   function of the API (it runs under valgrind: no cell may be lost).
   Exits 1 on a wrong value. *)
fn half (x: int): $R.result(int, int) =
  if x mod 2 = 0 then $R.ok(x / 2) else $R.err(x)

fn find (x: int): $R.option(int) =
  if x > 0 then $R.some(x) else $R.none()

implement main0 () = let
  val r1 = half(10)
  val b1 = $R.is_ok<int><int>(r1) && ~$R.is_err<int><int>(r1)
  val v1 = $R.unwrap_or<int><int>(r1, 0)
  val r2 = half(7)
  val b2 = $R.is_err<int><int>(r2)
  val v2 = (case+ r2 of ~$R.ok(v) => v | ~$R.err(e) => e * 100): int
  val () = $R.discard<int><int>(half(3))
  val o1 = find(4)
  val b3 = $R.is_some<int>(o1) && ~$R.is_none<int>(o1)
  val v3 = $R.option_unwrap_or<int>(o1, 0)
  val o2 = find(~4)
  val b4 = $R.is_none<int>(o2)
  val v4 = $R.option_unwrap_or<int>(o2, 9)
  val () = $R.option_discard<int>(find(1))
  val ok = b1 && b2 && b3 && b4 && v1 = 5 && v2 = 700 && v3 = 4 && v4 = 9
  val () = (if ok then () else println! ("FAIL: ", v1, " ", v2, " ", v3, " ", v4))
in if ok then () else exit(1) end
