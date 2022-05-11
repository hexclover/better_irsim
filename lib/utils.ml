module type MONAD = sig
  type 'a t

  val pure : 'a -> 'a t
  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
end

module MonadExtra =
functor
  (Monad : MONAD)
  ->
  struct
    open Monad

    let ( >> ) (f : 'a t) (g : 'b t) : 'b t = f >>= fun _ -> g
    let ( <* ) (f : 'a t) (g : 'b t) : 'a t = f >>= fun a -> g >> pure a
    let ( *> ) : 'a t -> 'b t -> 'b t = ( >> )
  end

module type STATE_ERROR = sig
  type s
  type e
end

module type STATE_EITHER = functor (State : STATE_ERROR) -> sig
  include MONAD

  val get : State.s t
  val put : State.s -> unit t
  val fail : State.e -> 'a t
  val run : State.s -> 'a t -> ('a, State.e) result * State.s
  val eval : State.s -> 'a t -> ('a, State.e) result
  val exec : State.s -> 'a t -> State.s
end

module StateEither : STATE_EITHER =
functor
  (State : STATE_ERROR)
  ->
  struct
    type state = State.s
    type 'a t = state -> ('a, State.e) result * state

    let pure a s = (Ok a, s)

    let ( >>= ) f g s =
      match f s with Ok a, s' -> g a s' | Error e, s' -> (Error e, s')

    let get s = (Ok s, s)
    let put s _ = (Ok (), s)
    let fail e s = (Error e, s)
    let run s f = f s
    let eval s f = fst (run s f)
    let exec s f = snd (run s f)
  end
