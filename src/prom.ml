open Option

module PromiseExtra = struct
  let const (t : 'a): 'a Js.Promise.t =
    Js.Promise.make (fun ~resolve ~reject -> resolve t [@bs])

  let option p =
    p
    |> Option.map (Js.Promise.then_ (fun v -> const (Some v)))
    |> Option.orElse (fun () -> const None)

  let map (f: 'a -> 'b) (p : 'a Js.Promise.t): 'b Js.Promise.t =
    Js.Promise.then_ (fun value -> const (f value)) p
end
