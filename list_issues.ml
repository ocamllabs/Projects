open Lwt
open Cmdliner
open Printf

module T = Github_t

let ocl_classes_of_labels labels =
  let names = List.map (fun { T.label_name } -> label_name) labels in
  List.map Ocamllabs.of_string names

let string_of_labels labels =
  let names = List.map (fun { T.label_name } -> label_name) labels in
  let _ocl_classes (* TODO *) = List.map Ocamllabs.of_string names in
  String.concat ", " names

let print_issue user repo issue =
  let {
    T.issue_number;
    issue_title;
    issue_labels;
    issue_comments;
    issue_state;
    issue_created_at;
    issue_closed_at;
  } = issue in
  (* box drawing from https://en.wikipedia.org/wiki/Box-drawing_character *)
  printf "┏ #%d %s\n%!" issue_number issue_title;
  printf "┣    Labels: %s\n%!" (string_of_labels issue_labels);
  printf "┣   Comments: %d\n%!" issue_comments;
  (match issue_state with
   | `Open   -> printf "┗    Created at %s\n" issue_created_at
   | `Closed -> match issue_closed_at with
     | Some timestamp -> printf "┗    Closed at %s\n" timestamp
     | None -> printf "┗    Closed timestamp missing!"
  )

let get_user_repos =
  List.map (fun r ->
    match Stringext.split ~max:2 ~on:'/' r with
    | [user;repo] -> (user,repo)
    | _ -> eprintf "Repositories must be in username/repo format"; exit 1
  ) 

let print_milestone m = 
  let open Github_t in
  Fmt.(pf stdout "╳ %a (%a/%d issues)\n%!"
    (styled `Bold string) m.milestone_title
    (styled `Green int) m.milestone_closed_issues
    m.milestone_open_issues)

let list_issues token repos ~all ~closed ~milestone =
 (* Get the issues per repo *)
  Lwt_list.iter_s (fun (user,repo) ->
    let state = if all then `All else if closed then `Closed else `Open in
    Github.(Monad.(run (
      let issues_s = Issue.for_repo ~token ~state ~user ~repo ~milestone () in
      Stream.to_list issues_s (* TODO: bound?!?! *)
      >>= fun list -> return (List.iter (fun i -> print_issue user repo i
      ) list))))
  ) (get_user_repos repos)

let github_map_s fn l =
  let open Github.Monad in
  let rec iter acc =
     function
     | hd::tl ->
         fn hd >>= fun r ->
         iter (r :: acc) tl
     | [] ->  return acc
  in
  iter [] l

let github_iter_s fn l =
  let open Github.Monad in
  let rec iter = function
    | hd::tl -> fn hd >>= fun () -> iter tl
    | [] ->  return ()
  in
  iter l

let list_one_milestone token user repo ~all ~closed =
  let open Github in
  let open Github.Monad in
  let milestone_s = Milestone.for_repo ~token ~user ~repo () in
  Stream.to_list milestone_s
  >>= fun list -> 
  github_iter_s (fun m ->
    print_milestone m;
    let state = if all then `All else if closed then `Closed else `Open in
    let milestone = `Num m.Github_t.milestone_number in
    let issues_s = Issue.for_repo ~token ~state ~milestone ~user ~repo () in
    Stream.to_list issues_s
    >>= fun list ->
    github_iter_s (fun i ->
      Github.Monad.return (print_issue user repo i)
    ) list
  ) list

let list_milestones token repos ~all ~closed =
  Lwt_list.iter_s (fun (user,repo) ->
    Github.(Monad.(run (
      list_one_milestone token user repo ~all ~closed
    )))
  ) (get_user_repos repos)

let cmd =
  let cookie = Jar_cli.cookie () in
  let repos = Jar_cli.repos ~doc_append:" to list issues and PRs" () in

  let doc = "show only closed issues" in
  let docv = "CLOSED" in
  let closed = Arg.(value & flag & info ["closed"] ~docv ~doc) in
  let doc = "show all issues" in
  let docv = "ALL" in
  let all = Arg.(value & flag & info ["all"] ~docv ~doc) in

  let doc = "list issues on GitHub repositories (open only by default)" in
  let man = [
    `S "BUGS";
    `P "Email bug reports to <mirageos-devel@lists.xenproject.org>.";
  ] in
  Term.((pure (fun t r all closed ->
    Lwt_main.run (list_milestones t r ~all ~closed)
  ) $ cookie $ repos $ all $ closed)),
  Term.info "git-list-issues" ~version:"1.0.0" ~doc ~man

let () = Fmt_tty.setup_std_outputs ()

let () = match Term.eval cmd with `Error _ -> exit 1 | _ -> exit 0

(*
 * Copyright (c) 2015 David Sheets <sheets@alum.mit.edu>
 * Copyright (c) 2015-2016 Anil Madhavapeddy <anil@recoil.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 *)


