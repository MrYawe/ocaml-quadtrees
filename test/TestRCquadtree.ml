open OUnit2;;
open Point;;
open Rectangle;;
open RCquadtree;;

(******************************************************************************)
(*                            Test rcinsert                                   *)
(******************************************************************************)

(* alias *)
let bs = base_surface;;


let rcinsert_pquadtrees = [
  (* #1 *)
  rcinsert RCEmpty {top=100; bottom=10; left=100; right=300};

  (* #2 *)
  rcinsert RCEmpty {top=100; bottom=10; left=100; right=200};

  (* #3 *)
  rcinsert_list [
    {top=100; bottom=10; left=100; right=300};
    {top=100; bottom=10; left=100; right=200};
  ];

  (* #4 *)
  rcinsert
    ~surface:{top=1024; right=1024; bottom=0; left=0}
    RCEmpty
    {top=600; bottom=500; left=10; right=220};

  (* #5 *)
  rcinsert
    ~surface:{top=1024; right=1024; bottom=0; left=0}
    RCEmpty
    {top=100; bottom=10; left=100; right=300};

  (* #6 *)
  rcinsert_list ~surface:{top=1024; right=1024; bottom=0; left=0} [
    {top=100; bottom=10; left=100; right=300};
    {top=400; bottom=70; left=600; right=750};
  ];

  (* #7 *)
  (*  This rcquadtree is equal to #6. The inconsistent surface (3 3 3 3) will
      be ignored because the base surface is already present in the first
      rcquadtree.
   *)
  rcinsert
    ~surface:{top=3; right=3; bottom=3; left=3}
    (rcinsert
      ~surface:{top=1024; right=1024; bottom=0; left=0}
      RCEmpty
      {top=100; bottom=10; left=100; right=300})
    {top=400; bottom=70; left=600; right=750};
]

let rcinsert_res = [
  (* #1 *)
  RCNode(
    bs,
    [{top=100; bottom=10; left=100; right=300}], [],
    RCEmpty, RCEmpty, RCEmpty, RCEmpty
  );

  (* #2 *)
  RCNode(
    bs,
    [], [],
    RCEmpty,
    RCEmpty,
    RCNode(
      {top=(bs.top/2); bottom=bs.bottom; left=bs.left; right=(bs.right/2)},
      [{top=100; bottom=10; left=100; right=200}], [],
      RCEmpty, RCEmpty, RCEmpty, RCEmpty
    ),
    RCEmpty
  );

  (* #3 *)
  RCNode(
    bs,
    [{top=100; bottom=10; left=100; right=300}], [],
    RCEmpty,
    RCEmpty,
    RCNode(
      {top=(bs.top/2); bottom=bs.bottom; left=bs.left; right=(bs.right/2)},
      [{top=100; bottom=10; left=100; right=200}], [],
      RCEmpty, RCEmpty, RCEmpty, RCEmpty
    ),
    RCEmpty
  );

  (* #4 *)
  RCNode(
    {top=1024; right=1024; bottom=0; left=0},
    [], [{top=600; bottom=500; left=10; right=220}],
    RCEmpty, RCEmpty, RCEmpty, RCEmpty
  );

  (* #5 *)
  RCNode(
    {top=1024; right=1024; bottom=0; left=0},
    [], [],
    RCEmpty,
    RCEmpty,
    RCNode(
      {top=512; right=512; bottom=0; left=0},
      [{top=100; bottom=10; left=100; right=300}], [],
      RCEmpty, RCEmpty, RCEmpty, RCEmpty
    ),
    RCEmpty
  );

  (* #6 *)
  RCNode(
    {top=1024; right=1024; bottom=0; left=0},
    [], [],
    RCEmpty,
    RCEmpty,
    RCNode(
      {top=512; right=512; bottom=0; left=0},
      [{top=100; bottom=10; left=100; right=300}], [],
      RCEmpty, RCEmpty, RCEmpty, RCEmpty
    ),
    RCNode(
      {top=512; right=1024; bottom=0; left=512},
      [], [{top=400; bottom=70; left=600; right=750}],
      RCEmpty, RCEmpty, RCEmpty, RCEmpty
    )
  );

  (* #7 *)
  RCNode(
    {top=1024; right=1024; bottom=0; left=0},
    [], [],
    RCEmpty,
    RCEmpty,
    RCNode(
      {top=512; right=512; bottom=0; left=0},
      [{top=100; bottom=10; left=100; right=300}], [],
      RCEmpty, RCEmpty, RCEmpty, RCEmpty
    ),
    RCNode(
      {top=512; right=1024; bottom=0; left=512},
      [], [{top=400; bottom=70; left=600; right=750}],
      RCEmpty, RCEmpty, RCEmpty, RCEmpty
    )
  );

  (* RCNode(
    bs,
    [], [],
    RCEmpty,
    RCEmpty,
    RCNode(
      {top=(bs.top/2); bottom=bs.bottom; left=bs.left; right=(bs.right/2)},
      [{top=100; bottom=10; left=100; right=200}], [],
      RCEmpty, RCEmpty, RCEmpty, RCEmpty
    ),
    RCEmpty
  ); *)

]

let test_rcinsert test_ctxt =
  List.iter2 assert_equal rcinsert_pquadtrees rcinsert_res;;


(******************************************************************************)
(*                            Test contains                                   *)
(******************************************************************************)

let rccontain_pqadtrees = [
  (* #1 *)
  rcinsert_list [{top=100; bottom=10; left=100; right=300}];

  (* #2 *)
  rcinsert_list [
    {top=460; bottom=310; left=230; right=310};
    {top=265; bottom=230; left=300; right=480};
    {top=215; bottom=150; left=110; right=235};
    {top=490; bottom=460; left=410; right=485};
    {top=275; bottom=240; left=260; right=310};
    {top=470; bottom=460; left=2; right=20};
    {top=250; bottom=210; left=270; right=305};
  ];
]

let rccontain_points = [
  (* #1 *)
  [
    {x=150; y=60}; (* ##1 *)
    {x=400; y=60}; (* ##2 *)
    {x=150; y=500}; (* ##3 *)
    {x=400; y=500}; (* ##4 *)
  ];

  (* #2 *)
  [
    {x=303; y=245}; (* ##1 *)
  ];
]

let rccontain_expected = [
  (* #1 *)
  [
    [{top=100; bottom=10; left=100; right=300}]; (* ##1 *)
    []; (* ##2 *)
    []; (* ##3 *)
    []; (* ##4 *)
  ];

  (* #2 *)
  [
    [ (* ##1 *)
      {top=250; bottom=210; left=270; right=305};
      {top=265; bottom=230; left=300; right=480};
      {top=275; bottom=240; left=260; right=310};
    ];
  ]
]

let test_rccontain test_ctxt =

  let res = List.map2
    (fun pqt pts -> List.map (rccontain pqt) pts)
    rccontain_pqadtrees
    rccontain_points in
    (* let map_res = List.map (fun li -> List.map string_of_rectangle_list li) res in
      List.iter (fun li -> List.iter print_string li) map_res; *)
      List.iter2
        (fun res1 res2 ->
            List.iter2
              (assert_equal ~printer:string_of_rectangle_list) res1 res2)
        rccontain_expected
        res;;

let tests =
"rcquadtree_tests">::: [
  "test_rcinsert">:: test_rcinsert;
  "test_rccontain">:: test_rccontain;
];;
