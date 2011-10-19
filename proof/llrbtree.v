(* USAGE
 * $ coqc --version
 * The Coq Proof Assistant, version 8.3pl2 (August 2011)
 * compiled on Aug 25 2011 04:21:44 with OCaml 3.12.0
 * $ time coqc -opt llrbtree.v
 * real    6m49.834s
 * user    6m47.449s
 * sys     0m2.208s
 *)

Inductive Color : Set := R | B.

Section RB.

  Parameter a : Set.
  
  Inductive RBTree :=
  | Leaf
  | Fork : Color -> RBTree -> a -> RBTree -> RBTree.

  Definition empty := Leaf.

  Inductive hasSameBlackDepth : nat -> RBTree -> Prop :=
  | Oleaf : hasSameBlackDepth O Leaf
  | SforkB : forall (l r: RBTree) (m: nat) (x: a),
    hasSameBlackDepth m l -> hasSameBlackDepth m r ->
    hasSameBlackDepth (S m) (Fork B l x r)
  | SforkR : forall (l r: RBTree) (m: nat) (x: a),
    hasSameBlackDepth m l -> hasSameBlackDepth m r ->
    hasSameBlackDepth m (Fork R l x r).

  Definition isBlackSame (t: RBTree): Prop :=
    exists n: nat, hasSameBlackDepth n t.

  
  Fixpoint reds (c: Color) (t: RBTree) : bool :=
    match (c, t) with
      | (_, Leaf) => true
      | (R, Fork R _ _ _) => false
      | (_, Fork c l _ r) => andb (reds c l) (reds c r)
    end.

  Definition isRedSeparate (t: RBTree) :=
    is_true (reds B t).
    
  Definition isBalanced (t: RBTree): Prop :=
    isBlackSame t /\ isRedSeparate t.

  Inductive cmp := LT | GT | EQ.
  Parameter compare : a -> a -> cmp.

  Definition balanceL c l x r :=
    match (c,l,x,r) with
      | (B, (Fork R (Fork R a x b) y c), z, d) =>
        Fork R (Fork B a x b) y (Fork B c z d)
      | (c, a, x, b) => Fork c a x b
    end.

  Definition balanceR c l x r :=
    match (c,l,x,r) with
      | (B, (Fork R a x b), y, (Fork R c z d)) =>
        Fork R (Fork B a x b) y (Fork B c z d)
      | (k, x, y, (Fork R c z d)) =>
        Fork k (Fork R x y c) z d
      | (c, a, x, b) => Fork c a x b
    end.

  Fixpoint ins x t :=
    match t with
      | Leaf => Fork R Leaf x Leaf
      | Fork c l y r =>
        match compare x y with
          | LT => balanceL c (ins x l) y r
          | GT => balanceR c l y (ins x r)
          | EQ => t
        end
    end.

  Definition insert a b :=
    match ins a b with
      | Fork _ d e f =>  Fork B d e f
      | Leaf => (* never reached *) Leaf
    end.

  Fixpoint isLeftLean t :=
    match t with
      | Leaf => true
      | (Fork B _ _ (Fork R _ _ _)) => false
      | (Fork _ r _ l) => andb (isLeftLean r) (isLeftLean l)
    end.

  Definition valid t :=
    isBalanced t /\ is_true (isLeftLean t).

  Lemma redsRB: forall t1,
    reds R t1 = true -> reds B t1 = true.
    destruct t1; intuition.
    inversion H.
    case_eq c.
    intro.
    subst.
    congruence.

    intro.
    subst.
    apply andb_prop in H1.
    destruct H1.
    simpl.
    reflexivity.

    Qed.
    
  Lemma hasSameFunctional: forall t m m0,
    hasSameBlackDepth m t -> 
    hasSameBlackDepth m0 t -> m = m0.
    induction t.
    intuition.
    inversion H.
    inversion H0.
    auto.

    intros.
    inversion H; subst.
    inversion H0; subst.

    intuition.

    inversion H; subst.
    inversion H0; subst.

    intuition.

    Qed.
    
    Lemma ins_result_not_leaf: forall x t, ins x t = Leaf -> False.
      intro x.
      induction t.
      simpl.
      congruence.
      simpl.
      destruct (compare x a0).
      case (ins x t1).
      unfold balanceL.
      case c.
      congruence.
      congruence.
      intro.

      intro.
      intro.
      intro.
      unfold balanceL.
      case c.
      congruence.
      case c0.
      case r.
      congruence.
      intros.
      destruct c1.
      congruence.
      congruence.
      congruence.
      unfold balanceR.
      destruct c.
      destruct (ins x t2); try congruence.
      destruct c; try congruence.
      destruct t2; try destruct (ins x t2); try destruct t1; try congruence.
      simpl.
      congruence.
      destruct c.
      simpl.
      congruence.
      simpl; congruence.
      destruct c; simpl; try congruence.
      destruct (compare x a1).
      destruct (balanceL R (ins x t2_1) a1 t2_2).
      congruence.
      destruct c.
      congruence.
      congruence.
      destruct (balanceR R t2_1 a1 (ins x t2_2)); try destruct c; try congruence.
      congruence.
      destruct (compare x a1); try destruct (balanceL B (ins x t2_1) a1 t2_2); try destruct (balanceR B t2_1 a1 (ins x t2_2)); try destruct c; try destruct c0; try congruence.
      destruct c0.
      destruct (ins x (Fork c t2_1 a1 t2_2)); try destruct c0; try congruence.
      destruct (ins x (Fork c t2_1 a1 t2_2)); try destruct c0; try congruence.
      congruence.

      Qed.


  Ltac pira :=
    unfold is_true, empty, isBalanced, isBlackSame, isRedSeparate, balanceL, balanceR in *;     simpl in *; auto; try congruence; 
    match goal with
      (* killing *)
      | [|- hasSameBlackDepth O Leaf] => apply Oleaf
      | [H :ins ?x ?t1 = Leaf |- _] => clear - H; apply False_ind; apply ins_result_not_leaf with x t1
      (* context_non_splitting *)
      | [H: ?m = ?m, H1: ?m = ?m |- _] =>
        clear H1
      | [|- _ -> _] => intro
      | [IH : valid ?t -> valid (insert ?x ?t) |- _] => destruct IH
      | [IH : context[valid _] |- _] => unfold valid in IH
      | [|- context[valid _] ] => unfold valid
      | [IH : context[insert _ _] |- _] => unfold insert in IH
      | [|- context[insert _ _] ] => unfold insert 
      | [H: exists n, _ |- _] => destruct H
      | [|- exists m: nat, hasSameBlackDepth m Leaf] => exists O
      | [|- exists n : nat, hasSameBlackDepth n (Fork R Leaf _ _)] => exists O
      | [|- exists n : nat, hasSameBlackDepth n (Fork B Leaf _ _)] => exists (S O)
      | [|- exists n : nat, hasSameBlackDepth n (Fork R _ _ Leaf)] => exists O
      | [|- exists n : nat, hasSameBlackDepth n (Fork B _ _ Leaf)] => exists (S O)
      | [|- exists n : nat, hasSameBlackDepth n (Fork B (Fork R Leaf _ _) _ _)] =>
        exists (S O)
      | [|- exists n : nat, hasSameBlackDepth n (Fork B (Fork R _ _ Leaf) _ _)] =>
        exists (S O)
      | [|- exists n : nat, hasSameBlackDepth n
        (Fork R (Fork B (Fork R Leaf _ _) _ _) _ _)] => exists (S O)
      | [|- exists n : nat, hasSameBlackDepth n
        (Fork R (Fork B (Fork R _ _ Leaf) _ _) _ _)] => exists (S O)
      | [|- context[reds _ Leaf]] => simpl
      | [H: _ /\ _ |- _] => destruct H
      | [H: hasSameBlackDepth ?x0 ?t1 |- exists n, hasSameBlackDepth n ?t1]
        => exists x0
      | [H: (?a && ?b)%bool = true |- _] => apply andb_prop in H
(*      | [H: ?a = true |- _] => rewrite H in * *)
      | [H : reds R ?t1 = true |- reds B ?t1 = true] => apply redsRB
      | [H: hasSameBlackDepth ?m ?t, H0: hasSameBlackDepth ?n ?t |- _] =>
        progress (
          first[
            match goal with
              | [H1: m = n |- _] => idtac
            end |
            (assert (m = n) by (apply hasSameFunctional with t; auto); subst)])
      | [H :ins ?x ?t = Fork _ _ _ _ |- _] => rewrite H in *
      |  [ H: hasSameBlackDepth ?x ?t1 |-
          exists n : nat, hasSameBlackDepth n (Fork R (Fork R ?t1 _ _) _ _)]
        =>
        exists x
      | [ |- exists n : nat, hasSameBlackDepth n (Fork R (Fork R Leaf _ Leaf) _ Leaf)] =>
        exists O
      | [ H31 : hasSameBlackDepth ?x ?r1 |- exists n : nat,
        hasSameBlackDepth n
        (Fork B (Fork R (Fork B ?r1 _ _) _ _)_ _)] =>
        exists (S (S x))
      | [ H31 : hasSameBlackDepth ?m1 ?r2 |-
        exists n : nat,
          hasSameBlackDepth n
          (Fork B (Fork R _ _ (Fork B _ _ ?r2)) _ _)] =>
        exists (S (S m1))
      | [ H31 : hasSameBlackDepth ?x ?r1 |- exists n : nat,
        hasSameBlackDepth n
        (Fork R (Fork B (Fork B ?r1 _ _) _ _)_ _)] =>
        exists (S (S x))
      | [ H33 : hasSameBlackDepth ?x ?r1 |-
        exists n : nat,
          hasSameBlackDepth n
          (Fork B (Fork B _ _ ?r1) _ _)] =>
        exists (S (S x))
      | [ H31 : hasSameBlackDepth ?x ?r1 |- exists n : nat,
        hasSameBlackDepth n
        (Fork B (Fork B (Fork R ?r1 _ _) _ _)_ _)] =>
        exists (S (S x))
      | [ H31 : hasSameBlackDepth ?x ?r1 |- exists n : nat,
        hasSameBlackDepth n
        (Fork B (Fork B (Fork B ?r1 _ _) _ _)_ _)] =>
        exists (S (S (S x)))
      | [H: hasSameBlackDepth _ (Fork _ _ _ _) |- _] =>
        inversion H; clear H; subst
      | [H: reds _ (Fork _ _ _ _) = _ |- _] =>
        inversion H; clear H; subst
      | [H: (isLeftLean (Fork _ _ _ _)) = true |- _] =>
        inversion H; clear H; subst
      | [H14 : hasSameBlackDepth _ Leaf |- _] =>
        inversion H14; clear H14; subst
      | [ H11 : Fork _ _ _ _ = Fork _ _ _ _ |- _] =>
        inversion H11; clear H11; subst
      (* context_splitting *)
      | [|- andb _ _ = true] => apply andb_true_intro
      | [|- _ /\ _] => split
      | [c : Color |-_] => case_eq c; subst
      | [H : hasSameBlackDepth ?x ?t |- exists n : nat, hasSameBlackDepth n (Fork B ?t _ _)] => exists (S x)
      | [H : hasSameBlackDepth ?x ?t |- exists n : nat, hasSameBlackDepth n (Fork R ?t _ _)] => exists x
      | [|- hasSameBlackDepth (S _) (Fork B _ _ _)] => apply SforkB
      | [H := ?x : nat |- hasSameBlackDepth ?x (Fork B _ _ _)] => destruct x
      | [|- hasSameBlackDepth _ (Fork R _ _ _)] => apply SforkR
      | [H := ?x : nat |- hasSameBlackDepth ?x Leaf] => destruct x
      | [|- context[compare ?a ?b]] => destruct (compare a b)
      | [H: context[compare ?a ?b] |- _ ] => destruct (compare a b)
      | [|- context [isLeftLean Leaf]] => simpl
      | [|- context [isLeftLean (Fork _ _ _)] ] => simpl
      (* too heavy *)
      (* undecided *)
(*       | [H14 : hasSameBlackDepth 0 _ |- _] =>
        inversion H14; clear H14; subst  causes inf loop *) 
    end
  .

  Ltac pirapira := progress (repeat pira).
  Ltac finish := abstract pirapira.

  Lemma valid_empty: valid empty.
    pirapira.
    Qed.
    
    Ltac c x t :=                                                                        
        case_eq (ins x t).                                                               
                                                                                         
    Ltac d t :=                                                                          
      destruct t.

    Ltac a :=
      try finish; pirapira.

  Lemma valid_insert: forall x t, valid t -> valid (insert x t).
    intro x; induction t; a.

    d t2; finish.
    d t2; finish.
    d t2; finish.
    d t1; a.
    c x t1_1; a.
    d r; a.
    d t1_1; a; c x t1_2; a.
    c x t1_1; a.
    c x t1_2; a.
    d t1_1; a.
    d t1_1; a.
    Focus 12.
    d t1; a; d t2; a.
    Unfocus.
    Focus 11.
    d t1; a; d t2; a.
    c x t2_1; a.
    d r; a.
    d t2_1; a; c x t2_2; a.
    c x t2_1; a.
    d r; a.
    d t2_1; c x t2_2;a.
    c x t2_1; a.
    d r; a.
    d t2_1; a; c x t2_2; a.
    Unfocus.
    Focus 10.
    d t1; a.
    d t2; a.
    d t2; a.
    c x t1_1; a.
    d r; a.
    c x t1_1;a.
    d r;a.
    c x t1_1;a.
    d r;a.
    d t2; a; c x t1_2; a.
    d t1_1;a.
    d t1_1;a.
    d t1_1;a.
    d t1_1;a.
    d t1_1;a.
    d t1_1;a.
    d t2; a; d t1_2; a.
    Unfocus.
    Focus 9.
    d t2; a.
    c x t2_1;a.
    d r;a.
    d t2_1; a; c x t2_2; a.
    Unfocus.
    Focus 8.
    d t1; a.
    c x t1_1;a.
    d r;a.
    d t1_1; a; c x t1_2; a.
    Unfocus.
    Focus 7.
    d t2; a.
    c x t2_1; a.
    d r; a.
    d t2_1; a; c x t2_2; a.
    Unfocus.
    Focus 6.
    d t1; a.
    c x t1_1;a.
    d r;a.
    d t1_1;a; c x t1_2; a.
    Unfocus.
    Focus 5.
    d t1; a.
    d t2; a.
    d t2; a.
    c x t2_1;a.
    d r;a.
    d t2_1; a; c x t2_2;a.
    d t2; a.
    c x t2_1; a.
    d r;a.
    d t2_1; a; c x t2_2; a.
    d t2; a.
    c x t2_1;a.
    d r;a.
    d t2_1;a; c x t2_2; a.
    d t2; a.
    d t1_2; a.
    c x t2_1; a.
    d r;a.
    d t1_2;a.
    d t2_1; a; c x t2_2; a.
    d t1_2; a.
    d t2; a.
    d t1_2; a.
    c x t2_1; a.
    d r;a.
    d t1_2;a.
    d t2_1;a.
    c x t2_2;a.
    c x t2_2;a.
    c x t2_2;a.
    d t1_2;a.
    d t2;a.
    c x t2_1;a.
    d r;a.
    d t2_1;a.
    c x t2_2;a.
    c x t2_2;a.
    c x t2_2;a.
    Unfocus.
    Focus 4.
    d t1; a.
    c x t1_1;a.
    d r;a.
    d t2;a;d t1_2;a.
    d t2;a.
    d t2;a.
    d t2;a.
    d t1_1; a; c x t1_2; a.
    d t2;a; d r0; a.
    d t2; a; d r0; a; d t1_1_2; a.
    d t2; a; d r0; a; d t1_1_2; a.
    d t2; a; d t1_1_2; a; d r0; a.
    d t2; a; d r0; a.
    c x t1_1; a.
    d t1_2; a.
    d t2; a; d r0; a; d t1_2_2; a.
    d r0; a.
    d t2; a; d t1_2; a.
    d t1_2; a; d r0_2; a.
    d r0_2; a.
    d t1_2; a.
    d t1_2; a.
    d t1_2; a.
    d t1_2; a.

    d t2;a.
    c x t1_2; a.
    d r;a.
    d r0;a.
    d r0; a; d r2; a.
    d r2; a.
    d r0;a.
    d r0;a.
    d r2_2; a.
    d r0;a.
    d r0; a.
    d r0;a.
    d r0;a.
    d r0;a.
    d t2; a.
    d r0; a.
    d r0;a.
    d r0;a.
    d r0;a.
    d t1_1;a.
    d t2; a; d t1_1_2;a.
    d t1_1;a.
    Unfocus.
    Focus 3.
    d t1; a; d t2; a.
    c x t2_1; a.
    d r;a.
    d t2_1;a.
    c x t2_2;a.
    c x t2_2;a.
    c x t2_2;a.
    c x t2_1;a.
    d r;a.
    d t2_1; a; c x t2_2; a.
    c x t2_1;a.
    d r;a.
    d t2_1; a;c x t2_2;a.
    c x t2_1; a.
    d r;a.
    d t2_1; a; c x t2_2; a.
    c x t2_1; a.
    d r; a.
    d t2_1; a; c x t2_2; a.
    c x t2_1; a.
    d r;a.
    d t2_1; a; c x t2_2; a.
    Unfocus.
    Focus 2.
    d t1; a.
    c x t1_1; a.
    d r;a.
    d t1_1; c x t1_2; a.
    c x t1_1; a.
    c x t1_2; a.
    d t1_1;a.
    d t1_1;a.
    Unfocus.
    d t1; a; d t2; a.
    c x t2_1;a.
    d r;a.
    d t2_1; a; c x t2_2;a.
    c x t2_1; a.
    d r;a.
    d t2_1;a; c x t2_2;a.
    c x t2_1; a.
    d r;a.
    d t2_1; a; c x t2_2;a.
    c x t2_1; a.
    d r;a.
    exists 2; a.
    exists (S (S (S m))); a.
    exists (S (S (S m))); a.
    Focus 2.
    exists (S (S m0)); a.
    Unfocus.
    Focus 7.
    exists (S (S m0)); a.
    Unfocus.
    d t2_1; a; c x t2_2;a.
    exists 2; a.
    exists (S (S (S m1))); a.
    exists (S (S (S m1))); a.
    exists (S (S (S m1))); a.
    c x t2_1; a.
    d r;a.
    exists 2; a.
    exists (S (S (S m0))); a.
    exists (S (S (S m0))); a.
    d t2_1; a; c x t2_2; a.
    exists 2; a.
    exists (S (S (S m1))); a.
    exists (S (S (S m1))); a.
    exists (S (S (S m1))); a.
    exists (S (S m)); a.
    c x t2_1; a.
    d r;a.
    exists 2; a.
    exists (S (S (S m))); a.
    exists (S (S (S m))); a.
    d t2_1; a; c x t2_2; a.
    exists 2; a.
    exists (S (S (S m1))); a.
    exists (S (S (S m1))); a.
    exists (S (S (S m1))); a.
    Qed.
    
End RB.
