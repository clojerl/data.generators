;   Copyright (c) Rich Hickey, Stuart Halloway, and contributors.
;   All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns ^{:author "Stuart Halloway"
      :doc "Data generators for Clojure."}
  clojure.data.generators
  (:refer-clojure :exclude [byte char int float double boolean string symbol keyword list vec tuple set hash-map name rand-nth shuffle])
  (:require [clojure.core :as core]))

(set! *warn-on-infer* true)

#_(def ^:dynamic ^java.util.Random
     *rnd*
     "Random instance for use in generators. By consistently using this
instance you can get a repeatable basis for tests."
     (java.util.Random. 42))

(defn- call-through
  "Recursively call x until it doesn't return a function."
  [x]
  (if (or (fn? x) (var? x))
    (recur (x))
    x))

(defn reps
  "Returns sizer repetitions of f (or (f) if f is a fn)."
  [sizer f]
  (let [count (call-through sizer)]
    (if (or (fn? f) (var? f))
      (repeatedly count f)
      (repeat count f))))

(defn geometric
  "Geometric distribution with mean 1/p."
  [p]
  (clj_utils/ceil (/ (math/log (rand/uniform))
                     (math/log (- 1.0 p)))))

(def ^:private MAX_INTEGER (dec (apply * (repeat 63 2))))
(def ^:private MIN_INTEGER (- MAX_INTEGER))

(defn uniform
  "Uniform distribution from lo (inclusive) to hi (exclusive).
   Defaults to range of 64 signed integer."
  ([] (uniform MIN_INTEGER MAX_INTEGER))
  ([lo hi] {:pre [(< lo hi)]}
   (clj_utils/floor (+ lo (* (rand/uniform) (- hi lo))))))

(defn float
  "Generate a float between 0 and 1 based on *rnd*"
  []
  (rand/uniform))

(defn double
  "Generate a double between 0 and 1 based on *rnd*."
  []
  (rand/uniform))

(defn rand-nth
  "Replacement of core/rand-nth that allows control of the
   randomization basis (through binding *rnd*)."
  [coll]
  (nth coll (uniform 0 (count coll))))

(defn tuple
  "Generate a tuple with one element from each generator."
  [& generators]
  (into [] (map #(%) generators)))

(defn weighted
  "Given a map of generators and weights, return a value from one of
   the generators, selecting generator based on weights."
  [m]
  (let [weights   (reductions + (vals m))
        total   (last weights)
        choices (map vector (keys m) weights)]
    (let [choice (uniform 0 total)]
      (loop [[[c w] & more] choices]
        (when w
          (if (< choice w)
            (call-through c)
            (recur more)))))))

(defn one-of
  "Generates one of the specs passed in, with equal probability."
  ([& specs]
     (weighted (zipmap specs (repeat 1)))))

(def int
  "Returns an int based on *rnd*. Same as uniform."
  uniform)

(defn boolean
  "Returns a bool based on *rnd*."
  []
  (< 0.5 (rand/uniform)))

(defn printable-ascii-char
  "Returns a char based on *rnd* in the printable ascii range."
  []
  (core/char (uniform 32 127)))

(defn char
  "Returns a character based on *rnd* in the range 0-65536."
  []
  (core/char (uniform 0 65536)))

(defn default-sizer
  "Default sizer used to run tests. If you want a specific distribution,
   create your own and pass it to a fn that wants a sizer."
  []
  (dec (geometric 0.02)))

(defn list
  "Create a list with elements from f and sized from sizer."
  ([f] (list f default-sizer))
  ([f sizer]
     (reps sizer f)))

(defn vec
  "Create a vec with elements from f and sized from sizer."
  ([f] (vec f default-sizer))
  ([f sizer]
     (into [] (reps sizer f))))

(defn set
  "Create a set with elements from f and sized from sizer."
  ([f] (set f default-sizer))
  ([f sizer]
     (into #{} (reps sizer f))))

(defn hash-map
  "Create a hash-map with keys from fk, vals from fv, and
   sized from sizer."
  ([fk fv] (hash-map fk fv default-sizer))
  ([fk fv sizer]
     (into {}
           (zipmap (reps sizer fk)
                   (reps sizer fv)))))

(defn string
  "Create a string with chars from f and sized from sizer."
  ([] (string printable-ascii-char))
  ([f] (string f default-sizer))
  ([f sizer] (apply str (reps sizer f))))

(def ^:private ascii-alpha
  (-> []
      (into (map core/char (range 65 (+ 65 26))))
      (into (map core/char (range 97 (+ 97 26))))))

(def ^:private symbol-start
  (-> (concat ascii-alpha [\* \+ \! \- \_ \?])
      core/vec))

(def ^:private symbol-char
  (into symbol-start [\1 \2 \3 \4 \5 \6 \7 \8 \9 \0]))

;; cannot generate every legal prefix, but at least avoids
;; the illegal "-1"

(defn- name-prefix
  []
  (str (rand-nth [(rand-nth symbol-start) ""])
       (rand-nth ascii-alpha)))

(defn- name-body
  [sizer]
  (string #(rand-nth symbol-char) sizer))

(defn- name
  ([] (name default-sizer))
  ([sizer]
   (str (name-prefix)
        (name-body sizer))))

(defn symbol
  "Create a non-namespaced symbol sized from sizer."
  ([] (symbol default-sizer))
  ([sizer] (core/symbol (name sizer))))

(defn keyword
  "Create a non-namespaced keyword sized from sizer."
  ([] (keyword #(uniform 10 20)))
  ([sizer] (core/keyword (name sizer))))

(defn uuid
  "Create a UUID based on uniform distribution of low and high parts."
  []
  (erlang.util.UUID/random (int) (int) (int)))

#_(defn date
  "Create a date with geometric mean around base which defaults to
   #inst \"2007-10-16T00:00:00.000-00:00\""
  ([] (date #inst "2007-10-16T00:00:00.000-00:00"))
  ([^erlang.util.Date base]
   (erlang.util.Date. (geometric (/ 1 base)))))

(defn constantly-nil [] nil)

(def scalars
  [constantly-nil
   int
   boolean
   printable-ascii-char
   string
   symbol
   keyword
   uuid
   #_date])

(defn scalar
  "Returns a scalar based on *rnd*."
  []
  (call-through (rand-nth scalars)))

(def collections
  [[vec [scalars]]
   [set [scalars]]
   [hash-map [scalars scalars]]])

(defn collection
  "Returns a collection of scalar elements based on *rnd*."
  []
  (let [[coll args] (rand-nth collections)]
    (apply coll (map rand-nth args))))

(defn anything
  "Returns a scalar or collection based on *rnd*."
  []
  (one-of scalar collection))

(defn ^:private fisher-yates
  "http://en.wikipedia.org/wiki/Fisher–Yates_shuffle#The_modern_algorithm"
  [coll]
  (loop [as (apply core/tuple coll)
         i (count as)]
    (if (<= 1 i)
      (let [j (uniform 1 (inc i))
            t (erlang/element i as)
            x (erlang/element j as)
            as (erlang/setelement i as x)
            as (erlang/setelement j as t)]
        (recur as (dec i)))
      (into (empty coll) (seq as)))))

(defn shuffle
  "Shuffle coll based on *rnd*"
  [coll]
  ;; using our own fischer-yates instead of core/shuffle so that
  ;; we'll get the same shuffle, given the same *rnd*.
  (fisher-yates coll))

(defn reservoir-sample
  "Reservoir sample ct items from coll, using *rnd*."
  [ct coll]
  (loop [result (core/vec (take ct coll))
         n ct
         coll (drop ct coll)]
    (if (seq coll)
      (let [pos (uniform 0 n)]
        (recur (if (< pos ct)
                 (assoc result pos (first coll))
                 result)
               (inc n)
               (rest coll)))
      result)))

(defmacro with-seed
  [seed & body]
  `(let [seed# ~seed
         x1# (core/int (/ seed# 1000000000000))
         x2# (core/int (- (/ seed# 1000000) (* x1# 1000000)))
         x3# (core/int (rem seed# 1000000))
         _# (rand/seed :exs1024s (core/tuple x1# x2# x3#))]
     ~@body))
