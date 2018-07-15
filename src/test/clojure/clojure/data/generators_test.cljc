(ns clojure.data.generators-test
  (:require [clojure.data.generators :as gen]
            [clojure.test :refer (deftest is)]))

(defn print-read-roundtrip
  [o]
  (binding [*print-length* nil
            *print-level* nil]
    (-> o pr-str read-string)))

(defn check-print-read-roundtrip
  [o]
  (let [o2 (print-read-roundtrip o)]
    (when-not (= o o2)
      (throw (ex-info "Value cannot roundtrip, see ex-data" {:value o :roundtrip o2})))))

(deftest test-print-read-roundtrip
  (dotimes [_ 50]
    (check-print-read-roundtrip (gen/anything))))

(deftest test-shuffle
  (dotimes [_ 50]
    (let [coll (gen/vec gen/anything #(do 100))
          shuf (gen/shuffle coll)]
      (is (= (into #{} coll)
             (into #{} shuf))))))

(deftest test-reservoir-sample-consistency
  (dotimes [n 50]
    (let [coll (range 100)
          seed (erlang/system_time :microsecond)
          sample-1 (gen/with-seed seed
                     (gen/reservoir-sample 10 coll))
          sample-2 (gen/with-seed seed
                     (gen/reservoir-sample 10 coll))]
      (is (= sample-1 sample-2)))))
