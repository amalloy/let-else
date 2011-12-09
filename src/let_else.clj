(ns let-else)

(defmacro let?
  "Expands into a let, except where a binding is followed by :when <pred> or :else <else>
   or both, in either order.

   For a :when, the <pred> is evaluated after the associated binding is evaluated
   and must be truthy to continue evaluating the rest of the bindings and the body.
   If the <pred> is falsey, the <else> is the value of the let?, if present, or nil if not.

   For an :else without a :when, if the associated binding is falsey, <else> is the value of the let?."
  [bindings & body]
  (let [bindings (partition 2 bindings)
        sections (partition-between (fn [[[left] [right]]]
                                      (not (keyword? right)))
                                    bindings)]
    (reduce (fn [body section]
              (let [[[name val] & opts] section]
                (if-not opts
                  `(let [~name ~val]
                     ~body)
                  (let [tmp-name (gensym)
                        {:keys [when else]
                         :or {when tmp-name}}
                        (apply hash-map
                               (apply concat opts))]
                    `(let [~tmp-name ~val
                           ~name ~tmp-name]
                       (if ~when
                         ~body
                         ~else))))))
            `(do ~@body)
            (reverse sections))))