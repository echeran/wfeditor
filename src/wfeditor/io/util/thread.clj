(ns wfeditor.io.util.thread
  (:require
   [wfeditor.io.util.const :as io-const]))

(defn do-and-sleep-repeatedly-bg-thread
  "a function that takes an input fn (and optionally a time, in millisecs) and returns a future that is also a running, newly-created background thread.   do-and-sleep-repeatedly takes the same inputs as the input fn.  the future is initialized with an anonymous function that performs the input fn (with the provided args) and sleeps for the given amount of time, repeatedly"
  ([sleep-time f & args]
     (future
       (dorun
         (repeatedly
          (fn []
            (apply f args)
            (Thread/sleep sleep-time)))))))