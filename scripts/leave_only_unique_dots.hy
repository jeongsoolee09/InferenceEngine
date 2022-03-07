(import os)
(import glob)


(defn glob-for-dots [directory]
  (.glob glob (os.path.join directory "*.dot")))


(defn read-files [filepaths]
  "returns an iterator of (filepath, contents) combo."
  (gfor filepath filepaths
        :setv content (with [f (open filepath "r+")]
                        (.read f))
        (, filepath content)))


(defn dedup-file [files-and-contents]
  "deduplicate the files-and-contents, leaving only unique files in terms of contents"
  (setv memory [])
  (for [(, file content) files-and-contents]
    (if (in content (list (map second memory)))
      (continue)
      (.append memory (, file content))))
  (list (map first memory)))


(defn remove-duplicate-file [files-and-contents]
  (setv all-files (list (map first files-and-contents)))
  (setv non-dups (dedup-file files-and-contents))
  (print (list non-dups))
  (print "=============")
  (for [file all-files]
    (when (not-in file non-dups)
      ;;(os.remove file)
      (print file)
      )))


(defmain []
  (setv directory "/Users/jslee/Dropbox/InferenceEngine/lib/")
  (setv globbed (glob-for-dots directory))
  ;; (print f"globbed = {globbed}")
  (setv files-and-contents (read-files globbed))
  ;; (print f"files-and-contents = {(list files-and-contents)}")
  (for [file (dedup-file files-and-contents)]
    (print file))
  ;; (remove-duplicate-file files-and-contents)
  )


;; (defmain []
;;   (->> "/Users/jslee/Dropbox/InferenceEngine/lib/"
;;        (glob-for-dots)
;;        (read-files)
;;        (remove-duplicate-file)))
