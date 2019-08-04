;;; org-clock-graph.el --- Visualize Org mode clocks -*- lexical-binding: t; -*-

;; Version: 0.1
;; Package-Requires: ((org "9.0") (cl-lib "0.3"))

;;; Commentary:
;;; TODO: Commentary

;;; Code:

;;; TODO: Should all of these things be required?
(require 'org)
(require 'org-agenda)
(require 'org-clock)
(require 'org-macs)
(require 'cl-lib)
(require 'subr-x)

(cl-defstruct (org-clock-graph-block-entry
               (:constructor org-clock-graph-make-block-entry))
  name                                  ;Name of the entry
  tod-minutes                           ;Time of day in which entry was clocked
  duration                              ;How long entry was clocked for
  intangible                            ;Whether to skip block when `cursor-intangible-mode' is enabled
  width                                 ;Width of the block in pixels
  hd-marker)                            ;Marker pointing to actual entry


(defun org-clock-graph-color-hash (obj)
  "Hash OBJ, returning a string of a hex color code."
  (concat "#" (cl-subseq (secure-hash 'md5 obj) -6)))

(defun org-clock-graph-tod-to-minutes (time-of-day)
  "Convert an Org TIME-OF-DAY to the Nth minute of today."
  (let ((tod-hours (/ time-of-day 100))
        (tod-minutes (% time-of-day 100)))
    (+ (* 60 tod-hours) tod-minutes)))

(defun org-clock-graph-current-minute ()
  "Return the current time as an Nth minute of today."
  (let ((curr-time (decode-time)))
    (+ (nth 1 curr-time)
       (* 60 (nth 2 curr-time)))))


(defun org-clock-graph-make-structure ()
  "Make a block entry from the text properties at point.

Returns nil if the current entry does not have a valid duration."
  (cl-flet ((get-prop (prop) (get-text-property (point) prop)))
    (let* ((tod (get-prop 'time-of-day))
           (minutes (org-clock-graph-tod-to-minutes tod))
           (duration
            (floor
             (or (when-let ((prop-duration (get-prop 'duration)))
                   ;; A negative duration means the entry was clocked out the
                   ;; next day and the clock is less than 24 hours.
                   ;; FIXME: We need to be able to tell if a clock is more than
                   ;; 24 hours, since it will have a positive duration.
                   (if (< prop-duration 0)
                       (- 1440 minutes)
                     prop-duration))
                 ;; If the entry is currently clocked in, it ends this minute.
                 (when (and (org-clocking-p)
                            (equal org-clock-hd-marker (get-prop 'org-hd-marker)))
                   (- (org-clock-graph-current-minute) minutes))))))
      ;; TODO: I'm not sure if we need this check. Can a clocked entry show up
      ;; in the agenda if it doesn't have an end time and isn't currently
      ;; clocked in?
      (when duration
        (org-clock-graph-make-block-entry
         :name (substring-no-properties (get-prop 'txt))
         :tod-minutes minutes
         :duration duration
         :hd-marker (get-prop 'org-hd-marker))))))

(defun org-clock-graph-day-blocks ()
  "Get a list of blocks for a given day in the agenda, sorted by time of day.

This function assumes that point is at the start of an agenda
date header."
  (save-excursion
   (let (total
         (limit (next-single-property-change (point) 'day)))
     (while (< (point) limit)
       (when (equal (org-get-at-bol 'type) "clock")
         (push
          (org-clock-graph-make-structure)
          total))
       (forward-line))
     (cl-sort total '< :key 'org-clock-graph-block-entry-tod-minutes))))

(defun org-clock-graph-create-break (first second)
  "Create a break between FIRST and SECOND.

The break is an empty block filling up the time between the end
of the first clock and the start of the second."
  (let* ((first-start (org-clock-graph-block-entry-tod-minutes first))
         (first-end (+ first-start (org-clock-graph-block-entry-duration first)))
         (second-start (org-clock-graph-block-entry-tod-minutes second)))
    (when (> second-start first-end)
      (org-clock-graph-make-block-entry
       :tod-minutes first-end
       :duration (- second-start first-end)))))

(defun org-clock-graph-insert-breaks (block-list)
  (cl-flet ((red-func (first others)
                      (let* ((second (car others))
                             (insertion
                              (and second
                                   (org-clock-graph-create-break first second))))
                        (if insertion
                            (cons first (cons insertion others))
                          (cons first others)))))
    (cl-reduce #'red-func block-list :from-end t :initial-value nil)))

(defun org-clock-graph-cap-blocks (block-list)
  "Ensure that BLOCK-LIST spans 24 hours."
  (let* ((first (car block-list))
         (last (car (last block-list)))
         (begin-duration (org-clock-graph-block-entry-tod-minutes
                          first))
         (end-tod (+ (org-clock-graph-block-entry-duration last)
                     (org-clock-graph-block-entry-tod-minutes last)))
         (new-first (org-clock-graph-make-block-entry
                     :tod-minutes 0
                     :duration begin-duration))
         (new-last (org-clock-graph-make-block-entry
                    :tod-minutes end-tod
                    :duration (- 1440 end-tod))))
    (cons new-first (append block-list (list new-last)))))

(defun org-clock-graph-set-intangibles (block-list)
  "Set the`:intangible' property of elements in BLOCK-LIST.

This allows skipping over empty blocks when
`cursor-intangible-mode' is enabled in the agenda buffer.
This function modifies the input list."
  (cl-loop for (first second) on block-list
           when second
           when (equal nil (org-clock-graph-block-entry-name second))
           do (setf (org-clock-graph-block-entry-intangible first) t)
           finally (return block-list)))

(defun org-clock-graph-set-widths (target-width block-list)
  (let* ((newlst (cl-sort (cl-copy-list block-list) '>
                          :key #'org-clock-graph-block-entry-duration))
         ;; Set the widths, making sure that each block is at least 1px wide
         (actual-widths
          (mapcar
           (lambda (b)
             (setf (org-clock-graph-block-entry-width b)
                   (round (max 1.0
                               (* target-width
                                  (/ (org-clock-graph-block-entry-duration b)
                                     1440.0))))))
           newlst))
         ;; Calculate how far off we are from our target width
         (width-difference (- target-width (cl-reduce #'+ actual-widths))))
    ;; Adjust widths within list to match target width
    ;; TODO: We can make adjustments based on the ratio of the current value to
    ;; the next -- the proportion taken out of width-difference on this
    ;; ratio
    (cl-flet ((transfer (from amount)
                    (cl-incf (org-clock-graph-block-entry-width from) amount)
                    (cl-decf width-difference amount)))
     (cl-loop for curr in newlst
              with diff = (if (< width-difference 0) -1 1)
              while (/= width-difference 0)
              do (transfer curr diff)
              finally do (transfer (car newlst) width-difference))
     ;; Can't finally do and finally return inside a cl-loop
     block-list)))


(defmacro org-clock-graph-make-sensor (headline-mark)
  "Create a function that highlights headlines with HEADLINE-MARK within the agenda buffer.

The generated function is used by `cursor-intangible-functions'"
  `(lambda (_window _oldpos dir)
    (cl-case dir
      ;; Some of this code was taken from `org-agenda-mark-clocking-task'
      (entered
       (let (ov s)
         (save-excursion
           (while (setq s (next-single-property-change (point) 'org-hd-marker))
           (goto-char s)
           (when (equal (org-get-at-bol 'org-hd-marker) ,headline-mark)
             (setq ov (make-overlay (point-at-bol) (1+ (point-at-eol))))
             (overlay-put ov 'type 'org-agenda-graph-highlight)
             ;; TODO: Custom face
             (overlay-put ov 'face 'org-agenda-clocking))))))
      (left
       (mapc (lambda (o)
               (when (eq (overlay-get o 'type) 'org-agenda-graph-highlight)
                 (delete-overlay o)))
             (overlays-in (point-min) (point-max)))))))

(defun org-clock-graph-create-string (block-struct)
  "Create the string inserted into the agenda from BLOCK-STRUCT."
  ;; TODO: This name could be set in the initial creation
  (let ((name (or (org-clock-graph-block-entry-name block-struct) "none")))
    (propertize " "
                'face `(:background ,(org-clock-graph-color-hash name))
                'cursor-intangible (org-clock-graph-block-entry-intangible
                                    block-struct)
                'display `(space :width (,(org-clock-graph-block-entry-width
                                           block-struct)))
                'cursor-sensor-functions (list
                                          (org-clock-graph-make-sensor
                                           (org-clock-graph-block-entry-hd-marker block-struct)))
                'help-echo (org-clock-graph-block-entry-name block-struct))))

(defun org-clock-graph-final-str ()
  "Return the final string to insert next to a particular date header.

This function assumes that point is at the start of an agenda
date header."
  (when-let (blocks (org-clock-graph-day-blocks))
    (mapconcat #'org-clock-graph-create-string
                   (org-clock-graph-set-widths
                    ;; FIXME: Selected window != current buffer window
                    ;; Check this to make sure it's right
                    (- (window-pixel-width (selected-window))
                       (apply #'+
                              (cl-remove-if-not
                               'identity
                               (window-fringes (selected-window)))))
                    (org-clock-graph-set-intangibles
                     (org-clock-graph-cap-blocks
                      (org-clock-graph-insert-breaks blocks))))
                   "")))

(defun org-clock-graph-insert-graphs ()
  "Insert clock graphs after each date header in the current agenda buffer."
  (save-excursion
    (let (pos)
      (while (setq pos (text-property-any
                        (point) (point-max) 'org-agenda-date-header t))
        (goto-char pos)
        (let ((str (org-clock-graph-final-str)))
          (end-of-line)
          ;; TODO: This needs to be have the same properties as the external text
          (insert (concat "\n" str)))
        (forward-line)))))

;;;###autoload
(defun org-clock-graph ()
  "Insert graphs into the agenda buffer.

This docstring really needs improvement."
  (when (not (eq major-mode 'org-agenda-mode))
    (user-error "Error: org-clock-graph must be run inside an agenda buffer!"))
  (cursor-sensor-mode)
  (cursor-intangible-mode)
  ;; This save-excursion shouldn't be here
  (save-excursion
    (goto-char (point-min))
    (let ((inhibit-read-only t))
      (org-clock-graph-insert-graphs))))

(provide 'org-clock-graph)
;;; org-clock-graph.el ends here
