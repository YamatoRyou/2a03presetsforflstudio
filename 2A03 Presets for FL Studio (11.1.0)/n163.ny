;nyquist plug-in
;version 3
;type analyze
;name "N163/FDS waveform..."
;action "Calculating N163/FDS waveform..."

;control mode "Mode" choice "Namco 163,Famicom Disk System" 0
;control wavesize-choice "Wave size (N163 only)" choice "4,8,12,16,20,24,28,32,36,40,44,48,52,56,60,64,68,72,76,80,84,88,92,96,100,104,108,112,116,120,124,128,132,136,140,144,148,152,156,160,164,168,172,176,180,184,188,192,196,200,204,208,212,216,220,224,228,232,236,240" 7
;control normalize-choice "Normalize result" choice "yes,no" 0

; **********************************************************************
; Namco 163/Famicom Disk System waveform tool
; By Kef Schecter
; Public domain
; **********************************************************************

(setf *gc-flag* nil)

(setf mode-n163 0)
(setf mode-fds 1)

(setf normalize-yes 0)
(setf normalize-no 1)

(cond
  ((= mode mode-n163) ; Convert from wavesize-choice table to actual
                      ; wave size (4, 8, 12...)
                      (setf wavesize (* 4 (1+ wavesize-choice)))
                      (setf max-volume 15))
  (t (setf wavesize 64)                         ; FDS mode
     (setf max-volume 63)))

(setf half-max-volume (/ (+ max-volume 1) 2.0))

(setf new-old-ratio (/ (float wavesize) len))

(setf resample-rate (truncate (* *sound-srate* new-old-ratio)))


(defmacro push (item lst)
  `(setq ,lst (cons ,item ,lst)))


(defun process ()
  (let* ((resampled (resample s resample-rate))
         (normalized (if (= normalize-choice normalize-yes)
                         (normalize resampled)
                         resampled))
         (samples-array (snd-samples normalized wavesize))
         (samples-list '()))

    (dotimes (index wavesize)
      (push (n163-adjust (aref samples-array index)) samples-list))

    (reverse samples-list)
  )
)


(defun normalize (snd)
  (let ((maximum (peak snd NY:ALL)))
    (scale (/ 1.0 maximum) snd)))


; Converts value in range from -1.0 to 1.0 into an integer from 0 to max-volume
; Does not bother range-checking input
(defun n163-adjust (value)
  (min (float max-volume) (truncate (* (+ value 1.0) half-max-volume))))


(defun run ()
  (list (list 0 (string-trim "()" (format nil "~a" (process))))))


; Execution begins here
(if (arrayp s)
  "Stereo tracks are not supported at this time."
  (run))
