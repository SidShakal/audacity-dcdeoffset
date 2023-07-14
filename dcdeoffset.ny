; ------------------------------------------------------------------------------
; MIT License
; 
; Copyright (c) 2023 Sidney Shakal
; 
; Permission is hereby granted, free of charge, to any person obtaining a copy
; of this software and associated documentation files (the "Software"), to deal
; in the Software without restriction, including without limitation the rights
; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
; copies of the Software, and to permit persons to whom the Software is
; furnished to do so, subject to the following conditions:
; 
; The above copyright notice and this permission notice shall be included in all
; copies or substantial portions of the Software.
; 
; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
; SOFTWARE.
; ------------------------------------------------------------------------------

;nyquist plug-in
;version 4
;codetype lisp
;name "DC Deoffset"
;author "Sid Shakal"
;release 1.0.0
;copyright "Relesed under terms of the MIT License."
;type "process"
;debugbutton enabled
;;debugflags trace
;preview disabled

;control text "DC Deoffset - Subtract a DC offset from a signal"
;control text "Suggested use:\n1. Select silence containing a DC offset.\n2. Run this plugin with the 'Analyze selection' action selected.\n3. Select audio to apply offset correction to.\n4. Run this plugin with the 'Apply correction from analysis' action selected.\n\nAlternative use for manual corrections:\n1. Select audio to apply offset correction to.\n2. Run this plugin with the 'Apply correction from slider(s)' action selected and the slider(s) set to the offset(s) to correct for."
;control action "Action" choice "Analyze selection,Apply correction from analysis,Apply correction from slider(s)" 0
;control left-slider "Left offset" float "" 0.0 -1.0 1.0
;control right-slider "Right offset" float "(ignored if selection is mono)" 0.0 -1.0 1.0

; there are way more comments in here than there should be, and they are way
;   more verbose than is probably necessary, but i don't really want to have to
;   relearn everything if i have to tweak something later.
; useful resources i used while writing this script:
;   https://plugins.audacityteam.org/contributing/developing-your-own-plugins-and-scripts/creating-your-own-nyquist-plugins
;   http://www.cs.cmu.edu/~rbd/doc/nyquist/
;   https://www.audacity-forum.de/download/edgar/nyquist/nyquist-doc/xlisp/xlisp-ref/xlisp-ref-index.htm
; -- Sid, 13 Jul 2023

; define a function to encapsulate the analysis mode.
(defun analyze-selection (track len)
    ; purpose:
    ;   take the mean of all samples in selection to determine the dc offset and
    ;   save it for use in a second invocation of the plugin.
    ; plan:
    ;   if selection is stereo,
    ;     calculate mean of samples in left channel of selection. this is the
    ;       left offset.
    ;     calculate mean of samples in right channel of selection. this is the
    ;       right offset.
    ;     store the offsets for later.
    ;     display the offsets.
    ;   else the selection is mono,
    ;     calculate mean value of samples in selection. this is the offset.
    ;     store the offset for later. (store it as both the left offset and the
    ;       right offset, to make life easier and in case the user wants to use
    ;       a mono reference to remove an offset from a stereo selection for
    ;       some reason.)
    ;     display the offset.
    
    ; define the mean calculation function.
    (defun calculate-mean (sound n-samples)
        (do
            (
                ; initialize running-sum to 0.0
                (running-sum 0.0)
                ; initialize current-sample to the next sample in sound, and at
                ;   the end of each iteration, set it to the next sample in
                ;   sound.
                (current-sample (snd-fetch sound) (snd-fetch sound))
            )
            (
                ; iteration condition:
                ;   loop until there are no more samples to fetch. ('snd-fetch'
                ;   returns nil when all samples have been fetched. 'not'
                ;   returns T if its argument is nil, and nil otherwise.)
                (not current-sample)
                ; return value:
                ;   at loop end, return the running sum divided by the total
                ;   number of samples.
                (/ running-sum n-samples)
            )
            ; on each iteration, add the current sample value to the running
            ;   sum.
            (setq running-sum (+ running-sum current-sample))
        )
    )
    
    ; delete any existing stored offset data.
    (remprop '*scratch* 'dcdeoffset-stored-offsets)
    
    ; if selection is stereo,
    (if (arrayp track)
        (let
            ; declare local variables.
            (left-mean right-mean offsets)
            ; calculate mean of all samples in left channel of selection.
            ;   this is the left channel offset.
            (setq left-mean (calculate-mean (aref track 0) len))
            ; calculate mean of all samples in right channel of selection.
            ;   this is the right channel offset.
            (setq right-mean (calculate-mean (aref track 1) len))
            ; create offsets array to contain the calculated means.
            (setq offsets (vector left-mean right-mean))
            ; store the offsets array into the dcdeoffset-stored-offsets
            ;   property of the *scratch* global provided by audacity for data
            ;   persistence across plugin invocations.
            (putprop '*scratch* offsets 'dcdeoffset-stored-offsets)
            ; return the offsets via message.
            (format nil
                "Calculated DC offsets:~%Left offset: ~A~%Right offset: ~A"
                left-mean
                right-mean
            )
        )
    ; otherwise, selection is mono,
        (let
            ; declare local variables.
            (mean offsets)
            ; calculate the mean of all samples in the selection.
            ;   this is the offset.
            (setq mean (calculate-mean track len))
            ; create offsets array to contain the calculated offsets, and fill
            ;   it with the same value for the left channel and right channel
            ;   offsets.
            (setq offsets (vector mean mean))
            ; store the offsets array into the dcdeoffset-stored-offsets
            ;   property of the *scratch* global provided by audacity for data
            ;   persistence across plugin invocations.
            (putprop '*scratch* (vector mean mean) 'dcdeoffset-stored-offsets)
            ; return the offset via message.
            (format nil
                "Calculated DC offset:~%~A"
                mean
            )
        )
    )
)

; define a function to encapsulate the apply-from-analysis mode.
(defun apply-from-analysis (track)
    ; purpose:
    ;   apply a corrective offset to the selected audio, based on the dc offsets
    ;   calculated by a previous run of the plugin.
    ; plan:
    ;   if valid stored offsets are unavailable,
    ;     return error message suggesting user run the analysis first.
    ;   else stored offsets are available,
    ;     if selection is stereo,
    ;       offset the left channel by the negative of the stored left offset.
    ;       offset the right channel by the negative of the stored right offset.
    ;       return modified selection.
    ;     else selection is mono,
    ;       offset the sound by the negative of the stored left offset.
    ;       return modified selection.
    
    ; define a function to validate stored offsets. probably a little over-the-
    ;   top, but a little paranoia is fine.
    (defun offsets-are-valid (offsets)
        ; offsets must be an array of two float values in the interval [-1.0,
        ;   1.0]. anything else is invalid.
        (and
            (arrayp offsets)
            (eql (length offsets) 2)
            (floatp (aref offsets 0))
            (<= -1.0 (aref offsets 0) 1.0)
            (floatp (aref offsets 1))
            (<= -1.0 (aref offsets 1) 1.0)
        )
    )
    
    ; i-don't-lisp note: 'let' expressions are a way to do local variables.
    (let
        (
            ; load any offsets stored by a prior analysis into local variable
            ;   stored-offsets.
            (stored-offsets (get '*scratch* 'dcdeoffset-stored-offsets))
        )
        ; if no offsets were stored, or if for some reason the data stored where
        ;   the offsets should be stored is not a valid set of stored offsets,
        (if (not (offsets-are-valid stored-offsets))
            ; return a message suggesting the user run the analysis mode.
            (strcat
                "No analysis results are stored. Please select a region of "
                "silence to calculate the DC offset from and rerun this plugin "
                "in 'Analyze selection' mode."
            )
        ; else, the stored offsets are valid and usable,
            (let
                (
                    (left-offset (aref stored-offsets 0))
                    (right-offset (aref stored-offsets 1))
                )
                ; if selection is stereo,
                (if (arrayp track)
                    ; subtract the left offset from the left channel sound,
                    ;   subtract the right offset from the right channel sound,
                    ;   and return an array of the two resulting sounds.
                    (vector
                        (diff (aref track 0) left-offset)
                        (diff (aref track 1) right-offset)
                    )
                ; else selection is mono,
                    ; subtract the left offset from the lone sound, and return
                    ;   the resulting sound.
                    (diff track left-offset)
                )
            )
        )
    )
)

; define a function to encapsulate the apply-from-sliders mode.
(defun apply-from-sliders (track)
    ; purpose:
    ;   apply a corrective offset to the selected audio based on the sliders in
    ;   the plugin interface.
    ; plan:
    ;   if selection is stereo,
    ;     offset the left channel by the negative of the left slider value.
    ;     offset the right channel by the negative of the right slider value.
    ;     return the modified audio.
    ;   else selection is mono,
    ;     offset the sound by the negative of the left slider value.
    ;     return the modified audio.
    
    ; if selection is stereo,
    (if (arrayp track)
        ; subtract the left slider's value from the left channel sound,
        ;   subtract the right slider's value from the right channel sound,
        ;   and return an array of the two resulting sounds.
        (vector
            (diff (aref track 0) left-slider)
            (diff (aref track 1) right-slider)
        )
    ; else selection is mono,
        ; subtract the left slider's value from the lone sound, and return the
        ;   resulting sound.
        (diff track left-slider)
    )
)


;;; Main

; call the appropriate function for the selected mode.
;   (*track* and len are provided to nyquist plugins by audacity.)
(case action
    (0 (analyze-selection *track* len))
    (1 (apply-from-analysis *track*))
    (2 (apply-from-sliders *track*))
)
