;;; spite.el --- REST API REPL

;; Copyright (C) 2013, 2014, 2015, 2016 Ian Eure

;; Author: Ian Eure <ian.eure@gmail.com>
;; Version: 0.1.0
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'json)
(require 'url)
(require 'url-http)
(require 'url-parse)
(require 'ielm)

(defvar spite-pp-function 'spite-pp
  "Function to pretty-print the result of the eval.")

(defvar spite-insert-function 'spite-insert
  "Function to insert the result into the spite buffer.")

(defvar spite-input-filter-pre-read-hooks nil
  "List of functions to filter the raw input before read")
(make-variable-buffer-local 'spite-input-filter-pre-read-hooks)

(defvar spite-input-filter-pre-eval-hooks nil
  "List of functions to filter input expr before eval.")
(make-variable-buffer-local 'spite-input-filter-pre-eval-hooks)


;; Vars

(defvar spite-base-url ""
  "Base URL of the API Spite will interact with.")
(make-variable-buffer-local 'spite-base-url)

(defvar spite-last-response-headers nil
  "Response headers from the most recent spite request")
(defvaralias '*~ 'spite-last-response-headers)

(defvar spite-last-url nil
  "Most recently requested spite URL")
(defvaralias '*~ 'spite-last-url)


;; Response readers

(defun spite-resp-nocontentp ()
  "Is this a 204 No Content response?"
  (= url-http-response-status 204))

(defun spite-resp-nocontent-reader ()
  "Read 204 body"
  :no-content)

(defun spite-resp-imagep ()
  "Is this an image we can display inline?"
  (image-type-from-file-name (url-recreate-url url-http-target-url)))

(defun spite-resp-image-reader ()
  "Read image data body into an image object."
  (let ((type (image-type-from-file-name
               (url-recreate-url url-http-target-url))))
    (search-forward "\n\n")
    (create-image (buffer-substring (point) (point-max)) type t)))

(defun spite-resp-textp ()
  "Is this a text response?"
  (string-match-p "^text/plain" url-http-content-type))

(defun spite-resp-text-reader ()
  "Read & parse JSON data."
  (search-forward "\n\n")
  (buffer-substring (point) (point-max)))

(defun spite-resp-jsonp ()
  "Is this a JSON response?"
  (string-match-p "^application/json" url-http-content-type))

(defun spite-resp-json-reader ()
  "Read & parse JSON data."
  (search-forward "\n\n")
  (json-read))

(defun spite-resp-xmlp ()
  (string-match-p "^\\(application\\|text\\)/xml" url-http-content-type))

(defun spite-resp-xml-reader ()
  (search-forward "\n\n")
  (xml-parse-region))

(defun spite-resp-defaultp ()
  t)

(defun spite-resp-default-reader ()
  (error (format "No reader available for type `%s'" url-http-content-type)))

(defun spite-resp-readerp (reader)
  "Can READER read the current response?"
  (save-excursion
    (save-match-data
      (with-demoted-errors
          (funcall reader)))))

(defvar spite-response-readers
  '((spite-resp-nocontentp . spite-resp-nocontent-reader)
    (spite-resp-jsonp . spite-resp-json-reader)
    (spite-resp-imagep . spite-resp-image-reader)
    (spite-resp-xmlp . spite-resp-xml-reader)
    (spite-resp-textp . spite-resp-text-reader)
    (spite-resp-defaultp . spite-resp-default-reader))
  "Alist of response readers. Each entry takes the form (PREDF . READER);
   if PREDF returns non-nil (with the response buffer active), READER
   will be called to read the body.")

(defun spite-read-response ()
  "Find a reader and use it to return the response."
  (let ((reader))
    (dolist (reader spite-response-readers)
      (when (spite-resp-readerp (car reader))
        (return (funcall (cdr reader)))))))



(defun spite-make-url-fixup-objects (objects)
  "Turn keyword symbols into strings."
  (mapcar
   (lambda (obj)
     (cond ((keywordp obj) (substring (symbol-name obj) 1))
           (t obj)))
   objects))

(defun spite/sym->str (thing)
  "Turn symbols or keywords into strings."
  (cond ((keywordp thing) (substring (symbol-name thing) 1))
        ((symbolp thing) (symbol-name thing))
        ((stringp thing) thing)
        (t (error "Don't know what to do with this!"))))

(defun spite-make-url (path &rest objects)
  "Make an API URL.

   This works similar to `format'. PATH is a format string, and
   OBJECTS contains arguments for the format. If the last element of
   OBJECTS is a non-empty list, it is used to build the query-string
   arguments of the URL.

   The generated URL is prefixed with the value of `spite-base-url'.
   "
  (let* ((maybe-qargs (car (last objects)))
         (qargs (and (listp maybe-qargs) maybe-qargs))
         (objs (spite-make-url-fixup-objects
                (if qargs (butlast objects) objects)))
         (path (apply 'format (or path "") objs)))
    (concat spite-base-url path
            (when qargs
              (concat "?" (url-build-query-string qargs))))))

(cl-defun spite/req-url (url)
  "Make a request to a URL.

   API is the name of the service to call, for example \"user\".
   PATH is a format string representing the endpoint path within that
   service. OBJECTS are substituted into it, just like `format'. If
   the last element of OBJECTS is a list, it will provide pairs of
   query-string arguments.

   Returns the response body parsed into JSON on success, or raises an
   error on non-2xx or non-JSON response."
  (let* ((replbuf (current-buffer)))
    (setq spite-last-url url)
    (with-current-buffer (url-retrieve-synchronously url)
      ;; Save headers
      (let ((headers (save-restriction
                       (mail-narrow-to-head)
                       (buffer-substring (point-min) (point-max)))))
        (with-current-buffer replbuf
          (setq spite-last-response-headers headers)))
      (unless (and (>= url-http-response-status 200)
                   (<= url-http-response-status 299))
        (let ((body (buffer-substring (point-min) (point-max))))
          (pop-to-buffer (concat (buffer-name replbuf) " ERROR"))
          (spite-error-mode)
          (let ((buffer-read-only))
            (erase-buffer)
            (save-excursion
              (insert body)
              (spite-error-highlight-buffer))))
        (error (format "%s: %s" url
                       (buffer-substring (point-min) (line-end-position)))))

      (spite-read-response))))

(defun spite/req (path &rest objects)
  "Make a request to a REST API.

   API is the name of the service to call, for example \"user\".
   PATH is a format string representing the endpoint path within that
   service. OBJECTS are substituted into it, just like `format'. If
   the last element of OBJECTS is a list, it will provide pairs of
   query-string arguments.

   Returns the response body parsed into JSON on success, or raises an
   error on non-2xx or non-JSON response."
  (spite/req-url (apply 'spite-make-url path objects)))

(defun spite/normalize (body type)
  (condp body
         ((string= "applicaton/json") (json-encode body))
         (t body)))

(defmacro spite/send-json (method body &rest exprs)
  `(let ((url-request-method (upcase (spite/sym->str ,method)))
         (url-request-extra-headers '(("Content-Type" . "application/json; charset=utf-8")))
         (url-request-body (if (listp body) (json-encode ,body)
                             body)))
     ,@exprs))

(cl-defun spite/post ((path objects) &key body (type "application/json"))
  (let ((url-request-method "POST")
        (url-request-data (normalize body type)))
    (apply #'spite/req path objects)))

(cl-defun spite/put ((path objects) &key body (type "application/json"))
  (let ((url-request-method "PUT")
        (url-request-data (normalize body type)))
    (apply #'spite/req path objects)))

;; Repl

(defvar inferior-spite-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\t" 'comint-dynamic-complete)
    (define-key map "\C-m" 'spite-return)
    (define-key map "\C-j" 'spite-send-input)
    (define-key map "\e\C-x" 'eval-defun)         ; for consistency with
    (define-key map "\e\t" 'completion-at-point)  ; lisp-interaction-mode
    ;; These bindings are from `lisp-mode-shared-map' -- can you inherit
    ;; from more than one keymap??
    (define-key map "\e\C-q" 'indent-sexp)
    (define-key map "\177" 'backward-delete-char-untabify)
    (make-composed-keymap (list map) comint-mode-map))
  "Keymap for spite mode.")

(defvar spite-api-input)

(defun spite-pp (object)
  (if (spite-imagep object)
      object
    (pp-to-string object)))

(defun spite-handle-pp (object)
  (condition-case nil
      ;; Self-referential objects cause loops in the printer, so
      ;; trap quits here. May as well do errors, too
      (setq res (funcall spite-pp-function object))
    (error (setq ielm-error-type "IELM Error")
           (setq ielm-result "Error during pretty-printing (bug in pp)"))
    (quit  (setq ielm-error-type "IELM Error")
           (setq ielm-result "Quit during pretty-printing"))))

(defun spite-insert (result)
  (if (spite-imagep result)
      (progn
        (insert-image result)
        (when (image-animated-p result)
          (image-animate result))
        (set-marker (process-mark (ielm-process)) (point)))
    (comint-output-filter (ielm-process) result)))

(defun spite-input-sender (_proc input)
  ;; Just sets the variable ielm-input, which is in the scope of
  ;; `ielm-send-input's call.
  (setq spite-input input))

(defun spite-imagep (result)
  (and (listp result) (eq 'image (car result))))

(defun spite-return (&optional for-effect)
  "Newline and indent, or evaluate the sexp before the prompt.
Complete sexps are evaluated; for incomplete sexps inserts a newline
and indents.  If however `ielm-dynamic-return' is nil, this always
simply inserts a newline."
  (interactive)
  (if ielm-dynamic-return
      (let ((state
             (save-excursion
               (end-of-line)
               (parse-partial-sexp (ielm-pm)
                                   (point)))))
        (if (and (< (car state) 1) (not (nth 3 state)))
            (spite-send-input)
          (when (and ielm-dynamic-multiline-inputs
                     (save-excursion
                       (beginning-of-line)
                       (looking-at-p comint-prompt-regexp)))
            (save-excursion
              (goto-char (ielm-pm))
              (newline 1)))
          (newline-and-indent)))
    (newline)))

(defun spite-filter-across (input funcs)
  "Filter the input through funcs."
  (let ((val input))
    (mapcar (lambda (f)
              (setq val (funcall f val)))
            funcs)
    val))

(defun spite-filter-string (input-string)
  "Filter the input-string through `spite-input-filter-pre-read-hooks'."
  (spite-filter-across input-string spite-input-filter-pre-read-hooks))

(defun spite-filter-forms (input-forms)
  "Filter the input-forms through `spite-input-filter-pre-eval-hooks'."
  (spite-filter-across input-forms spite-input-filter-pre-eval-hooks))

(defun spite-eval-input (input-string)
  "Evaluate the Lisp expression INPUT-STRING, and pretty-print the result."
  ;; This is the function that actually `sends' the input to the
  ;; `inferior Lisp process'. All comint-send-input does is works out
  ;; what that input is.  What this function does is evaluates that
  ;; input and produces `output' which gets inserted into the buffer,
  ;; along with a new prompt.  A better way of doing this might have
  ;; been to actually send the output to the `cat' process, and write
  ;; this as in output filter that converted sexps in the output
  ;; stream to their evaluated value.  But that would have involved
  ;; more process coordination than I was happy to deal with.
  ;;
  ;; NOTE: all temporary variables in this function will be in scope
  ;; during the eval, and so need to have non-clashing names.
  (let ((ielm-string input-string)      ; input expression, as a string
        ielm-form           ; form to evaluate
        ielm-pos            ; End posn of parse in string
        ielm-result         ; Result, or error message
        ielm-error-type         ; string, nil if no error
        (ielm-output "")        ; result to display
        (ielm-pmark (ielm-pm)))
    (unless (ielm-is-whitespace-or-comment ielm-string)
      (condition-case err
          (let ((rout (read-from-string (spite-filter-string ielm-string))))
            (setq ielm-form (spite-filter-forms (car rout))
                  ielm-pos (cdr rout)))
        (error (setq ielm-result (error-message-string err))
               (setq ielm-error-type "Read error")))
      (unless ielm-error-type
        ;; Make sure working buffer has not been killed
        (if (ielm-is-whitespace-or-comment (substring ielm-string ielm-pos))
            ;; To correctly handle the ielm-local variables *,
            ;; ** and ***, we need a temporary buffer to be
            ;; current at entry to the inner of the next two let
            ;; forms.  We need another temporary buffer to exit
            ;; that same let.  To avoid problems, neither of
            ;; these buffers should be alive during the
            ;; evaluation of ielm-form.
            (let ((*1 *)
                  (*2 **)
                  (*3 ***)
                  ielm-temp-buffer)
              (set-match-data ielm-match-data)
              (save-excursion
                (condition-case err
                    (unwind-protect
                        ;; The next let form creates default
                        ;; bindings for *, ** and ***.  But
                        ;; these default bindings are
                        ;; identical to the ielm-local
                        ;; bindings.  Hence, during the
                        ;; evaluation of ielm-form, the
                        ;; ielm-local values are going to be
                        ;; used in all buffers except for
                        ;; other ielm buffers, which override
                        ;; them.  Normally, the variables *1,
                        ;; *2 and *3 also have default
                        ;; bindings, which are not overridden.
                        (let ((* *1)
                              (** *2)
                              (*** *3))
                          (setq ielm-result
                                (eval ielm-form lexical-binding))))
                  (error (setq ielm-result (error-message-string err))
                         (setq ielm-error-type "Eval error"))
                  (quit (setq ielm-result "Quit during evaluation")
                        (setq ielm-error-type "Eval error"))))
              (setq ielm-match-data (match-data)))
          (setq ielm-error-type "IELM error")
          (setq ielm-result "More than one sexp in input")))

      (goto-char ielm-pmark)

      (let (pp-result)
        (unless ielm-error-type
          (setq pp-result (spite-handle-pp ielm-result)))
        (if ielm-error-type
            (progn
              (when ielm-noisy (ding))
              (setq ielm-output (concat ielm-output "*** " ielm-error-type " ***  "))
              (setq ielm-output (concat ielm-output ielm-result)))
          ;; There was no error, so shift the *** values
          (setq *** **)
          (setq ** *)
          (setq * ielm-result))
        (setq ielm-output (concat ielm-output "\n"))
        (funcall spite-insert-function pp-result)))

    (setq ielm-output (concat ielm-output ielm-prompt-internal))
    (comint-output-filter (ielm-process) ielm-output)))

(defun spite-send-input (&optional for-effect)
  "Evaluate the Emacs Lisp expression after the prompt."
  (interactive)
  (let (spite-input)                    ; set by spite-input-sender
    (comint-send-input)                 ; update history, markers etc.
    (spite-eval-input spite-input)))

(define-derived-mode inferior-spite-mode inferior-emacs-lisp-mode
  "Spite"
  "Major mode for interacting with REST APIs."
  (setq comint-input-sender 'spite-input-sender)
  (use-local-map inferior-spite-mode-map))

(defmacro defspite (name base-url &rest body)
  "Create a REPL for a REST API."
  (let* ((strname (symbol-name name))
         (modesym (intern (format "%s-mode" strname)))
         (modemapsym (intern (format "%s-mode-map" strname)))
         (bufname (format "*%s*" strname)))
    `(progn
       (define-derived-mode ,modesym inferior-spite-mode ,strname
         ,(format "Major mode for interacting with the %s API." name)
         (use-local-map ,modemapsym)
         ,@body)

       (defun ,name ()
         ,(format "Interact with the %s API" strname)
         (interactive)
         (let ((old-point)
               (bufname ,bufname))
           (unless (comint-check-proc bufname)
             (with-current-buffer (get-buffer-create bufname)
               (unless (zerop (buffer-size)) (setq old-point (point)))
               (let ((ielm-header "")
                     (ielm-prompt ,(format "%s> " strname)))
                 (,modesym))
               (setq spite-base-url ,base-url)))
           (switch-to-buffer bufname)
           (when old-point (push-mark old-point)))))))

(defun spite ()
  "Interact with REST APIs"
  (interactive)
  (let ((old-point)
        (bufname "*spite*"))
    (unless (comint-check-proc bufname)
      (with-current-buffer (get-buffer-create bufname)
        (unless (zerop (buffer-size)) (setq old-point (point)))
        (let ((ielm-header "")
              (ielm-prompt "spite> "))
          (inferior-spite-mode))
        (setq ielm-working-buffer (current-buffer))))
    (switch-to-buffer bufname)
    (when old-point (push-mark old-point))))

(defvar spite-error-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "q" 'quit-window)
    (define-key map "n" 'next-line)
    (define-key map "p" 'previous-line)
    map))

(defgroup spite-error nil
  "Major mode for displaying Spite errors."
  :prefix "spite-error-"
  :group 'applications)

(defface spite-status-face
  '((t (:inherit font-lock-function-name-face)))

  "Face used for Spite HTTP status lines"
  :group 'spite-error)

(defface spite-error-header-name-face
  '((t (:inherit font-lock-variable-name-face)))

  "Face used for Spite HTTP header names."
  :group 'spite-error)

(define-derived-mode spite-error-mode fundamental-mode "Spite Error"
  "Major mode for displaying Spite errors."

  (use-local-map spite-error-mode-map)
  (setq buffer-read-only t))

(defun spite-error-replace (http-buf error-buf)
  (let ((body (with-current-buffer http-buf
                (buffer-substring (point-min)
                                  (point-max)))))
    (with-current-buffer error-buf
      (let ((buffer-read-only nil))
        (widen)
        (erase-buffer)
        (insert body)))))

(defun spite-error-highlight-buffer ()
  (save-excursion
    (widen)
    (goto-char (point-min))
    (let ((header-end-pos (save-excursion (search-forward "\n\n")))
          (status (make-overlay (line-beginning-position) (line-end-position))))
      (overlay-put status 'face 'spite-status-face)
      (while (re-search-forward "^[^:]+:" header-end-pos t)
        (let ((header (make-overlay (line-beginning-position) (match-end 0))))
          (overlay-put header 'face 'spite-error-header-name-face))))))

(provide 'spite)
;;; spite.el ends here
