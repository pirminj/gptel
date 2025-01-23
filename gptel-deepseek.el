;;; gptel-deepseek.el --- Deepseek API backend for gptel  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  pirminj

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This file adds support for Deepseek's API to gptel

;;; Code:
(require 'cl-generic)
(eval-when-compile
  (require 'cl-lib))
(require 'map)
(require 'gptel)

(defvar json-object-type)

(cl-defstruct (gptel-deepseek (:constructor gptel--make-deepseek)
                             (:copier nil)
                             (:include gptel-openai)))

(defun gptel-deepseek--display-reasoning (text)
  (let ((buffer (get-buffer-create "*gptel-reasoning*")))
    (with-current-buffer buffer
      (goto-char (point-max))
      (insert text))
    (unless (get-buffer-window buffer)
      (display-buffer buffer '((display-buffer-in-side-window)
                               (side . right)
                               (window-width . 0.33))))))

(cl-defmethod gptel-curl--parse-stream ((_backend gptel-deepseek) info)
  (let* ((content-strs)
         (reasoning-strs))
    (while (re-search-forward "^data:" nil t)
      (save-match-data
        (if (looking-at " *\\[DONE\\]")
            ;; (apply #'concat (nreverse content-strs))
	    (message "done")
          (let* ((response (gptel--json-read))
		 (delta (map-nested-elt response '(:choices 0 :delta)))
		 (content (plist-get delta :content))
		 (reasoning (plist-get delta :reasoning_content)))
            (when (and reasoning (not (eq reasoning :null)))
              (push reasoning reasoning-strs)
              (gptel-deepseek--display-reasoning reasoning))
            (when (and content (not (eq content :null)))
              (push content content-strs))))))
    (if content-strs
	(apply #'concat (nreverse content-strs))
      "")))

(cl-defmethod gptel--parse-response ((_backend gptel-deepseek) response info)
  (let ((reasoning-string (map-nested-elt response '(:choices 0 :message :reasoning_content)))
	(response-string (map-nested-elt response '(:choices 0 :message :content))))
    (concat reasoning-string response-string)))


(cl-defmethod gptel--request-data ((_backend gptel-deepseek) prompts)
  "JSON encode PROMPTS for sending to DeepSeek."
  (let ((prompts-plist
         `(:model ,(gptel--model-name gptel-model)
           :messages [,@prompts]
           :stream ,(or gptel-stream :json-false))))
    (when (and gptel--system-message
               (not (gptel--model-capable-p 'nosystem)))
      (plist-put prompts-plist :system gptel--system-message))
    ;; (when gptel-temperature
    ;;   (plist-put prompts-plist :temperature gptel-temperature))
    (when gptel-max-tokens
      (plist-put prompts-plist :max_tokens gptel-max-tokens))
    (gptel--merge-plists
     prompts-plist
     (gptel-backend-request-params gptel-backend)
     (gptel--model-request-params gptel-model))))


;;;###autoload
(cl-defun gptel-make-deepseek
    (name &key curl-args stream key request-params
          (header
           (lambda () (when-let* (key (gptel--get-api-key))
                        `(("Authorization" . ,(concat "Bearer " key))))))
          (host "api.deepseek.com")
          (protocol "https")
          (models '(deepseek-reasoner))
          (endpoint "/chat/completions"))
  "Register a Deepseek API backend for gptel with NAME."
  (declare (indent 1))
  (let ((backend (gptel--make-deepseek
                  :curl-args curl-args
                  :name name
                  :host host
                  :header header
                  :key key
                  :models models
                  :protocol protocol
                  :endpoint endpoint
                  :stream stream
                  :request-params request-params
                  :url (concat protocol "://" host endpoint))))
    (prog1 backend
      (setf (alist-get name gptel--known-backends nil nil #'equal) backend))))

(provide 'gptel-deepseek)
;;; gptel-deepseek.el ends here
