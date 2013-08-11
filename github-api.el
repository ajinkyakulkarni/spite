;;; github-api.el --- GitHub API REPL

;; Copyright (C) 2013  Ian Eure

;; Author: Ian Eure <ian.eure@gmail.com>
;; Keywords: tools, convenience

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

;; Example REPL for the GitHub API.

;;; Code:

(require 'spite)
(require 'nspace)

(defun github-api-namespace (form)
  `(with-ns github ,form))

(defspite github-api "https://api.github.com/"
  (add-hook 'spite-input-filter-pre-eval-hooks 'github-api-namespace))

(defun github/repo (owner repo)
  (spite/req "repos/%s/%s" owner repo))

(defun github/repo/issues (owner repo)
  (spite/req "repos/%s/%s/issues" owner repo))

(defun github/issue (owner repo issue)
  (spite/req "repos/%s/%s/issues/%d" owner repo issue))

(defun github/repos (owner)
  (spite/req "users/%s/repos" owner))

(defun github/org (org)
  (spite/req "orgs/%s" org))

(defun github/org/members (org)
  (spite/req "orgs/%s/members" org))

(provide 'github-api)
;;; github-api.el ends here
