;;; godot-theme.el --- A Godot theme for emacs

;; Copyright (C) 2023

;; Author: John Long
;; Version: 1.0.0

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
;; A port of the Godot theme based on Monokai.
;;
;;; Credits:
;;
;; Wimer Hazenberg created the original theme.
;; - http://www.monokai.nl/blog/2006/07/15/textmate-color-theme/
;;
;; Bozhidar Batsov created zenburn-theme.el and solarized-theme.el
;;  on which this file is based.
;; - https://github.com/bbatsov/zenburn-emacs
;;
;; Color Scheme Designer 3 for complementary colours.
;; - http://colorschemedesigner.com/
;;
;; Xterm 256 Color Chart
;; - https://upload.wikimedia.org/wikipedia/en/1/15/Xterm_256color_chart.svg
;;
;; K. Adam Christensen for his personal monokai theme that addresses 256 colours.
;; - https://github.com/pope/personal/blob/master/etc/emacs.d/monokai-theme.el
;;
;; Thomas Frössman for his work on solarized-emacs.
;; - http://github.com/bbatsov/solarized-emacs
;;
;;; Code:

(unless (>= emacs-major-version 24)
  (error "The godot theme requires Emacs 24 or later!"))

(deftheme godot "The Godot colour theme")

(defgroup godot nil
  "Godot theme options.
The theme has to be reloaded after changing anything in this group."
  :group 'faces)

(defcustom godot-distinct-fringe-background nil
  "Make the fringe background different from the normal background color.
Also affects 'linum-mode' background."
  :type 'boolean
  :group 'godot)

(defcustom godot-use-variable-pitch nil
  "Use variable pitch face for some headings and titles."
  :type 'boolean
  :group 'godot)

(defcustom godot-doc-face-as-comment nil
  "Consider `font-lock-doc-face' as comment instead of a string."
  :type 'boolean
  :group 'godot
  :package-version "3.5.1")

(defcustom godot-height-minus-1 0.8
  "Font size -1."
  :type 'number
  :group 'godot)

(defcustom godot-height-plus-1 1.1
  "Font size +1."
  :type 'number
  :group 'godot)

(defcustom godot-height-plus-2 1.15
  "Font size +2."
  :type 'number
  :group 'godot)

(defcustom godot-height-plus-3 1.2
  "Font size +3."
  :type 'number
  :group 'godot)

(defcustom godot-height-plus-4 1.3
  "Font size +4."
  :type 'number
  :group 'godot)

;; Primary colors
(defcustom godot-yellow "#FFEDA1" ; #E6DB74
  "Primary colors - yellow"
  :type 'string
  :group 'godot)

(defcustom godot-orange "#FD971F" ; #FD971F
  "Primary colors - orange"
  :type 'string
  :group 'godot)

(defcustom godot-red "#FF7085" ; #F92672
  "Primary colors - red"
  :type 'string
  :group 'godot)

(defcustom godot-magenta "#ff8ccc" ; "#FD5FF0"
  "Primary colors - magenta"
  :type 'string
  :group 'godot)

(defcustom godot-blue "#abc9ff" ; "#66D9EF"
  "Primary colors - blue"
  :type 'string
  :group 'godot)

(defcustom godot-green "#42ffc2" ; "#A6E22E"
  "Primary colors - green"
  :type 'string
  :group 'godot)

(defcustom godot-cyan "#57b3ff" ; "#A1EFE4"
  "Primary colors - cyan"
  :type 'string
  :group 'godot)

(defcustom godot-violet "#a3a3f5" ; "#AE81FF"
  "Primary colors - violet"
  :type 'string
  :group 'godot)

(defcustom godot-gray "#64645E"
  "Primary colors - gray"
  :type 'string
  :group 'godot)

(defcustom godot-foreground "#cdcfd2" ; "#F8F8F2"
  "Adaptive colors - foreground"
  :type 'string
  :group 'godot)

(defcustom godot-background "#0D1219" ; #272822
  "Adaptive colors - background"
  :type 'string
  :group 'godot)

(defcustom godot-comments "#8F908A" ; "#75715E"
  "Adaptive colors - comments"
  :type 'string
  :group 'godot)

(defcustom godot-emphasis "#F8F8F0"
  "Adaptive colors - emphasis"
  :type 'string
  :group 'godot)

(defcustom godot-line-number "#8F908A"
  "Adaptive colors - line number"
  :type 'string
  :group 'godot)

(defcustom godot-highlight "#49483E"
  "Adaptive colors - highlight"
  :type 'string
  :group 'godot)

(defcustom godot-highlight-alt "#3E3D31"
  "Adaptive colors - highlight"
  :type 'string
  :group 'godot)

(defcustom godot-highlight-line "#3C3D37"
  "Adaptive colors - line highlight"
  :type 'string
  :group 'godot)

(let* (;; Variable pitch
       (godot-pitch (if godot-use-variable-pitch
                        'variable-pitch
                      'default))

       ;; Definitions for guis that support 256 colors
       (godot-class '((class color) (min-colors 257)))

       ;; Functionality specific colors
       (godot-diff-blue-base      "#232438")
       (godot-diff-blue-emphasis  "#1F204E")
       (godot-diff-green-base     "#233E1E")
       (godot-diff-green-emphasis "#1F541A")
       (godot-diff-red-base       "#3D241E")
       (godot-diff-red-emphasis   "#53201A")

       ;; Darker and lighter accented colors
       (godot-yellow-d       "#BEB244")
       (godot-yellow-l       "#FFF7A8")
       (godot-orange-d       "#D47402")
       (godot-orange-l       "#FFAC4A")
       (godot-red-d          "#F70057")
       (godot-red-l          "#FA518D")
       (godot-magenta-d      "#FB35EA")
       (godot-magenta-l      "#FE8CF4")
       (godot-violet-d       "#945AFF")
       (godot-violet-l       "#C9ACFF")
       (godot-blue-d         "#40CAE4")
       (godot-blue-l         "#92E7F7")
       (godot-cyan-d         "#74DBCD")
       (godot-cyan-l         "#D3FBF6")
       (godot-green-d        "#86C30D")
       (godot-green-l        "#BBEF53")
       (godot-gray-d         "#35331D")
       (godot-gray-l         "#7B7962")
       ;; Adaptive higher/lower contrast accented colors
       (godot-foreground-hc  "#141414")
       (godot-foreground-lc  "#171A0B")
       ;; High contrast colors
       (godot-yellow-hc      "#FFFACE")
       (godot-yellow-hc-alt  "#E7DB74")
       (godot-yellow-lc      "#9A8F21")
       (godot-orange-hc      "#FFBE74")
       (godot-orange-lc      "#A75B00")
       (godot-red-hc         "#FEB0CC")
       (godot-red-hc-alt     "#F83535")
       (godot-red-lc         "#F20055")
       (godot-magenta-hc     "#FEC6F9")
       (godot-magenta-lc     "#F309DF")
       (godot-violet-hc      "#F0E7FF")
       (godot-violet-lc      "#7830FC")
       (godot-blue-hc        "#CAF5FD")
       (godot-blue-lc        "#1DB4D0")
       (godot-cyan-hc        "#D3FBF6")
       (godot-cyan-lc        "#4BBEAE")
       (godot-green-hc       "#CCF47C")
       (godot-green-hc-alt   "#A6E22C")
       (godot-green-lc       "#679A01")

       ;; Distinct fringe
       (godot-fringe-bg (if godot-distinct-fringe-background
                            godot-gray
                          godot-background))

       ;; Definitions for terminals that do not support 256 colors
       (godot-256-class '((class color) (min-colors 89)))

       ;; Functionality specific colors
       (godot-256-diff-blue-base      "#00005f")
       (godot-256-diff-blue-emphasis  "#000087")
       (godot-256-diff-green-base     "#005800")
       (godot-256-diff-green-emphasis "#008700")
       (godot-256-diff-red-base       "#5f0000")
       (godot-256-diff-red-emphasis   "#870000")

       ;; Primary colors
       (godot-256-yellow         "#CDC673")
       (godot-256-orange         "#FF8C00")
       (godot-256-red            "#FF1493")
       (godot-256-magenta        "#D700D7")
       (godot-256-violet         "#AF87FF")
       (godot-256-blue           "#5FD7FF")
       (godot-256-cyan           "#5FFFFF")
       (godot-256-green          "#87D700")
       (godot-256-gray           "#3D3D3D")
       ;; Darker and lighter accented colors
       (godot-256-yellow-d       "#878700")
       (godot-256-yellow-l       "#FFFF87")
       (godot-256-orange-d       "#AF5F00")
       (godot-256-orange-l       "#FFAF5F")
       (godot-256-red-d          "#870000")
       (godot-256-red-l          "#FF5F87")
       (godot-256-magenta-d      "#AF0087")
       (godot-256-magenta-l      "#FF87DF")
       (godot-256-violet-d       "#5F00AF")
       (godot-256-violet-l       "#AF87D7")
       (godot-256-blue-d         "#008787")
       (godot-256-blue-l         "#87D7FF")
       (godot-256-cyan-d         "#5FAFAF")
       (godot-256-cyan-l         "#AFFFFF")
       (godot-256-green-d        "#5F8700")
       (godot-256-green-l        "#AFD700")
       (godot-256-gray-d         "#333333")
       (godot-256-gray-l         "#707070")
       ;; Adaptive colors
       (godot-256-foreground     "#F5F5F5")
       (godot-256-background     "#1B1E1C")
       (godot-256-comments       "#8B8878")
       (godot-256-emphasis       "#FFFAFA")
       (godot-256-line-number    "#8F908A")
       (godot-256-highlight      "#474747")
       (godot-256-highlight-alt  "#3E3E3E")
       (godot-256-highlight-line "#000000")
       ;; Adaptive higher/lower contrast accented colors
       (godot-256-foreground-hc  "#171A0B")
       (godot-256-foreground-lc  "#141414")
       ;; High contrast colors
       (godot-256-yellow-hc      godot-256-yellow-d)
       (godot-256-yellow-lc      godot-256-yellow-l)
       (godot-256-orange-hc      godot-256-orange-d)
       (godot-256-orange-lc      godot-256-orange-l)
       (godot-256-red-hc         godot-256-red-d)
       (godot-256-red-lc         godot-256-red-l)
       (godot-256-magenta-hc     godot-256-magenta-d)
       (godot-256-magenta-lc     godot-256-magenta-l)
       (godot-256-violet-hc      godot-256-violet-d)
       (godot-256-violet-lc      godot-256-violet-l)
       (godot-256-blue-hc        godot-256-blue-d)
       (godot-256-blue-lc        godot-256-blue-l)
       (godot-256-cyan-hc        godot-256-cyan-d)
       (godot-256-cyan-lc        godot-256-cyan-l)
       (godot-256-green-hc       godot-256-green-d)
       (godot-256-green-lc       godot-256-green-l)

       ;; Distinct fringe
       (godot-256-fringe-bg (if godot-distinct-fringe-background
                                godot-256-gray
                              godot-256-background)))

  ;; Define faces
  (custom-theme-set-faces
   'godot

   ;; font lock for syntax highlighting
   `(font-lock-builtin-face
     ((,godot-class (:foreground ,godot-red
                                 :weight normal))
      (,godot-256-class (:foreground ,godot-256-red
                                     :weight normal))))

   `(font-lock-comment-delimiter-face
     ((,godot-class (:foreground ,godot-comments))
      (,godot-256-class (:foreground ,godot-256-comments))))

   `(font-lock-comment-face
     ((,godot-class (:foreground ,godot-comments))
      (,godot-256-class (:foreground ,godot-256-comments))))

   `(font-lock-constant-face
     ((,godot-class (:foreground ,godot-violet))
      (,godot-256-class (:foreground ,godot-256-violet))))

   `(font-lock-doc-face
     ((,godot-class (:foreground ,(if godot-doc-face-as-comment
                                      godot-comments
                                    godot-yellow)))
      (,godot-256-class (:foreground ,(if godot-doc-face-as-comment
                                          godot-256-comments
                                        godot-256-yellow)))))

   `(font-lock-function-name-face
     ((,godot-class (:foreground ,godot-cyan))
      (,godot-256-class (:foreground ,godot-256-cyan))))

   `(font-lock-keyword-face
     ((,godot-class (:foreground ,godot-red
                                 :weight normal))
      (,godot-256-class (:foreground ,godot-256-red
                                     :weight normal))))

   `(font-lock-negation-char-face
     ((,godot-class (:foreground ,godot-yellow
                                 :weight bold))
      (,godot-256-class (:foreground ,godot-256-yellow
                                     :weight bold))))

   `(font-lock-preprocessor-face
     ((,godot-class (:foreground ,godot-red))
      (,godot-256-class (:foreground ,godot-256-red))))

   `(font-lock-regexp-grouping-construct
     ((,godot-class (:foreground ,godot-yellow
                                 :weight normal))
      (,godot-256-class (:foreground ,godot-256-yellow
                                     :weight normal))))

   `(font-lock-regexp-grouping-backslash
     ((,godot-class (:foreground ,godot-violet
                                 :weight normal))
      (,godot-256-class (:foreground ,godot-256-violet
                                     :weight normal))))

   `(font-lock-string-face
     ((,godot-class (:foreground ,godot-yellow))
      (,godot-256-class (:foreground ,godot-256-yellow))))

   `(font-lock-type-face
     ((,godot-class (:foreground ,godot-green
                                 :italic nil))
      (,godot-256-class (:foreground ,godot-256-green
                                     :italic nil))))

   `(font-lock-variable-name-face
     ((,godot-class (:foreground ,godot-orange))
      (,godot-256-class (:foreground ,godot-256-orange))))

   `(font-lock-warning-face
     ((,godot-class (:foreground ,godot-orange
                                 :weight bold
                                 :italic t
                                 :underline t))
      (,godot-256-class (:foreground ,godot-256-orange
                                     :weight bold
                                     :italic t
                                     :underline t))))

   `(c-annotation-face
     ((,godot-class (:inherit font-lock-constant-face))
      (,godot-256-class (:inherit font-lock-constant-face))))

   ;; general colouring
   '(button ((t (:underline t))))

   `(default
     ((,godot-class (:foreground ,godot-foreground
                                 :background ,godot-background))
      (,godot-256-class (:foreground ,godot-256-foreground
                                     :background ,godot-256-background))))

   `(highlight
     ((,godot-class (:background ,godot-highlight))
      (,godot-256-class (:background ,godot-256-highlight))))

   `(lazy-highlight
     ((,godot-class (:inherit highlight
                              :background ,godot-highlight-alt))
      (,godot-256-class (:inherit highlight
                                  :background ,godot-256-highlight-alt))))

   `(region
     ((,godot-class (:inherit highlight
                              :background ,godot-highlight))
      (,godot-256-class (:inherit highlight
                                  :background ,godot-256-highlight))))

   `(secondary-selection
     ((,godot-class (:inherit region
                              :background ,godot-highlight-alt))
      (,godot-256-class (:inherit region
                                  :background ,godot-256-highlight-alt))))

   `(shadow
     ((,godot-class (:foreground ,godot-comments))
      (,godot-256-class (:foreground ,godot-256-comments))))

   `(match
     ((,godot-class (:background ,godot-green
                                 :foreground ,godot-background
                                 :weight bold))
      (,godot-256-class (:background ,godot-256-green
                                     :foreground ,godot-256-background
                                     :weight bold))))

   `(cursor
     ((,godot-class (:foreground ,godot-background
                                 :background ,godot-foreground
                                 :inverse-video t))
      (,godot-256-class (:foreground ,godot-256-background
                                     :background ,godot-256-foreground
                                     :inverse-video t))))

   `(mouse
     ((,godot-class (:foreground ,godot-background
                                 :background ,godot-foreground
                                 :inverse-video t))
      (,godot-256-class (:foreground ,godot-256-background
                                     :background ,godot-256-foreground
                                     :inverse-video t))))

   `(escape-glyph
     ((,godot-class (:foreground ,godot-comments))
      (,godot-256-class (:foreground ,godot-256-comments))))

   `(escape-glyph-face
     ((,godot-class (:foreground ,godot-comments))
      (,godot-256-class (:foreground ,godot-256-comments))))

   `(fringe
     ((,godot-class (:foreground ,godot-foreground
                                 :background ,godot-fringe-bg))
      (,godot-256-class (:foreground ,godot-256-foreground
                                     :background ,godot-256-fringe-bg))))

   `(link
     ((,godot-class (:foreground ,godot-blue
                                 :underline t
                                 :weight bold))
      (,godot-256-class (:foreground ,godot-256-blue
                                     :underline t
                                     :weight bold))))

   `(link-visited
     ((,godot-class (:foreground ,godot-violet
                                 :underline t
                                 :weight normal))
      (,godot-256-class (:foreground ,godot-256-violet
                                     :underline t
                                     :weight normal))))

   `(success
     ((,godot-class (:foreground ,godot-green ))
      (,godot-256-class (:foreground ,godot-256-green ))))

   `(warning
     ((,godot-class (:foreground ,godot-yellow ))
      (,godot-256-class (:foreground ,godot-256-yellow ))))

   `(error
     ((,godot-class (:foreground ,godot-red))
      (,godot-256-class (:foreground ,godot-256-red))))

   `(eval-sexp-fu-flash
     ((,godot-class (:foreground ,godot-background
                                 :background ,godot-green))
      (,godot-256-class (:foreground ,godot-256-background
                                     :background ,godot-256-green))))

   `(eval-sexp-fu-flash-error
     ((,godot-class (:foreground ,godot-background
                                 :background ,godot-red))
      (,godot-256-class (:foreground ,godot-256-background
                                     :background ,godot-256-red))))

   `(trailing-whitespace
     ((,godot-class (:background ,godot-red))
      (,godot-256-class (:background ,godot-256-red))))

   `(vertical-border
     ((,godot-class (:foreground ,godot-gray))
      (,godot-256-class (:foreground ,godot-256-gray))))

   `(menu
     ((,godot-class (:foreground ,godot-foreground
                                 :background ,godot-background))
      (,godot-256-class (:foreground ,godot-256-foreground
                                     :background ,godot-256-background))))

   `(minibuffer-prompt
     ((,godot-class (:foreground ,godot-blue))
      (,godot-256-class (:foreground ,godot-256-blue))))

   ;; mode-line and powerline
   `(mode-line-buffer-id
     ((,godot-class (:foreground ,godot-green
                                 :weight bold))
      (,godot-256-class (:foreground ,godot-256-green
                                     :weight bold))))

   `(mode-line
     ((,godot-class (:inverse-video unspecified
                                    :underline unspecified
                                    :foreground ,godot-emphasis
                                    :background ,godot-highlight
                                    :box (:line-width 1
                                                      :color ,godot-gray
                                                      :style unspecified)))
      (,godot-256-class (:inverse-video unspecified
                                        :underline unspecified
                                        :foreground ,godot-256-foreground
                                        :background ,godot-256-background
                                        :box (:line-width 1
                                                          :color ,godot-256-highlight
                                                          :style unspecified)))))

   `(powerline-active1
     ((,godot-class (:background ,godot-gray-d))
      (,godot-256-class (:background ,godot-256-gray-d))))

   `(powerline-active2
     ((,godot-class (:background ,godot-background))
      (,godot-256-class (:background ,godot-256-background))))


   `(mode-line-inactive
     ((,godot-class (:inverse-video unspecified
                                    :underline unspecified
                                    :foreground ,godot-comments
                                    :background ,godot-background
                                    :box (:line-width 1
                                                      :color ,godot-gray
                                                      :style unspecified)))
      (,godot-256-class (:inverse-video unspecified
                                        :underline unspecified
                                        :foreground ,godot-256-comments
                                        :background ,godot-256-background
                                        :box (:line-width 1
                                                          :color ,godot-256-gray
                                                          :style unspecified)))))

   `(powerline-inactive1
     ((,godot-class (:background ,godot-gray-d))
      (,godot-256-class (:background ,godot-256-gray-d))))

   `(powerline-inactive2
     ((,godot-class (:background ,godot-background))
      (,godot-256-class (:background ,godot-256-background))))

   ;; header-line
   `(header-line
     ((,godot-class (:foreground ,godot-emphasis
                                 :background ,godot-highlight
                                 :box (:color ,godot-gray
                                              :line-width 1
                                              :style unspecified)))
      (,godot-256-class (:foreground ,godot-256-emphasis
                                     :background ,godot-256-highlight
                                     :box (:color ,godot-256-gray
                                                  :line-width 1
                                                  :style unspecified)))))

   ;; cua
   `(cua-global-mark
     ((,godot-class (:background ,godot-yellow
                                 :foreground ,godot-background))
      (,godot-256-class (:background ,godot-256-yellow
                                     :foreground ,godot-256-background))))

   `(cua-rectangle
     ((,godot-class (:inherit region))
      (,godot-256-class (:inherit region))))

   `(cua-rectangle-noselect
     ((,godot-class (:inherit secondary-selection))
      (,godot-256-class (:inherit secondary-selection))))

   ;; diary
   `(diary
     ((,godot-class (:foreground ,godot-yellow))
      (,godot-256-class (:foreground ,godot-256-yellow))))

   ;; dired
   `(dired-directory
     ((,godot-class (:foreground ,godot-blue))
      (,godot-256-class (:foreground ,godot-256-blue))))

   `(dired-flagged
     ((,godot-class (:foreground ,godot-red))
      (,godot-256-class (:foreground ,godot-256-red))))

   `(dired-header
     ((,godot-class (:foreground ,godot-blue
                                 :background ,godot-background
                                 :inherit bold))
      (,godot-256-class (:foreground ,godot-256-blue
                                     :background ,godot-256-background
                                     :inherit bold))))

   `(dired-ignored
     ((,godot-class (:inherit shadow))
      (,godot-256-class (:inherit shadow))))

   `(dired-mark
     ((,godot-class (:foreground ,godot-green
                                 :weight bold))
      (,godot-256-class (:foreground ,godot-256-green
                                     :weight bold))))

   `(dired-marked
     ((,godot-class (:foreground ,godot-violet
                                 :inherit bold))
      (,godot-256-class (:foreground ,godot-256-violet
                                     :inherit bold))))

   `(dired-perm-write
     ((,godot-class (:foreground ,godot-foreground
                                 :underline t))
      (,godot-256-class (:foreground ,godot-256-foreground
                                     :underline t))))

   `(dired-symlink
     ((,godot-class (:foreground ,godot-cyan
                                 :slant italic))
      (,godot-256-class (:foreground ,godot-256-cyan
                                     :slant italic))))

   `(dired-warning
     ((,godot-class (:foreground ,godot-orange
                                 :underline t))
      (,godot-256-class (:foreground ,godot-256-orange
                                     :underline t))))

   ;; dropdown
   `(dropdown-list-face
     ((,godot-class (:background ,godot-highlight-line
                                 :foreground ,godot-blue))
      (,godot-256-class (:background ,godot-256-highlight-line
                                     :foreground ,godot-256-blue))))

   `(dropdown-list-selection-face
     ((,godot-class (:background ,godot-green
                                 :foreground ,godot-background))
      (,godot-256-class (:background ,godot-256-green
                                     :foreground ,godot-256-background))))

   ;; ecb
   `(ecb-default-highlight-face
     ((,godot-class (:background ,godot-blue
                                 :foreground ,godot-background))
      (,godot-256-class (:background ,godot-256-blue
                                     :foreground ,godot-256-background))))

   `(ecb-history-bucket-node-dir-soure-path-face
     ((,godot-class (:inherit ecb-history-bucket-node-face
                              :foreground ,godot-yellow))
      (,godot-256-class (:inherit ecb-history-bucket-node-face
                                  :foreground ,godot-256-yellow))))

   `(ecb-source-in-directories-buffer-face
     ((,godot-class (:inherit ecb-directories-general-face
                              :foreground ,godot-foreground))
      (,godot-256-class (:inherit ecb-directories-general-face
                                  :foreground ,godot-256-foreground))))

   `(ecb-history-dead-buffer-face
     ((,godot-class (:inherit ecb-history-general-face
                              :foreground ,godot-comments))
      (,godot-256-class (:inherit ecb-history-general-face
                                  :foreground ,godot-256-comments))))

   `(ecb-directory-not-accessible-face
     ((,godot-class (:inherit ecb-directories-general-face
                              :foreground ,godot-comments))
      (,godot-256-class (:inherit ecb-directories-general-face
                                  :foreground ,godot-256-comments))))

   `(ecb-bucket-node-face
     ((,godot-class (:inherit ecb-default-general-face
                              :weight normal
                              :foreground ,godot-blue))
      (,godot-256-class (:inherit ecb-default-general-face
                                  :weight normal
                                  :foreground ,godot-256-blue))))

   `(ecb-tag-header-face
     ((,godot-class (:background ,godot-highlight-line))
      (,godot-256-class (:background ,godot-256-highlight-line))))

   `(ecb-analyse-bucket-element-face
     ((,godot-class (:inherit ecb-analyse-general-face
                              :foreground ,godot-green))
      (,godot-256-class (:inherit ecb-analyse-general-face
                                  :foreground ,godot-256-green))))

   `(ecb-directories-general-face
     ((,godot-class (:inherit ecb-default-general-face
                              :height 1.0))
      (,godot-256-class (:inherit ecb-default-general-face
                                  :height 1.0))))

   `(ecb-method-non-semantic-face
     ((,godot-class (:inherit ecb-methods-general-face
                              :foreground ,godot-cyan))
      (,godot-256-class (:inherit ecb-methods-general-face
                                  :foreground ,godot-256-cyan))))

   `(ecb-mode-line-prefix-face
     ((,godot-class (:foreground ,godot-green))
      (,godot-256-class (:foreground ,godot-256-green))))

   `(ecb-tree-guide-line-face
     ((,godot-class (:inherit ecb-default-general-face
                              :foreground ,godot-gray
                              :height 1.0))
      (,godot-256-class (:inherit ecb-default-general-face
                                  :foreground ,godot-256-gray
                                  :height 1.0))))

   ;; ee
   `(ee-bookmarked
     ((,godot-class (:foreground ,godot-emphasis))
      (,godot-256-class (:foreground ,godot-256-emphasis))))

   `(ee-category
     ((,godot-class (:foreground ,godot-blue))
      (,godot-256-class (:foreground ,godot-256-blue))))

   `(ee-link
     ((,godot-class (:inherit link))
      (,godot-256-class (:inherit link))))

   `(ee-link-visited
     ((,godot-class (:inherit link-visited))
      (,godot-256-class (:inherit link-visited))))

   `(ee-marked
     ((,godot-class (:foreground ,godot-magenta
                                 :weight bold))
      (,godot-256-class (:foreground ,godot-256-magenta
                                     :weight bold))))

   `(ee-omitted
     ((,godot-class (:foreground ,godot-comments))
      (,godot-256-class (:foreground ,godot-256-comments))))

   `(ee-shadow
     ((,godot-class (:inherit shadow))
      (,godot-256-class (:inherit shadow))))

   ;; grep
   `(grep-context-face
     ((,godot-class (:foreground ,godot-foreground))
      (,godot-256-class (:foreground ,godot-256-foreground))))

   `(grep-error-face
     ((,godot-class (:foreground ,godot-red
                                 :weight bold
                                 :underline t))
      (,godot-256-class (:foreground ,godot-256-red
                                     :weight bold
                                     :underline t))))

   `(grep-hit-face
     ((,godot-class (:foreground ,godot-orange))
      (,godot-256-class (:foreground ,godot-256-orange))))

   `(grep-match-face
     ((,godot-class (:foreground ,godot-green
                                 :weight bold))
      (,godot-256-class (:foreground ,godot-256-green
                                     :weight bold))))

   ;; isearch
   `(isearch
     ((,godot-class (:inherit region
                              :foreground ,godot-background
                              :background ,godot-yellow))
      (,godot-256-class (:inherit region
                                  :foreground ,godot-256-background
                                  :background ,godot-256-yellow))))

   `(isearch-fail
     ((,godot-class (:inherit isearch
                              :foreground ,godot-red
                              :background ,godot-background
                              :bold t))
      (,godot-256-class (:inherit isearch
                                  :foreground ,godot-256-red
                                  :background ,godot-256-background
                                  :bold t))))


   ;; ace-jump-mode
   `(ace-jump-face-background
     ((,godot-class (:foreground ,godot-comments
                                 :background ,godot-background
                                 :inverse-video nil))
      (,godot-256-class (:foreground ,godot-256-comments
                                     :background ,godot-256-background
                                     :inverse-video nil))))

   `(ace-jump-face-foreground
     ((,godot-class (:foreground ,godot-yellow
                                 :background ,godot-background
                                 :inverse-video nil
                                 :weight bold))
      (,godot-256-class (:foreground ,godot-256-yellow
                                     :background ,godot-256-background
                                     :inverse-video nil
                                     :weight bold))))

   ;; auctex
   `(font-latex-bold-face
     ((,godot-class (:inherit bold
                              :foreground ,godot-emphasis))
      (,godot-256-class (:inherit bold
                                  :foreground ,godot-256-emphasis))))

   `(font-latex-doctex-documentation-face
     ((,godot-class (:background unspecified))
      (,godot-256-class (:background unspecified))))

   `(font-latex-doctex-preprocessor-face
     ((,godot-class
       (:inherit (font-latex-doctex-documentation-face
                  font-lock-builtin-face
                  font-lock-preprocessor-face)))
      (,godot-256-class
       (:inherit (font-latex-doctex-documentation-face
                  font-lock-builtin-face
                  font-lock-preprocessor-face)))))

   `(font-latex-italic-face
     ((,godot-class (:inherit italic :foreground ,godot-emphasis))
      (,godot-256-class (:inherit italic :foreground ,godot-256-emphasis))))

   `(font-latex-math-face
     ((,godot-class (:foreground ,godot-violet))
      (,godot-256-class (:foreground ,godot-256-violet))))

   `(font-latex-sectioning-0-face
     ((,godot-class (:inherit font-latex-sectioning-1-face
                              :height ,godot-height-plus-1))
      (,godot-256-class (:inherit font-latex-sectioning-1-face
                                  :height ,godot-height-plus-1))))

   `(font-latex-sectioning-1-face
     ((,godot-class (:inherit font-latex-sectioning-2-face
                              :height ,godot-height-plus-1))
      (,godot-256-class (:inherit font-latex-sectioning-2-face
                                  :height ,godot-height-plus-1))))

   `(font-latex-sectioning-2-face
     ((,godot-class (:inherit font-latex-sectioning-3-face
                              :height ,godot-height-plus-1))
      (,godot-256-class (:inherit font-latex-sectioning-3-face
                                  :height ,godot-height-plus-1))))

   `(font-latex-sectioning-3-face
     ((,godot-class (:inherit font-latex-sectioning-4-face
                              :height ,godot-height-plus-1))
      (,godot-256-class (:inherit font-latex-sectioning-4-face
                                  :height ,godot-height-plus-1))))

   `(font-latex-sectioning-4-face
     ((,godot-class (:inherit font-latex-sectioning-5-face
                              :height ,godot-height-plus-1))
      (,godot-256-class (:inherit font-latex-sectioning-5-face
                                  :height ,godot-height-plus-1))))

   `(font-latex-sectioning-5-face
     ((,godot-class (:inherit ,godot-pitch
                              :foreground ,godot-yellow
                              :weight bold))
      (,godot-256-class (:inherit ,godot-pitch :
                                  foreground ,godot-256-yellow
                                  :weight bold))))

   `(font-latex-sedate-face
     ((,godot-class (:foreground ,godot-emphasis))
      (,godot-256-class (:foreground ,godot-256-emphasis))))

   `(font-latex-slide-title-face
     ((,godot-class (:inherit (,godot-pitch font-lock-type-face)
                              :weight bold
                              :height ,godot-height-plus-3))
      (,godot-256-class (:inherit (,godot-pitch font-lock-type-face)
                                  :weight bold
                                  :height ,godot-height-plus-3))))

   `(font-latex-string-face
     ((,godot-class (:foreground ,godot-cyan))
      (,godot-256-class (:foreground ,godot-256-cyan))))

   `(font-latex-subscript-face
     ((,godot-class (:height ,godot-height-minus-1))
      (,godot-256-class (:height ,godot-height-minus-1))))

   `(font-latex-superscript-face
     ((,godot-class (:height ,godot-height-minus-1))
      (,godot-256-class (:height ,godot-height-minus-1))))

   `(font-latex-verbatim-face
     ((,godot-class (:inherit fixed-pitch
                              :foreground ,godot-foreground
                              :slant italic))
      (,godot-256-class (:inherit fixed-pitch
                                  :foreground ,godot-256-foreground
                                  :slant italic))))

   `(font-latex-warning-face
     ((,godot-class (:inherit bold
                              :foreground ,godot-orange))
      (,godot-256-class (:inherit bold
                                  :foreground ,godot-256-orange))))

   ;; auto-complete
   `(ac-candidate-face
     ((,godot-class (:background ,godot-highlight-line
                                 :foreground ,godot-blue))
      (,godot-256-class (:background ,godot-256-highlight-line
                                     :foreground ,godot-256-blue))))

   `(ac-selection-face
     ((,godot-class (:background ,godot-blue
                                 :foreground ,godot-background))
      (,godot-256-class (:background ,godot-256-blue
                                     :foreground ,godot-256-background))))

   `(ac-candidate-mouse-face
     ((,godot-class (:background ,godot-blue
                                 :foreground ,godot-background))
      (,godot-256-class (:background ,godot-256-blue
                                     :foreground ,godot-256-background))))

   `(ac-completion-face
     ((,godot-class (:foreground ,godot-emphasis
                                 :underline t))
      (,godot-256-class (:foreground ,godot-256-emphasis
                                     :underline t))))

   `(ac-gtags-candidate-face
     ((,godot-class (:background ,godot-highlight-line
                                 :foreground ,godot-blue))
      (,godot-256-class (:background ,godot-256-highlight-line
                                     :foreground ,godot-256-blue))))

   `(ac-gtags-selection-face
     ((,godot-class (:background ,godot-blue
                                 :foreground ,godot-background))
      (,godot-256-class (:background ,godot-256-blue
                                     :foreground ,godot-256-background))))

   `(ac-yasnippet-candidate-face
     ((,godot-class (:background ,godot-highlight-line
                                 :foreground ,godot-yellow))
      (,godot-256-class (:background ,godot-256-highlight-line
                                     :foreground ,godot-256-yellow))))

   `(ac-yasnippet-selection-face
     ((,godot-class (:background ,godot-yellow
                                 :foreground ,godot-background))
      (,godot-256-class (:background ,godot-256-yellow
                                     :foreground ,godot-256-background))))

   ;; auto highlight symbol
   `(ahs-definition-face
     ((,godot-class (:foreground ,godot-background
                                 :background ,godot-blue))
      (,godot-256-class (:foreground ,godot-256-background
                                     :background ,godot-256-blue))))

   `(ahs-edit-mode-face
     ((,godot-class (:foreground ,godot-background
                                 :background ,godot-highlight))
      (,godot-256-class (:foreground ,godot-256-background
                                     :background ,godot-256-highlight))))

   `(ahs-face
     ((,godot-class (:foreground ,godot-background
                                 :background ,godot-yellow))
      (,godot-256-class (:foreground ,godot-256-magenta
                                     :background unspecified))))

   `(ahs-plugin-bod-face
     ((,godot-class (:foreground ,godot-background
                                 :background ,godot-violet ))
      (,godot-256-class (:foreground ,godot-256-background
                                     :background ,godot-256-cyan ))))

   `(ahs-plugin-defalt-face
     ((,godot-class (:foreground ,godot-background
                                 :background ,godot-orange))
      (,godot-256-class (:foreground ,godot-256-background
                                     :background ,godot-256-orange))))

   `(ahs-plugin-whole-buffer-face
     ((,godot-class (:foreground ,godot-background
                                 :background ,godot-green))
      (,godot-256-class (:foreground ,godot-256-background
                                     :background ,godot-256-green))))

   `(ahs-warning-face
     ((,godot-class (:foreground ,godot-red
                                 :weight bold))
      (,godot-256-class (:foreground ,godot-256-red
                                     :weight bold))))

   ;; android mode
   `(android-mode-debug-face
     ((,godot-class (:foreground ,godot-green))
      (,godot-256-class (:foreground ,godot-256-green))))

   `(android-mode-error-face
     ((,godot-class (:foreground ,godot-orange
                                 :weight bold))
      (,godot-256-class (:foreground ,godot-256-orange
                                     :weight bold))))

   `(android-mode-info-face
     ((,godot-class (:foreground ,godot-foreground))
      (,godot-256-class (:foreground ,godot-256-foreground))))

   `(android-mode-verbose-face
     ((,godot-class (:foreground ,godot-comments))
      (,godot-256-class (:foreground ,godot-256-comments))))

   `(android-mode-warning-face
     ((,godot-class (:foreground ,godot-yellow))
      (,godot-256-class (:foreground ,godot-256-yellow))))

   ;; anzu-mode
   `(anzu-mode-line
     ((,godot-class (:foreground ,godot-violet
                                 :weight bold))
      (,godot-256-class (:foreground ,godot-256-violet
                                     :weight bold))))

   ;; bm
   `(bm-face
     ((,godot-class (:background ,godot-yellow-lc
                                 :foreground ,godot-background))
      (,godot-256-class (:background ,godot-256-yellow-lc
                                     :foreground ,godot-256-background))))

   `(bm-fringe-face
     ((,godot-class (:background ,godot-yellow-lc
                                 :foreground ,godot-background))
      (,godot-256-class (:background ,godot-256-yellow-lc
                                     :foreground ,godot-256-background))))

   `(bm-fringe-persistent-face
     ((,godot-class (:background ,godot-green-lc
                                 :foreground ,godot-background))
      (,godot-256-class (:background ,godot-256-green-lc
                                     :foreground ,godot-256-background))))

   `(bm-persistent-face
     ((,godot-class (:background ,godot-green-lc
                                 :foreground ,godot-background))
      (,godot-256-class (:background ,godot-256-green-lc
                                     :foreground ,godot-256-background))))

   ;; calfw
   `(cfw:face-day-title
     ((,godot-class (:background ,godot-highlight-line))
      (,godot-256-class (:background ,godot-256-highlight-line))))

   `(cfw:face-annotation
     ((,godot-class (:inherit cfw:face-day-title
                              :foreground ,godot-yellow))
      (,godot-256-class (:inherit cfw:face-day-title
                                  :foreground ,godot-256-yellow))))

   `(cfw:face-default-content
     ((,godot-class (:foreground ,godot-green))
      (,godot-256-class (:foreground ,godot-256-green))))

   `(cfw:face-default-day
     ((,godot-class (:inherit cfw:face-day-title
                              :weight bold))
      (,godot-256-class (:inherit cfw:face-day-title
                                  :weight bold))))

   `(cfw:face-disable
     ((,godot-class (:inherit cfw:face-day-title
                              :foreground ,godot-comments))
      (,godot-256-class (:inherit cfw:face-day-title
                                  :foreground ,godot-256-comments))))

   `(cfw:face-grid
     ((,godot-class (:foreground ,godot-comments))
      (,godot-256-class (:foreground ,godot-256-comments))))

   `(cfw:face-header
     ((,godot-class (:foreground ,godot-blue-hc
                                 :background ,godot-blue-lc
                                 :weight bold))
      (,godot-256-class (:foreground ,godot-256-blue-hc
                                     :background ,godot-256-blue-lc
                                     :weight bold))))

   `(cfw:face-holiday
     ((,godot-class (:background nil
                                 :foreground ,godot-red
                                 :weight bold))
      (,godot-256-class (:background nil
                                     :foreground ,godot-256-red
                                     :weight bold))))

   `(cfw:face-periods
     ((,godot-class (:foreground ,godot-magenta))
      (,godot-256-class (:foreground ,godot-256-magenta))))

   `(cfw:face-select
     ((,godot-class (:background ,godot-magenta-lc
                                 :foreground ,godot-magenta-hc))
      (,godot-256-class (:background ,godot-256-magenta-lc
                                     :foreground ,godot-256-magenta-hc))))

   `(cfw:face-saturday
     ((,godot-class (:foreground ,godot-cyan-hc
                                 :background ,godot-cyan-lc))
      (,godot-256-class (:foreground ,godot-256-cyan-hc
                                     :background ,godot-256-cyan-lc))))

   `(cfw:face-sunday
     ((,godot-class (:foreground ,godot-red-hc
                                 :background ,godot-red-lc
                                 :weight bold))
      (,godot-256-class (:foreground ,godot-256-red-hc
                                     :background ,godot-256-red-lc
                                     :weight bold))))

   `(cfw:face-title
     ((,godot-class (:inherit ,godot-pitch
                              :foreground ,godot-yellow
                              :weight bold
                              :height ,godot-height-plus-4))
      (,godot-256-class (:inherit ,godot-pitch
                                  :foreground ,godot-256-yellow
                                  :weight bold
                                  :height ,godot-height-plus-4))))

   `(cfw:face-today
     ((,godot-class (:weight bold
                             :background ,godot-highlight-line
                             :foreground nil))
      (,godot-256-class (:weight bold
                                 :background ,godot-256-highlight-line
                                 :foreground nil))))

   `(cfw:face-today-title
     ((,godot-class (:background ,godot-yellow-lc
                                 :foreground ,godot-yellow-hc
                                 :weight bold))
      (,godot-256-class (:background ,godot-256-yellow-lc
                                     :foreground ,godot-256-yellow-hc
                                     :weight bold))))

   `(cfw:face-toolbar
     ((,godot-class (:background ,godot-highlight-line
                                 :foreground ,godot-foreground))
      (,godot-256-class (:background ,godot-256-highlight-line
                                     :foreground ,godot-256-foreground))))

   `(cfw:face-toolbar-button-off
     ((,godot-class (:background ,godot-yellow-lc
                                 :foreground ,godot-yellow-hc
                                 :weight bold))
      (,godot-256-class (:background ,godot-256-yellow-lc
                                     :foreground ,godot-256-yellow-hc
                                     :weight bold))))

   `(cfw:face-toolbar-button-on
     ((,godot-class (:background ,godot-yellow-hc
                                 :foreground ,godot-yellow-lc
                                 :weight bold))
      (,godot-256-class (:background ,godot-256-yellow-hc
                                     :foreground ,godot-256-yellow-lc
                                     :weight bold))))

   ;; cider
   `(cider-enlightened
     ((,godot-class (:foreground ,godot-yellow
                                 :background nil
                                 :box (:color ,godot-yellow :line-width -1 :style nil)))
      (,godot-256-class (:foreground ,godot-256-yellow
                                     :background nil
                                     :box (:color ,godot-256-yellow :line-width -1 :style nil))) ))

   `(cider-enlightened-local
     ((,godot-class (:foreground ,godot-yellow))
      (,godot-256-class (:foreground ,godot-256-yellow))))

   `(cider-instrumented-face
     ((,godot-class (:foreground ,godot-violet
                                 :background nil
                                 :box (:color ,godot-violet :line-width -1 :style nil)))
      (,godot-256-class (:foreground ,godot-256-violet
                                     :background nil
                                     :box (:color ,godot-256-violet :line-width -1 :style nil)))))

   `(cider-result-overlay-face
     ((,godot-class (:foreground ,godot-blue
                                 :background nil
                                 :box (:color ,godot-blue :line-width -1 :style nil)))
      (,godot-256-class (:foreground ,godot-256-blue
                                     :background nil
                                     :box (:color ,godot-256-blue :line-width -1 :style nil)))))

   `(cider-test-error-face
     ((,godot-class (:foreground ,godot-background
                                 :background ,godot-orange))
      (,godot-256-class (:foreground ,godot-256-background
                                     :background ,godot-256-orange))))

   `(cider-test-failure-face
     ((,godot-class (:foreground ,godot-background
                                 :background ,godot-red))
      (,godot-256-class (:foreground ,godot-256-background
                                     :background ,godot-256-red))))

   `(cider-test-success-face
     ((,godot-class (:foreground ,godot-background
                                 :background ,godot-green))
      (,godot-256-class (:foreground ,godot-256-background
                                     :background ,godot-256-green))))

   `(cider-traced-face
     ((,godot-class :box (:color ,godot-blue :line-width -1 :style nil))
      (,godot-256-class  :box (:color ,godot-256-blue :line-width -1 :style nil))))

   ;; clojure-test
   `(clojure-test-failure-face
     ((,godot-class (:foreground ,godot-red
                                 :weight bold
                                 :underline t))
      (,godot-256-class (:foreground ,godot-256-red
                                     :weight bold
                                     :underline t))))

   `(clojure-test-error-face
     ((,godot-class (:foreground ,godot-orange
                                 :weight bold
                                 :underline t))
      (,godot-256-class (:foreground ,godot-256-red
                                     :weight bold
                                     :underline t))))

   `(clojure-test-success-face
     ((,godot-class (:foreground ,godot-green
                                 :weight bold
                                 :underline t))
      (,godot-256-class (:foreground ,godot-256-green
                                     :weight bold
                                     :underline t))))

   ;; company-mode
   `(company-tooltip
     ((,godot-class (:background ,godot-highlight-line
                                 :foreground ,godot-emphasis))
      (,godot-256-class (:background ,godot-256-highlight-line
                                     :foreground ,godot-256-emphasis))))

   `(company-tooltip-selection
     ((,godot-class (:background ,godot-blue
                                 :foreground ,godot-background))
      (,godot-256-class (:background ,godot-256-blue
                                     :foreground ,godot-256-background))))

   `(company-tooltip-mouse
     ((,godot-class (:background ,godot-blue
                                 :foreground ,godot-background))
      (,godot-256-class (:background ,godot-256-blue
                                     :foreground ,godot-256-background))))

   `(company-tooltip-common
     ((,godot-class (:foreground ,godot-blue
                                 :underline t))
      (,godot-256-class (:foreground ,godot-256-blue
                                     :underline t))))

   `(company-tooltip-common-selection
     ((,godot-class (:foreground ,godot-background
                                 :background ,godot-blue
                                 :underline t))
      (,godot-256-class (:foreground ,godot-256-background
                                     :background ,godot-256-blue
                                     :underline t))))

   `(company-preview
     ((,godot-class (:background ,godot-highlight-line
                                 :foreground ,godot-emphasis))
      (,godot-256-class (:background ,godot-256-highlight-line
                                     :foreground ,godot-256-emphasis))))

   `(company-preview-common
     ((,godot-class (:foreground ,godot-blue
                                 :underline t))
      (,godot-256-class (:foreground ,godot-256-blue
                                     :underline t))))

   `(company-scrollbar-bg
     ((,godot-class (:background ,godot-gray))
      (,godot-256-class (:background ,godot-256-gray))))

   `(company-scrollbar-fg
     ((,godot-class (:background ,godot-comments))
      (,godot-256-class (:background ,godot-256-comments))))

   `(company-tooltip-annotation
     ((,godot-class (:background ,godot-highlight-line
                                 :foreground ,godot-green))
      (,godot-256-class (:background ,godot-256-highlight-line
                                     :foreground ,godot-256-green))))

   `(company-template-field
     ((,godot-class (:background ,godot-highlight-line
                                 :foreground ,godot-blue))
      (,godot-256-class (:background ,godot-256-highlight-line
                                     :foreground ,godot-256-blue))))

   ;; compilation
   `(compilation-column-face
     ((,godot-class (:foreground ,godot-cyan
                                 :underline nil))
      (,godot-256-class (:foreground ,godot-256-cyan
                                     :underline nil))))

   `(compilation-column-number
     ((,godot-class (:inherit font-lock-doc-face
                              :foreground ,godot-cyan
                              :underline nil))
      (,godot-256-class (:inherit font-lock-doc-face
                                  :foreground ,godot-256-cyan
                                  :underline nil))))

   `(compilation-enter-directory-face
     ((,godot-class (:foreground ,godot-green
                                 :underline nil))
      (,godot-256-class (:foreground ,godot-256-green
                                     :underline nil))))

   `(compilation-error
     ((,godot-class (:inherit error
                              :underline nil))
      (,godot-256-class (:inherit error
                                  :underline nil))))

   `(compilation-error-face
     ((,godot-class (:foreground ,godot-red
                                 :underline nil))
      (,godot-256-class (:foreground ,godot-256-red
                                     :underline nil))))

   `(compilation-face
     ((,godot-class (:foreground ,godot-foreground
                                 :underline nil))
      (,godot-256-class (:foreground ,godot-256-foreground
                                     :underline nil))))

   `(compilation-info
     ((,godot-class (:foreground ,godot-comments
                                 :underline nil
                                 :bold nil))
      (,godot-256-class (:foreground ,godot-256-comments
                                     :underline nil
                                     :bold nil))))

   `(compilation-info-face
     ((,godot-class (:foreground ,godot-blue
                                 :underline nil))
      (,godot-256-class (:foreground ,godot-256-blue
                                     :underline nil))))

   `(compilation-leave-directory-face
     ((,godot-class (:foreground ,godot-green
                                 :underline nil))
      (,godot-256-class (:foreground ,godot-256-green
                                     :underline nil))))

   `(compilation-line-face
     ((,godot-class (:foreground ,godot-green
                                 :underline nil))
      (,godot-256-class (:foreground ,godot-256-green
                                     :underline nil))))

   `(compilation-line-number
     ((,godot-class (:foreground ,godot-green
                                 :underline nil))
      (,godot-256-class (:foreground ,godot-256-green
                                     :underline nil))))

   `(compilation-warning
     ((,godot-class (:inherit warning
                              :underline nil))
      (,godot-256-class (:inherit warning
                                  :underline nil))))

   `(compilation-warning-face
     ((,godot-class (:foreground ,godot-yellow
                                 :weight normal
                                 :underline nil))
      (,godot-256-class (:foreground ,godot-256-yellow
                                     :weight normal
                                     :underline nil))))

   `(compilation-mode-line-exit
     ((,godot-class (:inherit compilation-info
                              :foreground ,godot-green
                              :weight bold))
      (,godot-256-class (:inherit compilation-info
                                  :foreground ,godot-256-green
                                  :weight bold))))

   `(compilation-mode-line-fail
     ((,godot-class (:inherit compilation-error
                              :foreground ,godot-red
                              :weight bold))
      (,godot-256-class (:inherit compilation-error
                                  :foreground ,godot-256-red
                                  :weight bold))))

   `(compilation-mode-line-run
     ((,godot-class (:foreground ,godot-orange
                                 :weight bold))
      (,godot-256-class (:foreground ,godot-256-orange
                                     :weight bold))))

   ;; CSCOPE
   `(cscope-file-face
     ((,godot-class (:foreground ,godot-green
                                 :weight bold))
      (,godot-256-class (:foreground ,godot-256-green
                                     :weight bold))))

   `(cscope-function-face
     ((,godot-class (:foreground ,godot-blue))
      (,godot-256-class (:foreground ,godot-256-blue))))

   `(cscope-line-number-face
     ((,godot-class (:foreground ,godot-yellow))
      (,godot-256-class (:foreground ,godot-256-yellow))))

   `(cscope-line-face
     ((,godot-class (:foreground ,godot-foreground))
      (,godot-256-class (:foreground ,godot-256-foreground))))

   `(cscope-mouse-face
     ((,godot-class (:background ,godot-blue
                                 :foreground ,godot-foreground))
      (,godot-256-class (:background ,godot-256-blue
                                     :foreground ,godot-256-foreground))))

   ;; ctable
   `(ctbl:face-cell-select
     ((,godot-class (:background ,godot-highlight-line
                                 :foreground ,godot-emphasis
                                 :underline ,godot-emphasis
                                 :weight bold))
      (,godot-256-class (:background ,godot-256-highlight-line
                                     :foreground ,godot-256-emphasis
                                     :underline ,godot-256-emphasis
                                     :weight bold))))

   `(ctbl:face-continue-bar
     ((,godot-class (:background ,godot-gray
                                 :foreground ,godot-yellow))
      (,godot-256-class (:background ,godot-256-gray
                                     :foreground ,godot-256-yellow))))

   `(ctbl:face-row-select
     ((,godot-class (:background ,godot-highlight-line
                                 :foreground ,godot-foreground
                                 :underline t))
      (,godot-256-class (:background ,godot-256-highlight-line
                                     :foreground ,godot-256-foreground
                                     :underline t))))

   ;; coffee
   `(coffee-mode-class-name
     ((,godot-class (:foreground ,godot-yellow
                                 :weight bold))
      (,godot-256-class (:foreground ,godot-256-yellow
                                     :weight bold))))

   `(coffee-mode-function-param
     ((,godot-class (:foreground ,godot-violet
                                 :slant italic))
      (,godot-256-class (:foreground ,godot-256-violet
                                     :slant italic))))

   ;; custom
   `(custom-face-tag
     ((,godot-class (:inherit ,godot-pitch
                              :height ,godot-height-plus-3
                              :foreground ,godot-violet
                              :weight bold))
      (,godot-256-class (:inherit ,godot-pitch
                                  :height ,godot-height-plus-3
                                  :foreground ,godot-256-violet
                                  :weight bold))))

   `(custom-variable-tag
     ((,godot-class (:inherit ,godot-pitch
                              :foreground ,godot-cyan
                              :height ,godot-height-plus-3))
      (,godot-256-class (:inherit ,godot-pitch
                                  :foreground ,godot-256-cyan
                                  :height ,godot-height-plus-3))))

   `(custom-comment-tag
     ((,godot-class (:foreground ,godot-comments))
      (,godot-256-class (:foreground ,godot-256-comments))))

   `(custom-group-tag
     ((,godot-class (:inherit ,godot-pitch
                              :foreground ,godot-blue
                              :height ,godot-height-plus-3))
      (,godot-256-class (:inherit ,godot-pitch
                                  :foreground ,godot-256-blue
                                  :height ,godot-height-plus-3))))

   `(custom-group-tag-1
     ((,godot-class (:inherit ,godot-pitch
                              :foreground ,godot-red
                              :height ,godot-height-plus-3))
      (,godot-256-class (:inherit ,godot-pitch
                                  :foreground ,godot-256-red
                                  :height ,godot-height-plus-3))))

   `(custom-state
     ((,godot-class (:foreground ,godot-green))
      (,godot-256-class (:foreground ,godot-256-green))))

   ;; diff
   `(diff-added
     ((,godot-class (:foreground ,godot-green
                                 :background ,godot-background))
      (,godot-256-class (:foreground ,godot-256-green
                                     :background ,godot-256-background))))

   `(diff-changed
     ((,godot-class (:foreground ,godot-blue
                                 :background ,godot-background))
      (,godot-256-class (:foreground ,godot-256-blue
                                     :background ,godot-256-background))))

   `(diff-removed
     ((,godot-class (:foreground ,godot-red
                                 :background ,godot-background))
      (,godot-256-class (:foreground ,godot-256-red
                                     :background ,godot-256-background))))

   `(diff-header
     ((,godot-class (:background ,godot-background))
      (,godot-256-class (:background ,godot-256-background))))

   `(diff-file-header
     ((,godot-class (:background ,godot-background
                                 :foreground ,godot-foreground
                                 :weight bold))
      (,godot-256-class (:background ,godot-256-background
                                     :foreground ,godot-256-foreground
                                     :weight bold))))

   `(diff-refine-added
     ((,godot-class (:foreground ,godot-background
                                 :background ,godot-green))
      (,godot-256-class (:foreground ,godot-256-background
                                     :background ,godot-256-green))))

   `(diff-refine-change
     ((,godot-class (:foreground ,godot-background
                                 :background ,godot-blue))
      (,godot-256-class (:foreground ,godot-256-background
                                     :background ,godot-256-blue))))

   `(diff-refine-removed
     ((,godot-class (:foreground ,godot-background
                                 :background ,godot-red))
      (,godot-256-class (:foreground ,godot-256-background
                                     :background ,godot-256-red))))

   ;; diff-hl
   `(diff-hl-change
     ((,godot-class (:background ,godot-yellow-hc-alt
                                 :foreground ,godot-yellow-hc-alt))
      (,godot-256-class (:background ,godot-256-yellow-hc
                                     :foreground ,godot-256-yellow-hc))))

   `(diff-hl-delete
     ((,godot-class (:background ,godot-red-hc-alt
                                 :foreground ,godot-red-hc-alt))
      (,godot-256-class (:background ,godot-256-red-hc
                                     :foreground ,godot-256-red-hc))))

   `(diff-hl-insert
     ((,godot-class (:background ,godot-green-hc-alt
                                 :foreground ,godot-green-hc-alt))
      (,godot-256-class (:background ,godot-256-green-hc
                                     :foreground ,godot-256-green-hc))))

   `(diff-hl-unknown
     ((,godot-class (:background ,godot-violet-hc
                                 :foreground ,godot-violet-hc))
      (,godot-256-class (:background ,godot-256-violet-hc
                                     :foreground ,godot-256-violet-hc))))

   ;; ediff
   `(ediff-fine-diff-A
     ((,godot-class (:background ,godot-diff-red-emphasis))
      (,godot-256-class (:background ,godot-256-diff-red-emphasis))))

   `(ediff-fine-diff-B
     ((,godot-class (:background ,godot-diff-green-emphasis))
      (,godot-256-class (:background ,godot-256-diff-green-emphasis))))

   `(ediff-fine-diff-C
     ((,godot-class (:background ,godot-diff-blue-emphasis))
      (,godot-256-class (:background ,godot-256-diff-blue-emphasis))))

   `(ediff-current-diff-A
     ((,godot-class (:background ,godot-diff-red-base))
      (,godot-256-class (:background ,godot-256-diff-red-base))))

   `(ediff-current-diff-B
     ((,godot-class (:background ,godot-diff-green-base))
      (,godot-256-class (:background ,godot-256-diff-green-base))))

   `(ediff-current-diff-C
     ((,godot-class (:background ,godot-diff-blue-base))
      (,godot-256-class (:background ,godot-256-diff-blue-base))))

   `(ediff-even-diff-A
     ((,godot-class (:background ,godot-comments
                                 :foreground ,godot-foreground-lc ))
      (,godot-256-class (:background ,godot-256-comments
                                     :foreground ,godot-256-foreground-lc ))))

   `(ediff-odd-diff-A
     ((,godot-class (:background ,godot-comments
                                 :foreground ,godot-foreground-hc ))
      (,godot-256-class (:background ,godot-256-comments
                                     :foreground ,godot-256-foreground-hc ))))

   `(ediff-even-diff-B
     ((,godot-class (:background ,godot-comments
                                 :foreground ,godot-foreground-hc ))
      (,godot-256-class (:background ,godot-256-comments
                                     :foreground ,godot-256-foreground-hc ))))

   `(ediff-odd-diff-B
     ((,godot-class (:background ,godot-comments
                                 :foreground ,godot-foreground-lc ))
      (,godot-256-class (:background ,godot-256-comments
                                     :foreground ,godot-256-foreground-lc ))))

   `(ediff-even-diff-C
     ((,godot-class (:background ,godot-comments
                                 :foreground ,godot-foreground ))
      (,godot-256-class (:background ,godot-256-comments
                                     :foreground ,godot-256-foreground ))))

   `(ediff-odd-diff-C
     ((,godot-class (:background ,godot-comments
                                 :foreground ,godot-background ))
      (,godot-256-class (:background ,godot-256-comments
                                     :foreground ,godot-256-background ))))

   ;; edts
   `(edts-face-error-line
     ((,(append '((supports :underline (:style line))) godot-class)
       (:underline (:style line :color ,godot-red)
                   :inherit unspecified))
      (,godot-class (:foreground ,godot-red-hc
                                 :background ,godot-red-lc
                                 :weight bold
                                 :underline t))
      (,(append '((supports :underline (:style line))) godot-256-class )
       (:underline (:style line :color ,godot-256-red)
                   :inherit unspecified))
      (,godot-256-class (:foreground ,godot-256-red-hc
                                     :background ,godot-256-red-lc
                                     :weight bold
                                     :underline t))))

   `(edts-face-warning-line
     ((,(append '((supports :underline (:style line))) godot-class)
       (:underline (:style line :color ,godot-yellow)
                   :inherit unspecified))
      (,godot-class (:foreground ,godot-yellow-hc
                                 :background ,godot-yellow-lc
                                 :weight bold
                                 :underline t))
      (,(append '((supports :underline (:style line))) godot-256-class )
       (:underline (:style line :color ,godot-256-yellow)
                   :inherit unspecified))
      (,godot-256-class (:foreground ,godot-256-yellow-hc
                                     :background ,godot-256-yellow-lc
                                     :weight bold
                                     :underline t))))

   `(edts-face-error-fringe-bitmap
     ((,godot-class (:foreground ,godot-red
                                 :background unspecified
                                 :weight bold))
      (,godot-256-class (:foreground ,godot-256-red
                                     :background unspecified
                                     :weight bold))))

   `(edts-face-warning-fringe-bitmap
     ((,godot-class (:foreground ,godot-yellow
                                 :background unspecified
                                 :weight bold))
      (,godot-256-class (:foreground ,godot-256-yellow
                                     :background unspecified
                                     :weight bold))))

   `(edts-face-error-mode-line
     ((,godot-class (:background ,godot-red
                                 :foreground unspecified))
      (,godot-256-class (:background ,godot-256-red
                                     :foreground unspecified))))

   `(edts-face-warning-mode-line
     ((,godot-class (:background ,godot-yellow
                                 :foreground unspecified))
      (,godot-256-class (:background ,godot-256-yellow
                                     :foreground unspecified))))


   ;; elfeed
   `(elfeed-search-date-face
     ((,godot-class (:foreground ,godot-comments))
      (,godot-256-class (:foreground ,godot-256-comments))))

   `(elfeed-search-feed-face
     ((,godot-class (:foreground ,godot-comments))
      (,godot-256-class (:foreground ,godot-256-comments))))

   `(elfeed-search-tag-face
     ((,godot-class (:foreground ,godot-foreground))
      (,godot-256-class (:foreground ,godot-256-foreground))))

   `(elfeed-search-title-face
     ((,godot-class (:foreground ,godot-cyan))
      (,godot-256-class (:foreground ,godot-256-cyan))))

   ;; elixir
   `(elixir-attribute-face
     ((,godot-class (:foreground ,godot-orange))
      (,godot-256-class (:foreground ,godot-256-orange))))

   `(elixir-atom-face
     ((,godot-class (:foreground ,godot-violet))
      (,godot-256-class (:foreground ,godot-256-violet))))

   ;; ein
   `(ein:cell-input-area
     ((,godot-class (:background ,godot-highlight-line))
      (,godot-256-class (:background ,godot-256-highlight-line))))
   `(ein:cell-input-prompt
     ((,godot-class (:foreground ,godot-green))
      (,godot-256-class (:foreground ,godot-256-green))))
   `(ein:cell-output-prompt
     ((,godot-class (:foreground ,godot-red))
      (,godot-256-class (:foreground ,godot-256-red))))
   `(ein:notification-tab-normal
     ((,godot-class (:foreground ,godot-blue))
      (,godot-256-class (:foreground ,godot-256-blue))))
   `(ein:notification-tab-selected
     ((,godot-class (:foreground ,godot-orange :inherit bold))
      (,godot-256-class (:foreground ,godot-256-orange :inherit bold))))

   ;; enhanced ruby mode
   `(enh-ruby-string-delimiter-face
     ((,godot-class (:inherit font-lock-string-face))
      (,godot-256-class (:inherit font-lock-string-face))))

   `(enh-ruby-heredoc-delimiter-face
     ((,godot-class (:inherit font-lock-string-face))
      (,godot-256-class (:inherit font-lock-string-face))))

   `(enh-ruby-regexp-delimiter-face
     ((,godot-class (:inherit font-lock-string-face))
      (,godot-256-class (:inherit font-lock-string-face))))

   `(enh-ruby-op-face
     ((,godot-class (:inherit font-lock-keyword-face))
      (,godot-256-class (:inherit font-lock-keyword-face))))

   ;; erm-syn
   `(erm-syn-errline
     ((,(append '((supports :underline (:style wave))) godot-class)
       (:underline (:style wave :color ,godot-red)
                   :inherit unspecified))
      (,godot-class (:foreground ,godot-red-hc
                                 :background ,godot-red-lc
                                 :weight bold
                                 :underline t))
      (,(append '((supports :underline (:style wave))) godot-256-class )
       (:underline (:style wave :color ,godot-256-red)
                   :inherit unspecified))
      (,godot-256-class (:foreground ,godot-256-red-hc
                                     :background ,godot-256-red-lc
                                     :weight bold
                                     :underline t))))

   `(erm-syn-warnline
     ((,(append '((supports :underline (:style wave))) godot-class)
       (:underline (:style wave :color ,godot-orange)
                   :inherit unspecified))
      (,godot-class (:foreground ,godot-orange-hc
                                 :background ,godot-orange-lc
                                 :weight bold
                                 :underline t))
      (,(append '((supports :underline (:style wave))) godot-256-class )
       (:underline (:style wave :color ,godot-256-orange)
                   :inherit unspecified))
      (,godot-256-class (:foreground ,godot-256-orange-hc
                                     :background ,godot-256-orange-lc
                                     :weight bold
                                     :underline t))))

   ;; epc
   `(epc:face-title
     ((,godot-class (:foreground ,godot-blue
                                 :background ,godot-background
                                 :weight normal
                                 :underline nil))
      (,godot-256-class (:foreground ,godot-256-blue
                                     :background ,godot-256-background
                                     :weight normal
                                     :underline nil))))

   ;; erc
   `(erc-action-face
     ((,godot-class (:inherit erc-default-face))
      (,godot-256-class (:inherit erc-default-face))))

   `(erc-bold-face
     ((,godot-class (:weight bold))
      (,godot-256-class (:weight bold))))

   `(erc-current-nick-face
     ((,godot-class (:foreground ,godot-blue :weight bold))
      (,godot-256-class (:foreground ,godot-256-blue
                                     :weight bold))))

   `(erc-dangerous-host-face
     ((,godot-class (:inherit font-lock-warning-face))
      (,godot-256-class (:inherit font-lock-warning-face))))

   `(erc-default-face
     ((,godot-class (:foreground ,godot-foreground))
      (,godot-256-class (:foreground ,godot-256-foreground))))

   `(erc-highlight-face
     ((,godot-class (:inherit erc-default-face
                              :background ,godot-highlight))
      (,godot-256-class (:inherit erc-default-face
                                  :background ,godot-256-highlight))))

   `(erc-direct-msg-face
     ((,godot-class (:inherit erc-default-face))
      (,godot-256-class (:inherit erc-default-face))))

   `(erc-error-face
     ((,godot-class (:inherit font-lock-warning-face))
      (,godot-256-class (:inherit font-lock-warning-face))))

   `(erc-fool-face
     ((,godot-class (:inherit erc-default-face))
      (,godot-256-class (:inherit erc-default-face))))

   `(erc-input-face
     ((,godot-class (:foreground ,godot-yellow))
      (,godot-256-class (:foreground ,godot-256-yellow))))

   `(erc-keyword-face
     ((,godot-class (:foreground ,godot-blue
                                 :weight bold))
      (,godot-256-class (:foreground ,godot-256-blue
                                     :weight bold))))

   `(erc-nick-default-face
     ((,godot-class (:foreground ,godot-yellow
                                 :weight bold))
      (,godot-256-class (:foreground ,godot-256-yellow
                                     :weight bold))))

   `(erc-my-nick-face
     ((,godot-class (:foreground ,godot-red
                                 :weight bold))
      (,godot-256-class (:foreground ,godot-256-red
                                     :weight bold))))

   `(erc-nick-msg-face
     ((,godot-class (:inherit erc-default-face))
      (,godot-256-class (:inherit erc-default-face))))

   `(erc-notice-face
     ((,godot-class (:foreground ,godot-green))
      (,godot-256-class (:foreground ,godot-256-green))))

   `(erc-pal-face
     ((,godot-class (:foreground ,godot-orange
                                 :weight bold))
      (,godot-256-class (:foreground ,godot-256-orange
                                     :weight bold))))

   `(erc-prompt-face
     ((,godot-class (:foreground ,godot-orange
                                 :background ,godot-background
                                 :weight bold))
      (,godot-256-class (:foreground ,godot-256-orange
                                     :background ,godot-256-background
                                     :weight bold))))

   `(erc-timestamp-face
     ((,godot-class (:foreground ,godot-green))
      (,godot-256-class (:foreground ,godot-256-green))))

   `(erc-underline-face
     ((t (:underline t))))

   ;; eshell
   `(eshell-prompt
     ((,godot-class (:foreground ,godot-blue
                                 :inherit bold))
      (,godot-256-class (:foreground ,godot-256-blue
                                     :inherit bold))))

   `(eshell-ls-archive
     ((,godot-class (:foreground ,godot-red
                                 :weight bold))
      (,godot-256-class (:foreground ,godot-256-red
                                     :inherit bold))))

   `(eshell-ls-backup
     ((,godot-class (:inherit font-lock-comment-face))
      (,godot-256-class (:inherit font-lock-comment-face))))

   `(eshell-ls-clutter
     ((,godot-class (:inherit font-lock-comment-face))
      (,godot-256-class (:inherit font-lock-comment-face))))

   `(eshell-ls-directory
     ((,godot-class (:foreground ,godot-blue
                                 :inherit bold))
      (,godot-256-class (:foreground ,godot-256-blue
                                     :inherit bold))))

   `(eshell-ls-executable
     ((,godot-class (:foreground ,godot-green
                                 :inherit bold))
      (,godot-256-class (:foreground ,godot-256-green
                                     :inherit bold))))

   `(eshell-ls-unreadable
     ((,godot-class (:foreground ,godot-foreground))
      (,godot-256-class (:foreground ,godot-256-foreground))))

   `(eshell-ls-missing
     ((,godot-class (:inherit font-lock-warning-face))
      (,godot-256-class (:inherit font-lock-warning-face))))

   `(eshell-ls-product
     ((,godot-class (:inherit font-lock-doc-face))
      (,godot-256-class (:inherit font-lock-doc-face))))

   `(eshell-ls-special
     ((,godot-class (:foreground ,godot-yellow
                                 :inherit bold))
      (,godot-256-class (:foreground ,godot-256-yellow
                                     :inherit bold))))

   `(eshell-ls-symlink
     ((,godot-class (:foreground ,godot-cyan
                                 :inherit bold))
      (,godot-256-class (:foreground ,godot-256-cyan
                                     :inherit bold))))

   ;; evil-ex-substitute
   `(evil-ex-substitute-matches
     ((,godot-class (:background ,godot-highlight-line
                                 :foreground ,godot-red-l
                                 :inherit italic))
      (,godot-256-class (:background ,godot-256-highlight-line
                                     :foreground ,godot-256-red-l
                                     :inherit italic))))
   `(evil-ex-substitute-replacement
     ((,godot-class (:background ,godot-highlight-line
                                 :foreground ,godot-green-l
                                 :inherit italic))
      (,godot-256-class (:background ,godot-256-highlight-line :foreground ,godot-256-green-l :inherit italic))))

   ;; evil-search-highlight-persist
   `(evil-search-highlight-persist-highlight-face
     ((,godot-class (:inherit region))
      (,godot-256-class (:inherit region))))

   ;; fic
   `(fic-author-face
     ((,godot-class (:background ,godot-background
                                 :foreground ,godot-orange
                                 :underline t
                                 :slant italic))
      (,godot-256-class (:background ,godot-256-background
                                     :foreground ,godot-256-orange
                                     :underline t
                                     :slant italic))))

   `(fic-face
     ((,godot-class (:background ,godot-background
                                 :foreground ,godot-orange
                                 :weight normal
                                 :slant italic))
      (,godot-256-class (:background ,godot-256-background
                                     :foreground ,godot-256-orange
                                     :weight normal
                                     :slant italic))))

   `(font-lock-fic-face
     ((,godot-class (:background ,godot-background
                                 :foreground ,godot-orange
                                 :weight normal
                                 :slant italic))
      (,godot-256-class (:background ,godot-256-background
                                     :foreground ,godot-256-orange
                                     :weight normal
                                     :slant italic))))

   ;; flx
   `(flx-highlight-face
     ((,godot-class (:foreground ,godot-blue
                                 :weight normal
                                 :underline nil))
      (,godot-256-class (:foreground ,godot-256-blue
                                     :weight normal
                                     :underline nil))))

   ;; flymake
   `(flymake-errline
     ((,(append '((supports :underline (:style wave))) godot-class)
       (:underline (:style wave :color ,godot-red)
                   :inherit unspecified
                   :foreground unspecified
                   :background unspecified))
      (,godot-class (:foreground ,godot-red-hc
                                 :background ,godot-red-lc
                                 :weight bold
                                 :underline t))
      (,(append '((supports :underline (:style wave))) godot-256-class )
       (:underline (:style wave :color ,godot-256-red)
                   :inherit unspecified
                   :foreground unspecified
                   :background unspecified))
      (,godot-256-class (:foreground ,godot-256-red-hc
                                     :background ,godot-256-red-lc
                                     :weight bold
                                     :underline t))))

   `(flymake-infoline
     ((,(append '((supports :underline (:style wave))) godot-class)
       (:underline (:style wave :color ,godot-green)
                   :inherit unspecified
                   :foreground unspecified
                   :background unspecified))
      (,godot-class (:foreground ,godot-green-hc
                                 :background ,godot-green-lc))
      (,(append '((supports :underline (:style wave))) godot-256-class )
       (:underline (:style wave :color ,godot-256-green)
                   :inherit unspecified
                   :foreground unspecified
                   :background unspecified))
      (,godot-256-class (:foreground ,godot-256-green-hc
                                     :background ,godot-256-green-lc))))

   `(flymake-warnline
     ((,(append '((supports :underline (:style wave))) godot-class)
       (:underline (:style wave :color ,godot-yellow)
                   :inherit unspecified
                   :foreground unspecified
                   :background unspecified))
      (,godot-class (:foreground ,godot-yellow-hc
                                 :background ,godot-yellow-lc
                                 :weight bold
                                 :underline t))
      (,(append '((supports :underline (:style wave))) godot-256-class )
       (:underline (:style wave :color ,godot-256-yellow)
                   :inherit unspecified
                   :foreground unspecified
                   :background unspecified))
      (,godot-256-class (:foreground ,godot-256-yellow-hc
                                     :background ,godot-256-yellow-lc
                                     :weight bold
                                     :underline t))))

   ;; flycheck
   `(flycheck-error
     ((,(append '((supports :underline (:style line))) godot-class)
       (:underline (:style line :color ,godot-red)))
      (,godot-class (:foreground ,godot-red
                                 :background ,godot-background
                                 :weight bold
                                 :underline t))
      (,(append '((supports :underline (:style line))) godot-256-class )
       (:underline (:style line :color ,godot-256-red)))
      (,godot-256-class (:foreground ,godot-256-red
                                     :background ,godot-256-background
                                     :weight bold
                                     :underline t))))

   `(flycheck-warning
     ((,(append '((supports :underline (:style line))) godot-class)
       (:underline (:style line :color ,godot-orange)))
      (,godot-class (:foreground ,godot-orange
                                 :background ,godot-background
                                 :weight bold
                                 :underline t))
      (,(append '((supports :underline (:style line))) godot-256-class )
       (:underline (:style line :color ,godot-256-orange)))
      (,godot-256-class (:foreground ,godot-256-orange
                                     :background ,godot-256-background
                                     :weight bold
                                     :underline t))))

   `(flycheck-info
     ((,(append '((supports :underline (:style line))) godot-class)
       (:underline (:style line :color ,godot-blue)))
      (,godot-class (:foreground ,godot-blue
                                 :background ,godot-background
                                 :weight bold
                                 :underline t))
      (,(append '((supports :underline (:style line))) godot-256-class )
       (:underline (:style line :color ,godot-256-blue)))
      (,godot-256-class (:foreground ,godot-256-blue
                                     :background ,godot-256-background
                                     :weight bold
                                     :underline t))))

   `(flycheck-fringe-error
     ((,godot-class (:foreground ,godot-red-l
                                 :background unspecified
                                 :weight bold))
      (,godot-256-class (:foreground ,godot-256-red-l
                                     :background unspecified
                                     :weight bold))))

   `(flycheck-fringe-warning
     ((,godot-class (:foreground ,godot-orange-l
                                 :background unspecified
                                 :weight bold))
      (,godot-256-class (:foreground ,godot-256-orange-l
                                     :background unspecified
                                     :weight bold))))

   `(flycheck-fringe-info
     ((,godot-class (:foreground ,godot-blue-l
                                 :background unspecified
                                 :weight bold))
      (,godot-256-class (:foreground ,godot-256-blue-l
                                     :background unspecified
                                     :weight bold))))

   ;; flyspell
   `(flyspell-duplicate
     ((,(append '((supports :underline (:style wave))) godot-class)
       (:underline (:style wave :color ,godot-yellow)
                   :inherit unspecified))
      (,godot-class (:foreground ,godot-yellow
                                 :weight bold
                                 :underline t))
      (,(append '((supports :underline (:style wave))) godot-256-class )
       (:underline (:style wave :color ,godot-256-yellow)
                   :inherit unspecified))
      (,godot-256-class (:foreground ,godot-256-yellow
                                     :weight bold
                                     :underline t))))

   `(flyspell-incorrect
     ((,(append '((supports :underline (:style wave))) godot-class)
       (:underline (:style wave :color ,godot-red)
                   :inherit unspecified))
      (,godot-class (:foreground ,godot-red
                                 :weight bold
                                 :underline t))
      (,(append '((supports :underline (:style wave))) godot-256-class )
       (:underline (:style wave :color ,godot-256-red)
                   :inherit unspecified))
      (,godot-256-class (:foreground ,godot-256-red
                                     :weight bold
                                     :underline t))))


   ;; git-gutter
   `(git-gutter:added
     ((,godot-class (:background ,godot-green
                                 :foreground ,godot-background
                                 :inherit bold))
      (,godot-256-class (:background ,godot-256-green
                                     :foreground ,godot-256-background
                                     :inherit bold))))

   `(git-gutter:deleted
     ((,godot-class (:background ,godot-red
                                 :foreground ,godot-background
                                 :inherit bold))
      (,godot-256-class (:background ,godot-256-red
                                     :foreground ,godot-256-background
                                     :inherit bold))))

   `(git-gutter:modified
     ((,godot-class (:background ,godot-blue
                                 :foreground ,godot-background
                                 :inherit bold))
      (,godot-256-class (:background ,godot-256-blue
                                     :foreground ,godot-256-background
                                     :inherit bold))))

   `(git-gutter:unchanged
     ((,godot-class (:background ,godot-highlight-line
                                 :foreground ,godot-background
                                 :inherit bold))
      (,godot-256-class (:background ,godot-256-highlight-line
                                     :foreground ,godot-256-background
                                     :inherit bold))))

   ;; git-gutter-fr
   `(git-gutter-fr:added
     ((,godot-class (:foreground ,godot-green
                                 :inherit bold))
      (,godot-256-class (:foreground ,godot-256-green
                                     :inherit bold))))

   `(git-gutter-fr:deleted
     ((,godot-class (:foreground ,godot-red
                                 :inherit bold))
      (,godot-256-class (:foreground ,godot-256-red
                                     :inherit bold))))

   `(git-gutter-fr:modified
     ((,godot-class (:foreground ,godot-blue
                                 :inherit bold))
      (,godot-256-class (:foreground ,godot-256-blue
                                     :inherit bold))))

   ;; git-gutter+ and git-gutter+-fr
   `(git-gutter+-added
     ((,godot-class (:background ,godot-green
                                 :foreground ,godot-background
                                 :inherit bold))
      (,godot-256-class (:background ,godot-256-green
                                     :foreground ,godot-256-background
                                     :inherit bold))))

   `(git-gutter+-deleted
     ((,godot-class (:background ,godot-red
                                 :foreground ,godot-background
                                 :inherit bold))
      (,godot-256-class (:background ,godot-256-red
                                     :foreground ,godot-256-background
                                     :inherit bold))))

   `(git-gutter+-modified
     ((,godot-class (:background ,godot-blue
                                 :foreground ,godot-background
                                 :inherit bold))
      (,godot-256-class (:background ,godot-256-blue
                                     :foreground ,godot-256-background
                                     :inherit bold))))

   `(git-gutter+-unchanged
     ((,godot-class (:background ,godot-highlight-line
                                 :foreground ,godot-background
                                 :inherit bold))
      (,godot-256-class (:background ,godot-256-highlight-line
                                     :foreground ,godot-256-background
                                     :inherit bold))))

   `(git-gutter-fr+-added
     ((,godot-class (:foreground ,godot-green
                                 :weight bold))
      (,godot-256-class (:foreground ,godot-256-green
                                     :weight bold))))

   `(git-gutter-fr+-deleted
     ((,godot-class (:foreground ,godot-red
                                 :weight bold))
      (,godot-256-class (:foreground ,godot-256-red
                                     :weight bold))))

   `(git-gutter-fr+-modified
     ((,godot-class (:foreground ,godot-blue
                                 :weight bold))
      (,godot-256-class (:foreground ,godot-256-blue
                                     :weight bold))))

   ;; git-timemachine
   `(git-timemachine-minibuffer-detail-face
     ((,godot-class (:foreground ,godot-blue
                                 :background ,godot-highlight-line
                                 :inherit bold))
      (,godot-256-class (:foreground ,godot-blue
                                     :background ,godot-256-highlight-line
                                     :inherit bold))))

   ;; guide-key
   `(guide-key/highlight-command-face
     ((,godot-class (:foreground ,godot-blue))
      (,godot-256-class (:foreground ,godot-256-blue))))

   `(guide-key/key-face
     ((,godot-class (:foreground ,godot-orange))
      (,godot-256-class (:foreground ,godot-256-orange))))

   `(guide-key/prefix-command-face
     ((,godot-class (:foreground ,godot-violet))
      (,godot-256-class (:foreground ,godot-256-violet))))

   ;; gnus
   `(gnus-group-mail-1
     ((,godot-class (:weight bold
                             :inherit gnus-group-mail-1-empty))
      (,godot-256-class (:weight bold
                                 :inherit gnus-group-mail-1-empty))))

   `(gnus-group-mail-1-empty
     ((,godot-class (:inherit gnus-group-news-1-empty))
      (,godot-256-class (:inherit gnus-group-news-1-empty))))

   `(gnus-group-mail-2
     ((,godot-class (:weight bold
                             :inherit gnus-group-mail-2-empty))
      (,godot-256-class (:weight bold
                                 :inherit gnus-group-mail-2-empty))))

   `(gnus-group-mail-2-empty
     ((,godot-class (:inherit gnus-group-news-2-empty))
      (,godot-256-class (:inherit gnus-group-news-2-empty))))

   `(gnus-group-mail-3
     ((,godot-class (:weight bold
                             :inherit gnus-group-mail-3-empty))
      (,godot-256-class (:weight bold
                                 :inherit gnus-group-mail-3-empty))))

   `(gnus-group-mail-3-empty
     ((,godot-class (:inherit gnus-group-news-3-empty))
      (,godot-256-class (:inherit gnus-group-news-3-empty))))

   `(gnus-group-mail-low
     ((,godot-class (:weight bold
                             :inherit gnus-group-mail-low-empty))
      (,godot-256-class (:weight bold
                                 :inherit gnus-group-mail-low-empty))))

   `(gnus-group-mail-low-empty
     ((,godot-class (:inherit gnus-group-news-low-empty))
      (,godot-256-class (:inherit gnus-group-news-low-empty))))

   `(gnus-group-news-1
     ((,godot-class (:weight bold
                             :inherit gnus-group-news-1-empty))
      (,godot-256-class (:weight bold
                                 :inherit gnus-group-news-1-empty))))

   `(gnus-group-news-2
     ((,godot-class (:weight bold
                             :inherit gnus-group-news-2-empty))
      (,godot-256-class (:weight bold
                                 :inherit gnus-group-news-2-empty))))

   `(gnus-group-news-3
     ((,godot-class (:weight bold
                             :inherit gnus-group-news-3-empty))
      (,godot-256-class (:weight bold
                                 :inherit gnus-group-news-3-empty))))

   `(gnus-group-news-4
     ((,godot-class (:weight bold
                             :inherit gnus-group-news-4-empty))
      (,godot-256-class (:weight bold
                                 :inherit gnus-group-news-4-empty))))

   `(gnus-group-news-5
     ((,godot-class (:weight bold
                             :inherit gnus-group-news-5-empty))
      (,godot-256-class (:weight bold
                                 :inherit gnus-group-news-5-empty))))

   `(gnus-group-news-6
     ((,godot-class (:weight bold
                             :inherit gnus-group-news-6-empty))
      (,godot-256-class (:weight bold
                                 :inherit gnus-group-news-6-empty))))

   `(gnus-group-news-low
     ((,godot-class (:weight bold
                             :inherit gnus-group-news-low-empty))
      (,godot-256-class (:weight bold
                                 :inherit gnus-group-news-low-empty))))

   `(gnus-header-content
     ((,godot-class (:inherit message-header-other))
      (,godot-256-class (:inherit message-header-other))))

   `(gnus-header-from
     ((,godot-class (:inherit message-header-other))
      (,godot-256-class (:inherit message-header-other))))

   `(gnus-header-name
     ((,godot-class (:inherit message-header-name))
      (,godot-256-class (:inherit message-header-name))))

   `(gnus-header-newsgroups
     ((,godot-class (:inherit message-header-other))
      (,godot-256-class (:inherit message-header-other))))

   `(gnus-header-subject
     ((,godot-class (:inherit message-header-subject))
      (,godot-256-class (:inherit message-header-subject))))

   `(gnus-summary-cancelled
     ((,godot-class (:foreground ,godot-orange))
      (,godot-256-class (:foreground ,godot-256-orange))))

   `(gnus-summary-high-ancient
     ((,godot-class (:foreground ,godot-blue
                                 :weight bold))
      (,godot-256-class (:foreground ,godot-256-blue
                                     :weight bold))))

   `(gnus-summary-high-read
     ((,godot-class (:foreground ,godot-green
                                 :weight bold))
      (,godot-256-class (:foreground ,godot-256-green
                                     :weight bold))))

   `(gnus-summary-high-ticked
     ((,godot-class (:foreground ,godot-orange
                                 :weight bold))
      (,godot-256-class (:foreground ,godot-256-orange
                                     :weight bold))))

   `(gnus-summary-high-unread
     ((,godot-class (:foreground ,godot-foreground
                                 :weight bold))
      (,godot-256-class (:foreground ,godot-256-foreground
                                     :weight bold))))

   `(gnus-summary-low-ancient
     ((,godot-class (:foreground ,godot-blue))
      (,godot-256-class (:foreground ,godot-256-blue))))

   `(gnus-summary-low-read
     ((,godot-class (:foreground ,godot-green))
      (,godot-256-class (:foreground ,godot-256-green))))

   `(gnus-summary-low-ticked
     ((,godot-class (:foreground ,godot-orange))
      (,godot-256-class (:foreground ,godot-256-orange))))

   `(gnus-summary-low-unread
     ((,godot-class (:foreground ,godot-foreground))
      (,godot-256-class (:foreground ,godot-256-foreground))))

   `(gnus-summary-normal-ancient
     ((,godot-class (:foreground ,godot-blue))
      (,godot-256-class (:foreground ,godot-256-blue))))

   `(gnus-summary-normal-read
     ((,godot-class (:foreground ,godot-green))
      (,godot-256-class (:foreground ,godot-256-green))))

   `(gnus-summary-normal-ticked
     ((,godot-class (:foreground ,godot-orange))
      (,godot-256-class (:foreground ,godot-256-orange))))

   `(gnus-summary-normal-unread
     ((,godot-class (:foreground ,godot-foreground))
      (,godot-256-class (:foreground ,godot-256-foreground))))

   `(gnus-summary-selected
     ((,godot-class (:foreground ,godot-yellow
                                 :weight bold))
      (,godot-256-class (:foreground ,godot-256-yellow
                                     :weight bold))))

   `(gnus-cite-1
     ((,godot-class (:foreground ,godot-blue))
      (,godot-256-class (:foreground ,godot-256-blue))))

   `(gnus-cite-2
     ((,godot-class (:foreground ,godot-blue))
      (,godot-256-class (:foreground ,godot-256-blue))))

   `(gnus-cite-3
     ((,godot-class (:foreground ,godot-blue))
      (,godot-256-class (:foreground ,godot-256-blue))))

   `(gnus-cite-4
     ((,godot-class (:foreground ,godot-green))
      (,godot-256-class (:foreground ,godot-256-green))))

   `(gnus-cite-5
     ((,godot-class (:foreground ,godot-green))
      (,godot-256-class (:foreground ,godot-256-green))))

   `(gnus-cite-6
     ((,godot-class (:foreground ,godot-green))
      (,godot-256-class (:foreground ,godot-256-green))))

   `(gnus-cite-7
     ((,godot-class (:foreground ,godot-red))
      (,godot-256-class (:foreground ,godot-256-red))))

   `(gnus-cite-8
     ((,godot-class (:foreground ,godot-red))
      (,godot-256-class (:foreground ,godot-256-red))))

   `(gnus-cite-9
     ((,godot-class (:foreground ,godot-red))
      (,godot-256-class (:foreground ,godot-256-red))))

   `(gnus-cite-10
     ((,godot-class (:foreground ,godot-yellow))
      (,godot-256-class (:foreground ,godot-256-yellow))))

   `(gnus-cite-11
     ((,godot-class (:foreground ,godot-yellow))
      (,godot-256-class (:foreground ,godot-256-yellow))))

   `(gnus-group-news-1-empty
     ((,godot-class (:foreground ,godot-yellow))
      (,godot-256-class (:foreground ,godot-256-yellow))))

   `(gnus-group-news-2-empty
     ((,godot-class (:foreground ,godot-green))
      (,godot-256-class (:foreground ,godot-256-green))))

   `(gnus-group-news-3-empty
     ((,godot-class (:foreground ,godot-green))
      (,godot-256-class (:foreground ,godot-256-green))))

   `(gnus-group-news-4-empty
     ((,godot-class (:foreground ,godot-blue))
      (,godot-256-class (:foreground ,godot-256-blue))))

   `(gnus-group-news-5-empty
     ((,godot-class (:foreground ,godot-blue))
      (,godot-256-class (:foreground ,godot-256-blue))))

   `(gnus-group-news-6-empty
     ((,godot-class (:foreground ,godot-blue-lc))
      (,godot-256-class (:foreground ,godot-256-blue-lc))))

   `(gnus-group-news-low-empty
     ((,godot-class (:foreground ,godot-comments))
      (,godot-256-class (:foreground ,godot-256-comments))))

   `(gnus-signature
     ((,godot-class (:foreground ,godot-yellow))
      (,godot-256-class (:foreground ,godot-256-yellow))))

   `(gnus-x-face
     ((,godot-class (:background ,godot-foreground
                                 :foreground ,godot-background))
      (,godot-256-class (:background ,godot-256-foreground
                                     :foreground ,godot-256-background))))


   ;; helm
   `(helm-apt-deinstalled
     ((,godot-class (:foreground ,godot-comments))
      (,godot-256-class (:foreground ,godot-256-comments))))

   `(helm-apt-installed
     ((,godot-class (:foreground ,godot-green))
      (,godot-256-class (:foreground ,godot-256-green))))

   `(helm-bookmark-directory
     ((,godot-class (:inherit helm-ff-directory))
      (,godot-256-class (:inherit helm-ff-directory))))

   `(helm-bookmark-file
     ((,godot-class (:foreground ,godot-foreground))
      (,godot-256-class (:foreground ,godot-256-foreground))))

   `(helm-bookmark-gnus
     ((,godot-class (:foreground ,godot-cyan))
      (,godot-256-class (:foreground ,godot-256-cyan))))

   `(helm-bookmark-info
     ((,godot-class (:foreground ,godot-green))
      (,godot-256-class (:foreground ,godot-256-green))))

   `(helm-bookmark-man
     ((,godot-class (:foreground ,godot-violet))
      (,godot-256-class (:foreground ,godot-256-violet))))

   `(helm-bookmark-w3m
     ((,godot-class (:foreground ,godot-yellow))
      (,godot-256-class (:foreground ,godot-256-yellow))))

   `(helm-bookmarks-su
     ((,godot-class (:foreground ,godot-orange))
      (,godot-256-class (:foreground ,godot-256-orange))))

   `(helm-buffer-file
     ((,godot-class (:foreground ,godot-foreground))
      (,godot-256-class (:foreground ,godot-256-foreground))))

   `(helm-buffer-directory
     ((,godot-class (:foreground ,godot-blue))
      (,godot-256-class (:foreground ,godot-256-blue))))

   `(helm-buffer-process
     ((,godot-class (:foreground ,godot-comments))
      (,godot-256-class (:foreground ,godot-256-comments))))

   `(helm-buffer-saved-out
     ((,godot-class (:foreground ,godot-red
                                 :background ,godot-background
                                 :inverse-video t))
      (,godot-256-class (:foreground ,godot-256-red
                                     :background ,godot-256-background
                                     :inverse-video t))))

   `(helm-buffer-size
     ((,godot-class (:foreground ,godot-comments))
      (,godot-256-class (:foreground ,godot-256-comments))))

   `(helm-candidate-number
     ((,godot-class (:background ,godot-highlight-line
                                 :foreground ,godot-emphasis
                                 :bold t))
      (,godot-256-class (:background ,godot-256-highlight-line
                                     :foreground ,godot-256-emphasis
                                     :bold t))))

   `(helm-ff-directory
     ((,godot-class (:foreground ,godot-blue))
      (,godot-256-class (:foreground ,godot-256-blue))))

   `(helm-ff-executable
     ((,godot-class (:foreground ,godot-green))
      (,godot-256-class (:foreground ,godot-256-green))))

   `(helm-ff-file
     ((,godot-class (:background ,godot-background
                                 :foreground ,godot-foreground))
      (,godot-256-class (:background ,godot-256-background
                                     :foreground ,godot-256-foreground))))

   `(helm-ff-invalid-symlink
     ((,godot-class (:background ,godot-background
                                 :foreground ,godot-orange
                                 :slant italic))
      (,godot-256-class (:background ,godot-256-background
                                     :foreground ,godot-256-orange
                                     :slant italic))))

   `(helm-ff-prefix
     ((,godot-class (:background ,godot-green
                                 :foreground ,godot-background))
      (,godot-256-class (:background ,godot-256-green
                                     :foreground ,godot-256-background))))

   `(helm-ff-symlink
     ((,godot-class (:foreground ,godot-cyan))
      (,godot-256-class (:foreground ,godot-256-cyan))))

   `(helm-grep-file
     ((,godot-class (:foreground ,godot-cyan
                                 :underline t))
      (,godot-256-class (:foreground ,godot-256-cyan
                                     :underline t))))

   `(helm-grep-finish
     ((,godot-class (:foreground ,godot-green))
      (,godot-256-class (:foreground ,godot-256-green))))

   `(helm-grep-lineno
     ((,godot-class (:foreground ,godot-orange))
      (,godot-256-class (:foreground ,godot-256-orange))))

   `(helm-grep-match
     ((,godot-class (:inherit helm-match))
      (,godot-256-class (:inherit helm-match))))

   `(helm-grep-running
     ((,godot-class (:foreground ,godot-red))
      (,godot-256-class (:foreground ,godot-256-red))))

   `(helm-header
     ((,godot-class (:inherit header-line))
      (,godot-256-class (:inherit terminal-header-line))))

   `(helm-lisp-completion-info
     ((,godot-class (:foreground ,godot-foreground))
      (,godot-256-class (:foreground ,godot-256-foreground))))

   `(helm-lisp-show-completion
     ((,godot-class (:foreground ,godot-yellow
                                 :background ,godot-highlight-line
                                 :bold t))
      (,godot-256-class (:foreground ,godot-256-yellow
                                     :background ,godot-256-highlight-line
                                     :bold t))))

   `(helm-M-x-key
     ((,godot-class (:foreground ,godot-orange
                                 :underline t))
      (,godot-256-class (:foreground ,godot-256-orange
                                     :underline t))))

   `(helm-moccur-buffer
     ((,godot-class (:foreground ,godot-cyan
                                 :underline t))
      (,godot-256-class (:foreground ,godot-256-cyan
                                     :underline t))))

   `(helm-match
     ((,godot-class (:foreground ,godot-green :inherit bold))
      (,godot-256-class (:foreground ,godot-256-green :inherit bold))))

   `(helm-match-item
     ((,godot-class (:inherit helm-match))
      (,godot-256-class (:inherit helm-match))))

   `(helm-selection
     ((,godot-class (:background ,godot-highlight
                                 :inherit bold
                                 :underline nil))
      (,godot-256-class (:background ,godot-256-highlight
                                     :inherit bold
                                     :underline nil))))

   `(helm-selection-line
     ((,godot-class (:background ,godot-highlight-line
                                 :foreground ,godot-emphasis
                                 :underline nil))
      (,godot-256-class (:background ,godot-256-highlight-line
                                     :foreground ,godot-256-emphasis
                                     :underline nil))))

   `(helm-separator
     ((,godot-class (:foreground ,godot-gray))
      (,godot-256-class (:foreground ,godot-256-gray))))

   `(helm-source-header
     ((,godot-class (:background ,godot-violet-l
                                 :foreground ,godot-background
                                 :underline nil))
      (,godot-256-class (:background ,godot-256-violet-l
                                     :foreground ,godot-256-background
                                     :underline nil))))

   `(helm-swoop-target-line-face
     ((,godot-class (:background ,godot-highlight-line))
      (,godot-256-class (:background ,godot-256-highlight-line))))

   `(helm-swoop-target-line-block-face
     ((,godot-class (:background ,godot-highlight-line))
      (,godot-256-class (:background ,godot-256-highlight-line))))

   `(helm-swoop-target-word-face
     ((,godot-class (:foreground ,godot-green))
      (,godot-256-class (:foreground ,godot-256-green))))

   `(helm-time-zone-current
     ((,godot-class (:foreground ,godot-green))
      (,godot-256-class (:foreground ,godot-256-green))))

   `(helm-time-zone-home
     ((,godot-class (:foreground ,godot-red))
      (,godot-256-class (:foreground ,godot-256-red))))

   `(helm-visible-mark
     ((,godot-class (:background ,godot-background
                                 :foreground ,godot-magenta :bold t))
      (,godot-256-class (:background ,godot-256-background
                                     :foreground ,godot-256-magenta :bold t))))

   ;; helm-ls-git
   `(helm-ls-git-modified-not-staged-face
     ((,godot-class :foreground ,godot-blue)
      (,godot-256-class  :foreground ,godot-256-blue)))

   `(helm-ls-git-modified-and-staged-face
     ((,godot-class :foreground ,godot-blue-l)
      (,godot-256-class  :foreground ,godot-256-blue-l)))

   `(helm-ls-git-renamed-modified-face
     ((,godot-class :foreground ,godot-blue-l)
      (,godot-256-class  :foreground ,godot-256-blue-l)))

   `(helm-ls-git-untracked-face
     ((,godot-class :foreground ,godot-orange)
      (,godot-256-class  :foreground ,godot-256-orange)))

   `(helm-ls-git-added-copied-face
     ((,godot-class :foreground ,godot-green)
      (,godot-256-class  :foreground ,godot-256-green)))

   `(helm-ls-git-added-modified-face
     ((,godot-class :foreground ,godot-green-l)
      (,godot-256-class  :foreground ,godot-256-green-l)))

   `(helm-ls-git-deleted-not-staged-face
     ((,godot-class :foreground ,godot-red)
      (,godot-256-class  :foreground ,godot-256-red)))

   `(helm-ls-git-deleted-and-staged-face
     ((,godot-class :foreground ,godot-red-l)
      (,godot-256-class  :foreground ,godot-256-red-l)))

   `(helm-ls-git-conflict-face
     ((,godot-class :foreground ,godot-yellow)
      (,godot-256-class  :foreground ,godot-256-yellow)))

   ;; hi-lock-mode
   `(hi-yellow
     ((,godot-class (:foreground ,godot-yellow-lc
                                 :background ,godot-yellow-hc))
      (,godot-256-class (:foreground ,godot-256-yellow-lc
                                     :background ,godot-256-yellow-hc))))

   `(hi-pink
     ((,godot-class (:foreground ,godot-magenta-lc
                                 :background ,godot-magenta-hc))
      (,godot-256-class (:foreground ,godot-256-magenta-lc
                                     :background ,godot-256-magenta-hc))))

   `(hi-green
     ((,godot-class (:foreground ,godot-green-lc
                                 :background ,godot-green-hc))
      (,godot-256-class (:foreground ,godot-256-green-lc
                                     :background ,godot-256-green-hc))))

   `(hi-blue
     ((,godot-class (:foreground ,godot-blue-lc
                                 :background ,godot-blue-hc))
      (,godot-256-class (:foreground ,godot-256-blue-lc
                                     :background ,godot-256-blue-hc))))

   `(hi-black-b
     ((,godot-class (:foreground ,godot-emphasis
                                 :background ,godot-background
                                 :weight bold))
      (,godot-256-class (:foreground ,godot-256-emphasis
                                     :background ,godot-256-background
                                     :weight bold))))

   `(hi-blue-b
     ((,godot-class (:foreground ,godot-blue-lc
                                 :weight bold))
      (,godot-256-class (:foreground ,godot-256-blue-lc
                                     :weight bold))))

   `(hi-green-b
     ((,godot-class (:foreground ,godot-green-lc
                                 :weight bold))
      (,godot-256-class (:foreground ,godot-256-green-lc
                                     :weight bold))))

   `(hi-red-b
     ((,godot-class (:foreground ,godot-red
                                 :weight bold))
      (,godot-256-class (:foreground ,godot-256-red
                                     :weight bold))))

   `(hi-black-hb
     ((,godot-class (:foreground ,godot-emphasis
                                 :background ,godot-background
                                 :weight bold))
      (,godot-256-class (:foreground ,godot-256-emphasis
                                     :background ,godot-256-background
                                     :weight bold))))

   ;; highlight-changes
   `(highlight-changes
     ((,godot-class (:foreground ,godot-orange))
      (,godot-256-class (:foreground ,godot-256-orange))))

   `(highlight-changes-delete
     ((,godot-class (:foreground ,godot-red
                                 :underline t))
      (,godot-256-class (:foreground ,godot-256-red
                                     :underline t))))

   ;; highlight-indentation
   `(highlight-indentation-face
     ((,godot-class (:background ,godot-gray))
      (,godot-256-class (:background ,godot-256-gray))))

   `(highlight-indentation-current-column-face
     ((,godot-class (:background ,godot-gray))
      (,godot-256-class (:background ,godot-256-gray))))

   ;; highlight-symbol
   `(highlight-symbol-face
     ((,godot-class (:background ,godot-highlight))
      (,godot-256-class (:background ,godot-256-highlight))))

   ;; hl-line-mode
   `(hl-line
     ((,godot-class (:background ,godot-highlight-line))
      (,godot-256-class (:background ,godot-256-highlight-line))))

   `(hl-line-face
     ((,godot-class (:background ,godot-highlight-line))
      (,godot-256-class (:background ,godot-256-highlight-line))))

   ;; ido-mode
   `(ido-first-match
     ((,godot-class (:foreground ,godot-yellow
                                 :weight normal))
      (,godot-256-class (:foreground ,godot-256-yellow
                                     :weight normal))))

   `(ido-only-match
     ((,godot-class (:foreground ,godot-background
                                 :background ,godot-yellow
                                 :weight normal))
      (,godot-256-class (:foreground ,godot-256-background
                                     :background ,godot-256-yellow
                                     :weight normal))))

   `(ido-subdir
     ((,godot-class (:foreground ,godot-blue))
      (,godot-256-class (:foreground ,godot-256-blue))))

   `(ido-incomplete-regexp
     ((,godot-class (:foreground ,godot-red
                                 :weight bold ))
      (,godot-256-class (:foreground ,godot-256-red
                                     :weight bold ))))

   `(ido-indicator
     ((,godot-class (:background ,godot-red
                                 :foreground ,godot-background
                                 :width condensed))
      (,godot-256-class (:background ,godot-256-red
                                     :foreground ,godot-256-background
                                     :width condensed))))

   `(ido-virtual
     ((,godot-class (:foreground ,godot-cyan))
      (,godot-256-class (:foreground ,godot-256-cyan))))

   ;; info
   `(info-header-xref
     ((,godot-class (:foreground ,godot-green
                                 :inherit bold
                                 :underline t))
      (,godot-256-class (:foreground ,godot-256-green
                                     :inherit bold
                                     :underline t))))

   `(info-menu
     ((,godot-class (:foreground ,godot-blue))
      (,godot-256-class (:foreground ,godot-256-blue))))

   `(info-node
     ((,godot-class (:foreground ,godot-violet
                                 :inherit bold))
      (,godot-256-class (:foreground ,godot-256-violet
                                     :inherit bold))))

   `(info-quoted-name
     ((,godot-class (:foreground ,godot-orange))
      (,godot-256-class (:foreground ,godot-256-orange))))

   `(info-reference-item
     ((,godot-class (:background nil
                                 :underline t
                                 :inherit bold))
      (,godot-256-class (:background nil
                                     :underline t
                                     :inherit bold))))

   `(info-string
     ((,godot-class (:foreground ,godot-yellow))
      (,godot-256-class (:foreground ,godot-256-yellow))))

   `(info-title-1
     ((,godot-class (:height ,godot-height-plus-4))
      (,godot-256-class (:height ,godot-height-plus-4))))

   `(info-title-2
     ((,godot-class (:height ,godot-height-plus-3))
      (,godot-256-class (:height ,godot-height-plus-3))))

   `(info-title-3
     ((,godot-class (:height ,godot-height-plus-2))
      (,godot-256-class (:height ,godot-height-plus-2))))

   `(info-title-4
     ((,godot-class (:height ,godot-height-plus-1))
      (,godot-256-class (:height ,godot-height-plus-1))))

   ;; ivy
   `(ivy-current-match
     ((,godot-class (:background ,godot-gray :inherit bold))
      (,godot-256-class (:background ,godot-gray-l :inherit bold))))

   `(ivy-minibuffer-match-face-1
     ((,godot-class (:inherit bold))
      (,godot-256-class (:inherit bold))))

   `(ivy-minibuffer-match-face-2
     ((,godot-class (:foreground ,godot-violet
                                 :underline t))
      (,godot-256-class (:foreground ,godot-256-violet
                                     :underline t))))

   `(ivy-minibuffer-match-face-3
     ((,godot-class (:foreground ,godot-green
                                 :underline t))
      (,godot-256-class (:foreground ,godot-256-green
                                     :underline t))))

   `(ivy-minibuffer-match-face-4
     ((,godot-class (:foreground ,godot-yellow
                                 :underline t))
      (,godot-256-class (:foreground ,godot-256-yellow
                                     :underline t))))

   `(ivy-remote
     ((,godot-class (:foreground ,godot-blue))
      (,godot-256-class (:foreground ,godot-256-blue))))

   `(swiper-line-face
     ((,godot-class (:background ,godot-highlight-line))))

   `(swiper-match-face-1
     ((,godot-class (:background ,godot-gray-d))))

   `(swiper-match-face-2
     ((,godot-class (:background ,godot-green))))

   `(swiper-match-face-3
     ((,godot-class (:background ,godot-orange))))

   `(swiper-match-face-4
     ((,godot-class (:background ,godot-magenta))))

   ;; jabber
   `(jabber-activity-face
     ((,godot-class (:weight bold
                             :foreground ,godot-red))
      (,godot-256-class (:weight bold
                                 :foreground ,godot-256-red))))

   `(jabber-activity-personal-face
     ((,godot-class (:weight bold
                             :foreground ,godot-blue))
      (,godot-256-class (:weight bold
                                 :foreground ,godot-256-blue))))

   `(jabber-chat-error
     ((,godot-class (:weight bold
                             :foreground ,godot-red))
      (,godot-256-class (:weight bold
                                 :foreground ,godot-256-red))))

   `(jabber-chat-prompt-foreign
     ((,godot-class (:weight bold
                             :foreground ,godot-red))
      (,godot-256-class (:weight bold
                                 :foreground ,godot-256-red))))

   `(jabber-chat-prompt-local
     ((,godot-class (:weight bold
                             :foreground ,godot-blue))
      (,godot-256-class (:weight bold
                                 :foreground ,godot-256-blue))))

   `(jabber-chat-prompt-system
     ((,godot-class (:weight bold
                             :foreground ,godot-green))
      (,godot-256-class (:weight bold
                                 :foreground ,godot-256-green))))

   `(jabber-chat-text-foreign
     ((,godot-class (:foreground ,godot-comments))
      (,godot-256-class (:foreground ,godot-256-comments))))

   `(jabber-chat-text-local
     ((,godot-class (:foreground ,godot-foreground))
      (,godot-256-class (:foreground ,godot-256-foreground))))

   `(jabber-chat-rare-time-face
     ((,godot-class (:underline t
                                :foreground ,godot-green))
      (,godot-256-class (:underline t
                                    :foreground ,godot-256-green))))

   `(jabber-roster-user-away
     ((,godot-class (:slant italic
                            :foreground ,godot-green))
      (,godot-256-class (:slant italic
                                :foreground ,godot-256-green))))

   `(jabber-roster-user-chatty
     ((,godot-class (:weight bold
                             :foreground ,godot-orange))
      (,godot-256-class (:weight bold
                                 :foreground ,godot-256-orange))))

   `(jabber-roster-user-dnd
     ((,godot-class (:slant italic
                            :foreground ,godot-red))
      (,godot-256-class (:slant italic
                                :foreground ,godot-256-red))))

   `(jabber-roster-user-error
     ((,godot-class (:weight light
                             :slant italic
                             :foreground ,godot-red))
      (,godot-256-class (:weight light
                                 :slant italic
                                 :foreground ,godot-256-red))))

   `(jabber-roster-user-offline
     ((,godot-class (:foreground ,godot-comments))
      (,godot-256-class (:foreground ,godot-256-comments))))

   `(jabber-roster-user-online
     ((,godot-class (:weight bold
                             :foreground ,godot-blue))
      (,godot-256-class (:weight bold
                                 :foreground ,godot-256-blue))))

   `(jabber-roster-user-xa
     ((,godot-class (:slant italic
                            :foreground ,godot-magenta))
      (,godot-256-class (:slant italic
                                :foreground ,godot-256-magenta))))

   ;; js2-mode colors
   `(js2-error
     ((,godot-class (:foreground ,godot-red))
      (,godot-256-class (:foreground ,godot-256-red))))

   `(js2-external-variable
     ((,godot-class (:foreground ,godot-orange))
      (,godot-256-class (:foreground ,godot-256-orange))))

   `(js2-function-call
     ((,godot-class (:foreground ,godot-foreground))
      (,godot-256-class (:foreground ,godot-256-foreground))))

   `(js2-function-param
     ((,godot-class (:foreground ,godot-orange))
      (,godot-256-class (:foreground ,godot-256-orange))))

   `(js2-instance-member
     ((,godot-class (:foreground ,godot-violet))
      (,godot-256-class (:foreground ,godot-256-violet))))

   `(js2-jsdoc-html-tag-delimiter
     ((,godot-class (:foreground ,godot-green))
      (,godot-256-class (:foreground ,godot-256-green))))

   `(js2-jsdoc-html-tag-name
     ((,godot-class (:foreground ,godot-green))
      (,godot-256-class (:foreground ,godot-256-green))))

   `(js2-jsdoc-tag
     ((,godot-class (:foreground ,godot-violet))
      (,godot-256-class (:foreground ,godot-256-violet))))

   `(js2-jsdoc-type
     ((,godot-class (:foreground ,godot-blue))
      (,godot-256-class (:foreground ,godot-256-blue))))

   `(js2-jsdoc-value
     ((,godot-class (:foreground ,godot-orange))
      (,godot-256-class (:foreground ,godot-256-orange))))

   `(js2-magic-paren
     ((,godot-class (:underline t))
      (,godot-256-class (:underline t))))

   `(js2-object-property
     ((,godot-class (:foreground ,godot-foreground))
      (,godot-256-class (:foreground ,godot-256-foreground))))

   `(js2-private-function-call
     ((,godot-class (:foreground ,godot-violet))
      (,godot-256-class (:foreground ,godot-256-violet))))

   `(js2-private-member
     ((,godot-class (:foreground ,godot-blue))
      (,godot-256-class (:foreground ,godot-256-blue))))

   `(js2-warning
     ((,godot-class (:underline ,godot-orange))
      (,godot-256-class (:underline ,godot-256-orange))))

   ;; jedi
   `(jedi:highlight-function-argument
     ((,godot-class (:inherit bold))
      (,godot-256-class (:inherit bold))))

   ;; linum-mode
   `(linum
     ((,godot-class (:foreground ,godot-line-number
                                 :background ,godot-fringe-bg
                                 :inherit default
                                 :underline nil))
      (,godot-256-class (:foreground ,godot-256-line-number
                                     :background ,godot-256-fringe-bg
                                     :inherit default
                                     :underline nil))))

   ;; line-number (>= Emacs26)
   `(line-number
     ((,godot-class (:foreground ,godot-line-number
                                 :background ,godot-fringe-bg
                                 :inherit default
                                 :underline nil))
      (,godot-256-class (:foreground ,godot-256-line-number
                                     :background ,godot-256-fringe-bg
                                     :inherit default
                                     :underline nil))))
   `(line-number-current-line
     ((,godot-class (:foreground ,godot-foreground
                                 :background ,godot-fringe-bg
                                 :inherit default
                                 :underline nil))
      (,godot-256-class (:foreground ,godot-256-foreground
                                     :background ,godot-256-fringe-bg
                                     :inherit default
                                     :underline nil))))

   ;; linum-relative-current-face
   `(linum-relative-current-face
     ((,godot-class (:foreground ,godot-line-number
                                 :background ,godot-highlight-line
                                 :underline nil))
      (,godot-256-class (:foreground ,godot-256-line-number
                                     :background ,godot-256-highlight-line
                                     :underline nil))))

   ;; lsp-mode
   `(lsp-ui-doc-header
     ((,godot-class (:inherit org-document-title))
      (,godot-256-class (:inherit org-document-title))))

   `(lsp-ui-doc-background
     ((,godot-class (:background ,godot-highlight-line))
      (,godot-256-class (:background ,godot-highlight-line))))

   ;; lusty-explorer
   `(lusty-directory-face
     ((,godot-class (:inherit digodot-red-directory))
      (,godot-256-class (:inherit digodot-red-directory))))

   `(lusty-file-face
     ((,godot-class nil)
      (,godot-256-class  nil)))

   `(lusty-match-face
     ((,godot-class (:inherit ido-first-match))
      (,godot-256-class (:inherit ido-first-match))))

   `(lusty-slash-face
     ((,godot-class (:foreground ,godot-cyan
                                 :weight bold))
      (,godot-256-class (:foreground ,godot-256-cyan
                                     :weight bold))))

   ;; magit
   `(magit-bisect-good
     ((,godot-class (:foreground ,godot-green))
      (,godot-256-class (:foreground ,godot-256-green))))

   `(magit-bisect-skip
     ((,godot-class (:foreground ,godot-orange))
      (,godot-256-class (:foreground ,godot-256-orange))))

   `(magit-bisect-bad
     ((,godot-class (:foreground ,godot-red))
      (,godot-256-class (:foreground ,godot-256-red))))

   `(magit-blame-highlight
     ((,godot-class (:foreground ,godot-foreground
                                 :background ,godot-highlight-alt))
      (,godot-256-class (:foreground ,godot-256-foreground
                                     :background ,godot-256-highlight-alt))))

   `(magit-diff-file-heading-selection
     ((,godot-class (:inherit magit-diff-file-heading-highlight
                              :foreground ,godot-orange-d))
      (,godot-256-class (:inherit magit-diff-file-heading-highlight
                                  :foreground ,godot-256-orange-d))))

   `(magit-diff-hunk-heading
     ((,godot-class (:foreground ,godot-gray-d
                                 :background ,godot-gray-l))
      (,godot-256-class (:foreground ,godot-256-gray-d
                                     :background ,godot-256-gray-l))))

   `(magit-diff-hunk-heading-highlight
     ((,godot-class (:foreground ,godot-background
                                 :background ,godot-foreground))
      (,godot-256-class (:foreground ,godot-256-background
                                     :background ,godot-256-foreground))))

   `(magit-diff-hunk-heading-selection
     ((,godot-class (:inherit magit-diff-hunk-heading-highlight
                              :foreground ,godot-orange))
      (,godot-256-class (:inherit magit-diff-hunk-heading-highlight
                                  :foreground ,godot-256-orange))))

   `(magit-diff-lines-heading
     ((,godot-class (:inherit magit-diff-hunk-heading-highlight
                              :foreground ,godot-background
                              :background ,godot-orange-l))
      (,godot-256-class (:inherit magit-diff-hunk-heading-highlight
                                  :foreground ,godot-256-background
                                  :background ,godot-256-orange-l))))

   `(magit-diff-added
     ((,godot-class (:foreground ,godot-green
                                 :background ,godot-background))
      (,godot-256-class (:foreground ,godot-256-green
                                     :background ,godot-256-background))))

   `(magit-diff-removed
     ((,godot-class (:foreground ,godot-red
                                 :background ,godot-background))
      (,godot-256-class (:foreground ,godot-256-red
                                     :background ,godot-256-background))))

   `(magit-diff-base
     ((,godot-class (:foreground ,godot-yellow
                                 :background ,godot-background))
      (,godot-256-class (:foreground ,godot-256-yellow
                                     :background ,godot-256-background))))

   `(magit-diff-context
     ((,godot-class (:foreground ,godot-gray-l))
      (,godot-256-class (:foreground ,godot-256-gray-l))))

   `(magit-diff-added-highlight
     ((,godot-class (:foreground ,godot-green
                                 :background ,godot-highlight-alt))
      (,godot-256-class (:foreground ,godot-256-green
                                     :background ,godot-256-highlight-alt))))

   `(magit-diff-removed-highlight
     ((,godot-class (:foreground ,godot-red
                                 :background ,godot-highlight-alt))
      (,godot-256-class (:foreground ,godot-256-red
                                     :background ,godot-256-highlight-alt))))

   `(magit-diff-base-highlight
     ((,godot-class (:foreground ,godot-yellow
                                 :background ,godot-highlight-alt))
      (,godot-256-class (:foreground ,godot-256-yellow
                                     :background ,godot-256-highlight-alt))))

   `(magit-diff-context-highlight
     ((,godot-class (:foreground ,godot-foreground
                                 :background ,godot-highlight-alt))
      (,godot-256-class (:foreground ,godot-256-foreground
                                     :background ,godot-256-highlight-alt))))

   `(magit-diffstat-added
     ((,godot-class (:foreground ,godot-green))
      (,godot-256-class (:foreground ,godot-256-green))))

   `(magit-diffstat-removed
     ((,godot-class (:foreground ,godot-red))
      (,godot-256-class (:foreground ,godot-256-red))))

   `(magit-log-graph
     ((,godot-class (:foreground ,godot-comments))
      (,godot-256-class (:foreground ,godot-256-comments))))

   `(magit-log-author
     ((,godot-class (:foreground ,godot-red-d
                                 :slant normal
                                 :weight normal))
      (,godot-256-class (:foreground ,godot-256-red-d
                                     :slant normal
                                     :weight normal))))

   `(magit-log-date
     ((,godot-class (:foreground ,godot-gray
                                 :slant normal
                                 :weight normal))
      (,godot-256-class (:foreground ,godot-256-gray
                                     :slant normal
                                     :weight normal))))

   `(magit-process-ok
     ((,godot-class (:inherit magit-section-heading
                              :foreground ,godot-green))
      (,godot-256-class (:inherit magit-section-heading
                                  :foreground ,godot-256-green))))

   `(magit-process-ng
     ((,godot-class (:inherit magit-section-heading
                              :foreground ,godot-red))
      (,godot-256-class (:inherit magit-section-heading
                                  :foreground ,godot-256-red))))

   `(magit-reflog-commit
     ((,godot-class (:foreground ,godot-green))
      (,godot-256-class (:foreground ,godot-256-green))))

   `(magit-reflog-amend
     ((,godot-class (:foreground ,godot-magenta))
      (,godot-256-class (:foreground ,godot-256-magenta))))

   `(magit-reflog-merge
     ((,godot-class (:foreground ,godot-green))
      (,godot-256-class (:foreground ,godot-256-green))))

   `(magit-reflog-checkout
     ((,godot-class (:foreground ,godot-blue))
      (,godot-256-class (:foreground ,godot-256-blue))))

   `(magit-reflog-reset
     ((,godot-class (:foreground ,godot-red))
      (,godot-256-class (:foreground ,godot-256-red))))

   `(magit-reflog-rebase
     ((,godot-class (:foreground ,godot-violet))
      (,godot-256-class (:foreground ,godot-256-violet))))

   `(magit-reflog-cherry-pick
     ((,godot-class (:foreground ,godot-green))
      (,godot-256-class (:foreground ,godot-256-green))))

   `(magit-reflog-remote
     ((,godot-class (:foreground ,godot-cyan))
      (,godot-256-class (:foreground ,godot-256-cyan))))

   `(magit-reflog-other
     ((,godot-class (:foreground ,godot-cyan))
      (,godot-256-class (:foreground ,godot-256-cyan))))

   `(magit-section-highlight
     ((,godot-class (:background ,godot-highlight-line))
      (,godot-256-class (:background ,godot-256-highlight-line))))

   `(magit-section-heading
     ((,godot-class (:foreground ,godot-yellow
                                 :weight bold))
      (,godot-256-class (:foreground ,godot-256-yellow
                                     :weight bold))))

   `(magit-section-heading-selection
     ((,godot-class (:foreground ,godot-orange))
      (,godot-256-class (:foreground ,godot-256-orange))))

   `(magit-sequence-stop
     ((,godot-class (:foreground ,godot-cyan))
      (,godot-256-class (:foreground ,godot-256-cyan))))

   `(magit-sequence-part
     ((,godot-class (:foreground ,godot-orange))
      (,godot-256-class (:foreground ,godot-256-orange))))

   `(magit-sequence-head
     ((,godot-class (:foreground ,godot-blue))
      (,godot-256-class (:foreground ,godot-256-blue))))

   `(magit-sequence-drop
     ((,godot-class (:foreground ,godot-red))
      (,godot-256-class (:foreground ,godot-256-red))))

   `(magit-dimmed
     ((,godot-class (:foreground ,godot-comments))
      (,godot-256-class (:foreground ,godot-256-comments))))

   `(magit-hash
     ((,godot-class (:foreground ,godot-comments))
      (,godot-256-class (:foreground ,godot-256-comments))))

   `(magit-tag
     ((,godot-class (:foreground ,godot-orange))
      (,godot-256-class (:foreground ,godot-256-orange))))

   `(magit-branch-remote
     ((,godot-class (:foreground ,godot-green))
      (,godot-256-class (:foreground ,godot-256-green))))

   `(magit-branch-local
     ((,godot-class (:foreground ,godot-blue))
      (,godot-256-class (:foreground ,godot-256-blue))))

   `(magit-refname
     ((,godot-class (:foreground ,godot-comments))
      (,godot-256-class (:foreground ,godot-256-comments))))

   `(magit-signature-good
     ((,godot-class (:foreground ,godot-green-d))
      (,godot-256-class (:foreground ,godot-256-green-d))))

   `(magit-signature-bad
     ((,godot-class (:foreground ,godot-red-d
                                 :weight bold))
      (,godot-256-class (:foreground ,godot-256-red-d
                                     :weight bold))))

   `(magit-signature-untrusted
     ((,godot-class (:foreground ,godot-cyan))
      (,godot-256-class (:foreground ,godot-256-cyan))))

   `(magit-signature-expired
     ((,godot-class (:foreground ,godot-orange))
      (,godot-256-class (:foreground ,godot-256-orange))))

   `(magit-signature-revoked
     ((,godot-class (:foreground ,godot-magenta))
      (,godot-256-class (:foreground ,godot-256-magenta))))

   `(magit-signature-error
     ((,godot-class (:foreground ,godot-red-l))
      (,godot-256-class (:foreground ,godot-256-red-l))))

   `(magit-cherry-unmatched
     ((,godot-class (:foreground ,godot-cyan))
      (,godot-256-class (:foreground ,godot-256-cyan))))

   `(magit-cherry-equivalent
     ((,godot-class (:foreground ,godot-magenta))
      (,godot-256-class (:foreground ,godot-256-magenta))))

   ;; man
   `(Man-overstrike
     ((,godot-class (:foreground ,godot-blue
                                 :weight bold))
      (,godot-256-class (:foreground ,godot-256-blue
                                     :weight bold))))

   `(Man-reverse
     ((,godot-class (:foreground ,godot-orange))
      (,godot-256-class (:foreground ,godot-256-orange))))

   `(Man-underline
     ((,godot-class (:foreground ,godot-green :underline t))
      (,godot-256-class (:foreground ,godot-256-green :underline t))))

   ;; monky
   `(monky-section-title
     ((,godot-class (:foreground ,godot-yellow
                                 :weight bold))
      (,godot-256-class (:foreground ,godot-256-yellow
                                     :weight bold))))

   `(monky-diff-add
     ((,godot-class (:foreground ,godot-green))
      (,godot-256-class (:foreground ,godot-256-green))))

   `(monky-diff-del
     ((,godot-class (:foreground ,godot-red))
      (,godot-256-class (:foreground ,godot-256-red))))

   ;; markdown-mode
   `(markdown-header-face
     ((,godot-class (:foreground ,godot-green))
      (,godot-256-class (:foreground ,godot-256-green))))

   `(markdown-header-face-1
     ((,godot-class (:inherit markdown-header-face
                              :height ,godot-height-plus-4))
      (,godot-256-class (:inherit markdown-header-face
                                  :height ,godot-height-plus-4))))

   `(markdown-header-face-2
     ((,godot-class (:inherit markdown-header-face
                              :height ,godot-height-plus-3))
      (,godot-256-class (:inherit markdown-header-face
                                  :height ,godot-height-plus-3))))

   `(markdown-header-face-3
     ((,godot-class (:inherit markdown-header-face
                              :height ,godot-height-plus-2))
      (,godot-256-class (:inherit markdown-header-face
                                  :height ,godot-height-plus-2))))

   `(markdown-header-face-4
     ((,godot-class (:inherit markdown-header-face
                              :height ,godot-height-plus-1))
      (,godot-256-class (:inherit markdown-header-face
                                  :height ,godot-height-plus-1))))

   `(markdown-header-face-5
     ((,godot-class (:inherit markdown-header-face))
      (,godot-256-class (:inherit markdown-header-face))))

   `(markdown-header-face-6
     ((,godot-class (:inherit markdown-header-face))
      (,godot-256-class (:inherit markdown-header-face))))

   ;; message-mode
   `(message-cited-text
     ((,godot-class (:foreground ,godot-comments))
      (,godot-256-class (:foreground ,godot-256-comments))))

   `(message-header-name
     ((,godot-class (:foreground ,godot-comments))
      (,godot-256-class (:foreground ,godot-256-comments))))

   `(message-header-other
     ((,godot-class (:foreground ,godot-foreground
                                 :weight normal))
      (,godot-256-class (:foreground ,godot-256-foreground
                                     :weight normal))))

   `(message-header-to
     ((,godot-class (:foreground ,godot-foreground
                                 :weight normal))
      (,godot-256-class (:foreground ,godot-256-foreground
                                     :weight normal))))

   `(message-header-cc
     ((,godot-class (:foreground ,godot-foreground
                                 :weight normal))
      (,godot-256-class (:foreground ,godot-256-foreground
                                     :weight normal))))

   `(message-header-newsgroups
     ((,godot-class (:foreground ,godot-yellow
                                 :weight bold))
      (,godot-256-class (:foreground ,godot-256-yellow
                                     :weight bold))))

   `(message-header-subject
     ((,godot-class (:foreground ,godot-cyan
                                 :weight normal))
      (,godot-256-class (:foreground ,godot-256-cyan
                                     :weight normal))))

   `(message-header-xheader
     ((,godot-class (:foreground ,godot-cyan))
      (,godot-256-class (:foreground ,godot-256-cyan))))

   `(message-mml
     ((,godot-class (:foreground ,godot-yellow
                                 :weight bold))
      (,godot-256-class (:foreground ,godot-256-yellow
                                     :weight bold))))

   `(message-separator
     ((,godot-class (:foreground ,godot-comments
                                 :slant italic))
      (,godot-256-class (:foreground ,godot-256-comments
                                     :slant italic))))

   ;; mew
   `(mew-face-header-subject
     ((,godot-class (:foreground ,godot-orange))
      (,godot-256-class (:foreground ,godot-256-orange))))

   `(mew-face-header-from
     ((,godot-class (:foreground ,godot-yellow))
      (,godot-256-class (:foreground ,godot-256-yellow))))

   `(mew-face-header-date
     ((,godot-class (:foreground ,godot-green))
      (,godot-256-class (:foreground ,godot-256-green))))

   `(mew-face-header-to
     ((,godot-class (:foreground ,godot-red))
      (,godot-256-class (:foreground ,godot-256-red))))

   `(mew-face-header-key
     ((,godot-class (:foreground ,godot-green))
      (,godot-256-class (:foreground ,godot-256-green))))

   `(mew-face-header-private
     ((,godot-class (:foreground ,godot-green))
      (,godot-256-class (:foreground ,godot-256-green))))

   `(mew-face-header-important
     ((,godot-class (:foreground ,godot-blue))
      (,godot-256-class (:foreground ,godot-256-blue))))

   `(mew-face-header-marginal
     ((,godot-class (:foreground ,godot-foreground
                                 :weight bold))
      (,godot-256-class (:foreground ,godot-256-foreground
                                     :weight bold))))

   `(mew-face-header-warning
     ((,godot-class (:foreground ,godot-red))
      (,godot-256-class (:foreground ,godot-256-red))))

   `(mew-face-header-xmew
     ((,godot-class (:foreground ,godot-green))
      (,godot-256-class (:foreground ,godot-256-green))))

   `(mew-face-header-xmew-bad
     ((,godot-class (:foreground ,godot-red))
      (,godot-256-class (:foreground ,godot-256-red))))

   `(mew-face-body-url
     ((,godot-class (:foreground ,godot-orange))
      (,godot-256-class (:foreground ,godot-256-orange))))

   `(mew-face-body-comment
     ((,godot-class (:foreground ,godot-foreground
                                 :slant italic))
      (,godot-256-class (:foreground ,godot-256-foreground
                                     :slant italic))))

   `(mew-face-body-cite1
     ((,godot-class (:foreground ,godot-green))
      (,godot-256-class (:foreground ,godot-256-green))))

   `(mew-face-body-cite2
     ((,godot-class (:foreground ,godot-blue))
      (,godot-256-class (:foreground ,godot-256-blue))))

   `(mew-face-body-cite3
     ((,godot-class (:foreground ,godot-orange))
      (,godot-256-class (:foreground ,godot-256-orange))))

   `(mew-face-body-cite4
     ((,godot-class (:foreground ,godot-yellow))
      (,godot-256-class (:foreground ,godot-256-yellow))))

   `(mew-face-body-cite5
     ((,godot-class (:foreground ,godot-red))
      (,godot-256-class (:foreground ,godot-256-red))))

   `(mew-face-mark-review
     ((,godot-class (:foreground ,godot-blue))
      (,godot-256-class (:foreground ,godot-256-blue))))

   `(mew-face-mark-escape
     ((,godot-class (:foreground ,godot-green))
      (,godot-256-class (:foreground ,godot-256-green))))

   `(mew-face-mark-delete
     ((,godot-class (:foreground ,godot-red))
      (,godot-256-class (:foreground ,godot-256-red))))

   `(mew-face-mark-unlink
     ((,godot-class (:foreground ,godot-yellow))
      (,godot-256-class (:foreground ,godot-256-yellow))))

   `(mew-face-mark-refile
     ((,godot-class (:foreground ,godot-green))
      (,godot-256-class (:foreground ,godot-256-green))))

   `(mew-face-mark-unread
     ((,godot-class (:foreground ,godot-red))
      (,godot-256-class (:foreground ,godot-256-red))))

   `(mew-face-eof-message
     ((,godot-class (:foreground ,godot-green))
      (,godot-256-class (:foreground ,godot-256-green))))

   `(mew-face-eof-part
     ((,godot-class (:foreground ,godot-yellow))
      (,godot-256-class (:foreground ,godot-256-yellow))))

   ;; mingus
   `(mingus-directory-face
     ((,godot-class (:foreground ,godot-blue))
      (,godot-256-class (:foreground ,godot-256-blue))))

   `(mingus-pausing-face
     ((,godot-class (:foreground ,godot-magenta))
      (,godot-256-class (:foreground ,godot-256-magenta))))

   `(mingus-playing-face
     ((,godot-class (:foreground ,godot-cyan))
      (,godot-256-class (:foreground ,godot-256-cyan))))

   `(mingus-playlist-face
     ((,godot-class (:foreground ,godot-cyan ))
      (,godot-256-class (:foreground ,godot-256-cyan ))))

   `(mingus-song-file-face
     ((,godot-class (:foreground ,godot-yellow))
      (,godot-256-class (:foreground ,godot-256-yellow))))

   `(mingus-stopped-face
     ((,godot-class (:foreground ,godot-red))
      (,godot-256-class (:foreground ,godot-256-red))))

   ;; mmm
   `(mmm-init-submode-face
     ((,godot-class (:background ,godot-violet-d))
      (,godot-256-class (:background ,godot-256-violet-d))))

   `(mmm-cleanup-submode-face
     ((,godot-class (:background ,godot-orange-d))
      (,godot-256-class (:background ,godot-256-orange-d))))

   `(mmm-declaration-submode-face
     ((,godot-class (:background ,godot-cyan-d))
      (,godot-256-class (:background ,godot-256-cyan-d))))

   `(mmm-comment-submode-face
     ((,godot-class (:background ,godot-blue-d))
      (,godot-256-class (:background ,godot-256-blue-d))))

   `(mmm-output-submode-face
     ((,godot-class (:background ,godot-red-d))
      (,godot-256-class (:background ,godot-256-red-d))))

   `(mmm-special-submode-face
     ((,godot-class (:background ,godot-green-d))
      (,godot-256-class (:background ,godot-256-green-d))))

   `(mmm-code-submode-face
     ((,godot-class (:background ,godot-gray))
      (,godot-256-class (:background ,godot-256-gray))))

   `(mmm-default-submode-face
     ((,godot-class (:background ,godot-gray-d))
      (,godot-256-class (:background ,godot-256-gray-d))))

   ;; moccur
   `(moccur-current-line-face
     ((,godot-class (:underline t))
      (,godot-256-class (:underline t))))

   `(moccur-edit-done-face
     ((,godot-class (:foreground ,godot-comments
                                 :background ,godot-background
                                 :slant italic))
      (,godot-256-class (:foreground ,godot-256-comments
                                     :background ,godot-256-background
                                     :slant italic))))

   `(moccur-edit-face
     ((,godot-class (:background ,godot-yellow
                                 :foreground ,godot-background))
      (,godot-256-class (:background ,godot-256-yellow
                                     :foreground ,godot-256-background))))

   `(moccur-edit-file-face
     ((,godot-class (:background ,godot-highlight-line))
      (,godot-256-class (:background ,godot-256-highlight-line))))

   `(moccur-edit-reject-face
     ((,godot-class (:foreground ,godot-red))
      (,godot-256-class (:foreground ,godot-256-red))))

   `(moccur-face
     ((,godot-class (:background ,godot-highlight-line
                                 :foreground ,godot-emphasis
                                 :weight bold))
      (,godot-256-class (:background ,godot-256-highlight-line
                                     :foreground ,godot-256-emphasis
                                     :weight bold))))

   `(search-buffers-face
     ((,godot-class (:background ,godot-highlight-line
                                 :foreground ,godot-emphasis
                                 :weight bold))
      (,godot-256-class (:background ,godot-256-highlight-line
                                     :foreground ,godot-256-emphasis
                                     :weight bold))))

   `(search-buffers-header-face
     ((,godot-class (:background ,godot-highlight-line
                                 :foreground ,godot-yellow
                                 :weight bold))
      (,godot-256-class (:background ,godot-256-highlight-line
                                     :foreground ,godot-256-yellow
                                     :weight bold))))

   ;; mu4e
   `(mu4e-cited-1-face
     ((,godot-class (:foreground ,godot-green
                                 :slant italic
                                 :weight normal))
      (,godot-256-class (:foreground ,godot-256-green
                                     :slant italic
                                     :weight normal))))

   `(mu4e-cited-2-face
     ((,godot-class (:foreground ,godot-blue
                                 :slant italic
                                 :weight normal))
      (,godot-256-class (:foreground ,godot-256-blue
                                     :slant italic
                                     :weight normal))))

   `(mu4e-cited-3-face
     ((,godot-class (:foreground ,godot-orange
                                 :slant italic
                                 :weight normal))
      (,godot-256-class (:foreground ,godot-256-orange
                                     :slant italic
                                     :weight normal))))

   `(mu4e-cited-4-face
     ((,godot-class (:foreground ,godot-yellow
                                 :slant italic
                                 :weight normal))
      (,godot-256-class (:foreground ,godot-256-yellow
                                     :slant italic
                                     :weight normal))))

   `(mu4e-cited-5-face
     ((,godot-class (:foreground ,godot-cyan
                                 :slant italic
                                 :weight normal))
      (,godot-256-class (:foreground ,godot-256-cyan
                                     :slant italic
                                     :weight normal))))

   `(mu4e-cited-6-face
     ((,godot-class (:foreground ,godot-green
                                 :slant italic
                                 :weight normal))
      (,godot-256-class (:foreground ,godot-256-green
                                     :slant italic
                                     :weight normal))))

   `(mu4e-cited-7-face
     ((,godot-class (:foreground ,godot-blue
                                 :slant italic
                                 :weight normal))
      (,godot-256-class (:foreground ,godot-256-blue
                                     :slant italic
                                     :weight normal))))

   `(mu4e-flagged-face
     ((,godot-class (:foreground ,godot-magenta
                                 :weight bold))
      (,godot-256-class (:foreground ,godot-256-magenta
                                     :weight bold))))

   `(mu4e-view-url-number-face
     ((,godot-class (:foreground ,godot-yellow
                                 :weight normal))
      (,godot-256-class (:foreground ,godot-256-yellow
                                     :weight normal))))

   `(mu4e-warning-face
     ((,godot-class (:foreground ,godot-red
                                 :slant normal
                                 :weight bold))
      (,godot-256-class (:foreground ,godot-256-red
                                     :slant normal
                                     :weight bold))))

   `(mu4e-header-highlight-face
     ((,godot-class (:inherit unspecified
                              :foreground unspecified
                              :background ,godot-highlight-line
                              :underline ,godot-emphasis
                              :weight normal))
      (,godot-256-class (:inherit unspecified
                                  :foreground unspecified
                                  :background ,godot-256-highlight-line
                                  :underline ,godot-256-emphasis
                                  :weight normal))))


   `(mu4e-draft-face
     ((,godot-class (:inherit font-lock-string-face))
      (,godot-256-class (:inherit font-lock-string-face))))

   `(mu4e-footer-face
     ((,godot-class (:inherit font-lock-comment-face))
      (,godot-256-class (:inherit font-lock-comment-face))))

   `(mu4e-forwarded-face
     ((,godot-class (:inherit font-lock-builtin-face
                              :weight normal))
      (,godot-256-class (:inherit font-lock-builtin-face
                                  :weight normal))))

   `(mu4e-header-face
     ((,godot-class (:inherit default))
      (,godot-256-class (:inherit default))))

   `(mu4e-header-marks-face
     ((,godot-class (:inherit font-lock-preprocessor-face))
      (,godot-256-class (:inherit font-lock-preprocessor-face))))

   `(mu4e-header-title-face
     ((,godot-class (:inherit font-lock-type-face))
      (,godot-256-class (:inherit font-lock-type-face))))

   `(mu4e-highlight-face
     ((,godot-class (:inherit font-lock-pseudo-keyword-face
                              :weight bold))
      (,godot-256-class (:inherit font-lock-pseudo-keyword-face
                                  :weight bold))))

   `(mu4e-moved-face
     ((,godot-class (:inherit font-lock-comment-face
                              :slant italic))
      (,godot-256-class (:inherit font-lock-comment-face
                                  :slant italic))))

   `(mu4e-ok-face
     ((,godot-class (:inherit font-lock-comment-face
                              :slant normal
                              :weight bold))
      (,godot-256-class (:inherit font-lock-comment-face
                                  :slant normal
                                  :weight bold))))

   `(mu4e-replied-face
     ((,godot-class (:inherit font-lock-builtin-face
                              :weight normal))
      (,godot-256-class (:inherit font-lock-builtin-face
                                  :weight normal))))

   `(mu4e-system-face
     ((,godot-class (:inherit font-lock-comment-face
                              :slant italic))
      (,godot-256-class (:inherit font-lock-comment-face
                                  :slant italic))))

   `(mu4e-title-face
     ((,godot-class (:inherit font-lock-type-face
                              :weight bold))
      (,godot-256-class (:inherit font-lock-type-face
                                  :weight bold))))

   `(mu4e-trashed-face
     ((,godot-class (:inherit font-lock-comment-face
                              :strike-through t))
      (,godot-256-class (:inherit font-lock-comment-face
                                  :strike-through t))))

   `(mu4e-unread-face
     ((,godot-class (:inherit font-lock-keyword-face
                              :weight bold))
      (,godot-256-class (:inherit font-lock-keyword-face
                                  :weight bold))))

   `(mu4e-view-attach-number-face
     ((,godot-class (:inherit font-lock-variable-name-face
                              :weight bold))
      (,godot-256-class (:inherit font-lock-variable-name-face
                                  :weight bold))))

   `(mu4e-view-contact-face
     ((,godot-class (:foreground ,godot-foreground
                                 :weight normal))
      (,godot-256-class (:foreground ,godot-256-foreground
                                     :weight normal))))

   `(mu4e-view-header-key-face
     ((,godot-class (:inherit message-header-name
                              :weight normal))
      (,godot-256-class (:inherit message-header-name
                                  :weight normal))))

   `(mu4e-view-header-value-face
     ((,godot-class (:foreground ,godot-cyan
                                 :weight normal
                                 :slant normal))
      (,godot-256-class (:foreground ,godot-256-cyan
                                     :weight normal
                                     :slant normal))))

   `(mu4e-view-link-face
     ((,godot-class (:inherit link))
      (,godot-256-class (:inherit link))))

   `(mu4e-view-special-header-value-face
     ((,godot-class (:foreground ,godot-blue
                                 :weight normal
                                 :underline nil))
      (,godot-256-class (:foreground ,godot-256-blue
                                     :weight normal
                                     :underline nil))))

   ;; mumamo
   `(mumamo-background-chunk-submode1
     ((,godot-class (:background ,godot-highlight-line))
      (,godot-256-class (:background ,godot-256-highlight-line))))

   ;; nav
   `(nav-face-heading
     ((,godot-class (:foreground ,godot-yellow))
      (,godot-256-class (:foreground ,godot-256-yellow))))

   `(nav-face-button-num
     ((,godot-class (:foreground ,godot-cyan))
      (,godot-256-class (:foreground ,godot-256-cyan))))

   `(nav-face-dir
     ((,godot-class (:foreground ,godot-green))
      (,godot-256-class (:foreground ,godot-256-green))))

   `(nav-face-hdir
     ((,godot-class (:foreground ,godot-red))
      (,godot-256-class (:foreground ,godot-256-red))))

   `(nav-face-file
     ((,godot-class (:foreground ,godot-foreground))
      (,godot-256-class (:foreground ,godot-256-foreground))))

   `(nav-face-hfile
     ((,godot-class (:foreground ,godot-red))
      (,godot-256-class (:foreground ,godot-256-red))))

   ;; nav-flash
   `(nav-flash-face
     ((,godot-class (:background ,godot-highlight-line))
      (,godot-256-class (:background ,godot-256-highlight-line))))

   ;; neo-tree
   `(neo-banner-face
     ((,godot-class (:foreground ,godot-blue
                                 :background ,godot-background
                                 :weight bold))
      (,godot-256-class (:foreground ,godot-256-blue
                                     :background ,godot-256-background
                                     :weight bold))))


   `(neo-header-face
     ((,godot-class (:foreground ,godot-emphasis
                                 :background ,godot-background))
      (,godot-256-class (:foreground ,godot-256-emphasis
                                     :background ,godot-256-background))))

   `(neo-root-dir-face
     ((,godot-class (:foreground ,godot-green
                                 :background ,godot-background))
      (,godot-256-class (:foreground ,godot-256-green
                                     :background ,godot-256-background))))

   `(neo-dir-link-face
     ((,godot-class (:foreground ,godot-blue))
      (,godot-256-class (:foreground ,godot-256-blue
                                     :background ,godot-256-background))))

   `(neo-file-link-face
     ((,godot-class (:foreground ,godot-foreground))
      (,godot-256-class (:foreground ,godot-256-foreground))))

   `(neo-button-face
     ((,godot-class (:underline nil))
      (,godot-256-class (:underline nil))))

   `(neo-expand-btn-face
     ((,godot-class (:foreground ,godot-comments))
      (,godot-256-class (:foreground ,godot-256-comments))))

   `(neo-vc-default-face
     ((,godot-class (:foreground ,godot-foreground))
      (,godot-256-class (:foreground ,godot-256-foreground))))

   `(neo-vc-user-face
     ((,godot-class (:foreground ,godot-red
                                 :slant italic))
      (,godot-256-class (:foreground ,godot-256-red
                                     :slant italic))))

   `(neo-vc-up-to-date-face
     ((,godot-class (:foreground ,godot-comments))
      (,godot-256-class (:foreground ,godot-256-comments))))

   `(neo-vc-edited-face
     ((,godot-class (:foreground ,godot-orange))
      (,godot-256-class (:foreground ,godot-256-orange))))

   `(neo-vc-needs-update-face
     ((,godot-class (:underline t))
      (,godot-256-class (:underline t))))

   `(neo-vc-needs-merge-face
     ((,godot-class (:foreground ,godot-red))
      (,godot-256-class (:foreground ,godot-256-red))))

   `(neo-vc-unlocked-changes-face
     ((,godot-class (:foreground ,godot-red
                                 :background ,godot-comments))
      (,godot-256-class (:foreground ,godot-256-red
                                     :background ,godot-256-comments))))

   `(neo-vc-added-face
     ((,godot-class (:foreground ,godot-green))
      (,godot-256-class (:foreground ,godot-256-green))))

   `(neo-vc-removed-face
     ((,godot-class (:strike-through t))
      (,godot-256-class (:strike-through t))))

   `(neo-vc-conflict-face
     ((,godot-class (:foreground ,godot-red))
      (,godot-256-class (:foreground ,godot-256-red))))

   `(neo-vc-missing-face
     ((,godot-class (:foreground ,godot-red))
      (,godot-256-class (:foreground ,godot-256-red))))

   `(neo-vc-ignored-face
     ((,godot-class (:foreground ,godot-comments))
      (,godot-256-class (:foreground ,godot-256-comments))))

   ;; adoc-mode / markup
   `(markup-meta-face
     ((,godot-class (:foreground ,godot-gray-l))
      (,godot-256-class (:foreground ,godot-256-gray-l))))

   `(markup-table-face
     ((,godot-class (:foreground ,godot-blue-hc
                                 :background ,godot-blue-lc))
      (,godot-256-class (:foreground ,godot-256-blue-hc
                                     :background ,godot-256-blue-lc))))

   `(markup-verbatim-face
     ((,godot-class (:background ,godot-orange-lc))
      (,godot-256-class (:background ,godot-256-orange-lc))))

   `(markup-list-face
     ((,godot-class (:foreground ,godot-violet-hc
                                 :background ,godot-violet-lc))
      (,godot-256-class (:foreground ,godot-256-violet-hc
                                     :background ,godot-256-violet-lc))))

   `(markup-replacement-face
     ((,godot-class (:foreground ,godot-violet))
      (,godot-256-class (:foreground ,godot-256-violet))))

   `(markup-complex-replacement-face
     ((,godot-class (:foreground ,godot-violet-hc
                                 :background ,godot-violet-lc))
      (,godot-256-class (:foreground ,godot-256-violet-hc
                                     :background ,godot-256-violet-lc))))

   `(markup-gen-face
     ((,godot-class (:foreground ,godot-blue))
      (,godot-256-class (:foreground ,godot-256-blue))))

   `(markup-secondary-text-face
     ((,godot-class (:foreground ,godot-red))
      (,godot-256-class (:foreground ,godot-256-red))))

   ;; org-mode
   `(org-agenda-structure
     ((,godot-class (:foreground ,godot-emphasis
                                 :background ,godot-highlight-line
                                 :weight bold
                                 :slant normal
                                 :inverse-video nil
                                 :height ,godot-height-plus-1
                                 :underline nil
                                 :box (:line-width 2 :color ,godot-background)))
      (,godot-256-class (:foreground ,godot-256-emphasis
                                     :background ,godot-256-highlight-line
                                     :weight bold
                                     :slant normal
                                     :inverse-video nil
                                     :height ,godot-height-plus-1
                                     :underline nil
                                     :box (:line-width 2 :color ,godot-256-background)))))

   `(org-agenda-calendar-event
     ((,godot-class (:foreground ,godot-emphasis))
      (,godot-256-class (:foreground ,godot-256-emphasis))))

   `(org-agenda-calendar-sexp
     ((,godot-class (:foreground ,godot-foreground
                                 :slant italic))
      (,godot-256-class (:foreground ,godot-256-foreground
                                     :slant italic))))

   `(org-agenda-date
     ((,godot-class (:foreground ,godot-comments
                                 :background ,godot-background
                                 :weight normal
                                 :inverse-video nil
                                 :overline nil
                                 :slant normal
                                 :height 1.0
                                 :box (:line-width 2 :color ,godot-background)))
      (,godot-256-class (:foreground ,godot-256-comments
                                     :background ,godot-256-background
                                     :weight normal
                                     :inverse-video nil
                                     :overline nil
                                     :slant normal
                                     :height 1.0
                                     :box (:line-width 2 :color ,godot-256-background)))) t)

   `(org-agenda-date-weekend
     ((,godot-class (:inherit org-agenda-date
                              :inverse-video nil
                              :background unspecified
                              :foreground ,godot-comments
                              :weight unspecified
                              :underline t
                              :overline nil
                              :box unspecified))
      (,godot-256-class (:inherit org-agenda-date
                                  :inverse-video nil
                                  :background unspecified
                                  :foreground ,godot-256-comments
                                  :weight unspecified
                                  :underline t
                                  :overline nil
                                  :box unspecified))) t)

   `(org-agenda-date-today
     ((,godot-class (:inherit org-agenda-date
                              :inverse-video t
                              :weight bold
                              :underline unspecified
                              :overline nil
                              :box unspecified
                              :foreground ,godot-blue
                              :background ,godot-background))
      (,godot-256-class (:inherit org-agenda-date
                                  :inverse-video t
                                  :weight bold
                                  :underline unspecified
                                  :overline nil
                                  :box unspecified
                                  :foreground ,godot-256-blue
                                  :background ,godot-256-background))) t)

   `(org-agenda-done
     ((,godot-class (:foreground ,godot-comments
                                 :slant italic))
      (,godot-256-class (:foreground ,godot-256-comments
                                     :slant italic))) t)

   `(org-archived
     ((,godot-class (:foreground ,godot-comments
                                 :weight normal))
      (,godot-256-class (:foreground ,godot-256-comments
                                     :weight normal))))

   `(org-block
     ((,godot-class (:foreground ,godot-emphasis
                                 :background ,godot-highlight-alt))
      (,godot-256-class (:foreground ,godot-256-emphasis
                                     :background ,godot-256-highlight-alt))))

   `(org-block-background
     ((,godot-class (:background ,godot-highlight-alt))
      (,godot-256-class (:background ,godot-256-highlight-alt))))

   `(org-block-begin-line
     ((,godot-class (:foreground ,godot-comments
                                 :background ,godot-gray-d
                                 :slant italic))
      (,godot-256-class (:foreground ,godot-256-comments
                                     :background ,godot-256-gray-d
                                     :slant italic))))

   `(org-block-end-line
     ((,godot-class (:foreground ,godot-comments
                                 :background ,godot-gray-d
                                 :slant italic))
      (,godot-256-class (:foreground ,godot-256-comments
                                     :background ,godot-256-gray-d
                                     :slant italic))))

   `(org-checkbox
     ((,godot-class (:background ,godot-background
                                 :foreground ,godot-foreground
                                 :box (:line-width 1 :style released-button)))
      (,godot-256-class (:background ,godot-256-background
                                     :foreground ,godot-256-foreground
                                     :box (:line-width 1 :style released-button)))))

   `(org-code
     ((,godot-class (:foreground ,godot-comments))
      (,godot-256-class (:foreground ,godot-256-comments))))

   `(org-date
     ((,godot-class (:foreground ,godot-blue
                                 :underline t))
      (,godot-256-class (:foreground ,godot-256-blue
                                     :underline t))))

   `(org-done
     ((,godot-class (:weight bold
                             :foreground ,godot-green))
      (,godot-256-class (:weight bold
                                 :foreground ,godot-256-green))))

   `(org-ellipsis
     ((,godot-class (:foreground ,godot-comments))
      (,godot-256-class (:foreground ,godot-256-comments))))

   `(org-formula
     ((,godot-class (:foreground ,godot-yellow))
      (,godot-256-class (:foreground ,godot-256-yellow))))

   `(org-headline-done
     ((,godot-class (:foreground ,godot-green))
      (,godot-256-class (:foreground ,godot-256-green))))

   `(org-hide
     ((,godot-class (:foreground ,godot-background))
      (,godot-256-class (:foreground ,godot-256-background))))

   `(org-level-1
     ((,godot-class (:inherit ,godot-pitch
                              :height ,godot-height-plus-4
                              :foreground ,godot-orange))
      (,godot-256-class (:inherit ,godot-pitch
                                  :height ,godot-height-plus-4
                                  :foreground ,godot-256-orange))))

   `(org-level-2
     ((,godot-class (:inherit ,godot-pitch
                              :height ,godot-height-plus-3
                              :foreground ,godot-green))
      (,godot-256-class (:inherit ,godot-pitch
                                  :height ,godot-height-plus-3
                                  :foreground ,godot-256-green))))

   `(org-level-3
     ((,godot-class (:inherit ,godot-pitch
                              :height ,godot-height-plus-2
                              :foreground ,godot-blue))
      (,godot-256-class (:inherit ,godot-pitch
                                  :height ,godot-height-plus-2
                                  :foreground ,godot-256-blue))))

   `(org-level-4
     ((,godot-class (:inherit ,godot-pitch
                              :height ,godot-height-plus-1
                              :foreground ,godot-yellow))
      (,godot-256-class (:inherit ,godot-pitch
                                  :height ,godot-height-plus-1
                                  :foreground ,godot-256-yellow))))

   `(org-level-5
     ((,godot-class (:inherit ,godot-pitch
                              :foreground ,godot-cyan))
      (,godot-256-class (:inherit ,godot-pitch
                                  :foreground ,godot-256-cyan))))

   `(org-level-6
     ((,godot-class (:inherit ,godot-pitch
                              :foreground ,godot-green))
      (,godot-256-class (:inherit ,godot-pitch
                                  :foreground ,godot-256-green))))

   `(org-level-7
     ((,godot-class (:inherit ,godot-pitch
                              :foreground ,godot-red))
      (,godot-256-class (:inherit ,godot-pitch
                                  :foreground ,godot-256-red))))

   `(org-level-8
     ((,godot-class (:inherit ,godot-pitch
                              :foreground ,godot-blue))
      (,godot-256-class (:inherit ,godot-pitch
                                  :foreground ,godot-256-blue))))

   `(org-link
     ((,godot-class (:foreground ,godot-blue
                                 :underline t))
      (,godot-256-class (:foreground ,godot-256-blue
                                     :underline t))))

   `(org-sexp-date
     ((,godot-class (:foreground ,godot-violet))
      (,godot-256-class (:foreground ,godot-256-violet))))

   `(org-scheduled
     ((,godot-class (:foreground ,godot-green))
      (,godot-256-class (:foreground ,godot-256-green))))

   `(org-scheduled-previously
     ((,godot-class (:foreground ,godot-cyan))
      (,godot-256-class (:foreground ,godot-256-cyan))))

   `(org-scheduled-today
     ((,godot-class (:foreground ,godot-blue
                                 :weight normal))
      (,godot-256-class (:foreground ,godot-256-blue
                                     :weight normal))))

   `(org-special-keyword
     ((,godot-class (:foreground ,godot-comments
                                 :weight bold))
      (,godot-256-class (:foreground ,godot-256-comments
                                     :weight bold))))

   `(org-table
     ((,godot-class (:foreground ,godot-green))
      (,godot-256-class (:foreground ,godot-256-green))))

   `(org-tag
     ((,godot-class (:weight bold))
      (,godot-256-class (:weight bold))))

   `(org-time-grid
     ((,godot-class (:foreground ,godot-comments))
      (,godot-256-class (:foreground ,godot-256-comments))))

   `(org-todo
     ((,godot-class (:foreground ,godot-red
                                 :weight bold))
      (,godot-256-class (:foreground ,godot-256-red
                                     :weight bold))))

   `(org-upcoming-deadline
     ((,godot-class (:foreground ,godot-yellow
                                 :weight normal
                                 :underline nil))
      (,godot-256-class (:foreground ,godot-256-yellow
                                     :weight normal
                                     :underline nil))))

   `(org-warning
     ((,godot-class (:foreground ,godot-orange
                                 :weight normal
                                 :underline nil))
      (,godot-256-class (:foreground ,godot-256-orange
                                     :weight normal
                                     :underline nil))))

   ;; org-habit (clear=blue, ready=green, alert=yellow, overdue=red. future=lower contrast)
   `(org-habit-clear-face
     ((,godot-class (:background ,godot-blue-lc
                                 :foreground ,godot-blue-hc))
      (,godot-256-class (:background ,godot-256-blue-lc
                                     :foreground ,godot-256-blue-hc))))

   `(org-habit-clear-future-face
     ((,godot-class (:background ,godot-blue-lc))
      (,godot-256-class (:background ,godot-256-blue-lc))))

   `(org-habit-ready-face
     ((,godot-class (:background ,godot-green-lc
                                 :foreground ,godot-green))
      (,godot-256-class (:background ,godot-256-green-lc
                                     :foreground ,godot-256-green))))

   `(org-habit-ready-future-face
     ((,godot-class (:background ,godot-green-lc))
      (,godot-256-class (:background ,godot-256-green-lc))))

   `(org-habit-alert-face
     ((,godot-class (:background ,godot-yellow
                                 :foreground ,godot-yellow-lc))
      (,godot-256-class (:background ,godot-256-yellow
                                     :foreground ,godot-256-yellow-lc))))

   `(org-habit-alert-future-face
     ((,godot-class (:background ,godot-yellow-lc))
      (,godot-256-class (:background ,godot-256-yellow-lc))))

   `(org-habit-overdue-face
     ((,godot-class (:background ,godot-red
                                 :foreground ,godot-red-lc))
      (,godot-256-class (:background ,godot-256-red
                                     :foreground ,godot-256-red-lc))))

   `(org-habit-overdue-future-face
     ((,godot-class (:background ,godot-red-lc))
      (,godot-256-class (:background ,godot-256-red-lc))))

   ;; latest additions
   `(org-agenda-dimmed-todo-face
     ((,godot-class (:foreground ,godot-comments))
      (,godot-256-class (:foreground ,godot-256-comments))))

   `(org-agenda-restriction-lock
     ((,godot-class (:background ,godot-yellow))
      (,godot-256-class (:background ,godot-256-yellow))))

   `(org-clock-overlay
     ((,godot-class (:background ,godot-yellow))
      (,godot-256-class (:background ,godot-256-yellow))))

   `(org-column
     ((,godot-class (:background ,godot-highlight-line
                                 :strike-through nil
                                 :underline nil
                                 :slant normal
                                 :weight normal
                                 :inherit default))
      (,godot-256-class (:background ,godot-256-highlight-line
                                     :strike-through nil
                                     :underline nil
                                     :slant normal
                                     :weight normal
                                     :inherit default))))

   `(org-column-title
     ((,godot-class (:background ,godot-highlight-line
                                 :underline t
                                 :weight bold))
      (,godot-256-class (:background ,godot-256-highlight-line
                                     :underline t
                                     :weight bold))))

   `(org-date-selected
     ((,godot-class (:foreground ,godot-red
                                 :inverse-video t))
      (,godot-256-class (:foreground ,godot-256-red
                                     :inverse-video t))))

   `(org-document-info
     ((,godot-class (:foreground ,godot-foreground))
      (,godot-256-class (:foreground ,godot-256-foreground))))

   `(org-document-title
     ((,godot-class (:foreground ,godot-emphasis
                                 :weight bold
                                 :height ,godot-height-plus-4))
      (,godot-256-class (:foreground ,godot-256-emphasis
                                     :weight bold
                                     :height ,godot-height-plus-4))))

   `(org-drawer
     ((,godot-class (:foreground ,godot-cyan))
      (,godot-256-class (:foreground ,godot-256-cyan))))

   `(org-footnote
     ((,godot-class (:foreground ,godot-magenta
                                 :underline t))
      (,godot-256-class (:foreground ,godot-256-magenta
                                     :underline t))))

   `(org-latex-and-export-specials
     ((,godot-class (:foreground ,godot-orange))
      (,godot-256-class (:foreground ,godot-256-orange))))

   `(org-mode-line-clock-overrun
     ((,godot-class (:inherit mode-line))
      (,godot-256-class (:inherit mode-line))))

   ;; outline
   `(outline-1
     ((,godot-class (:inherit org-level-1))
      (,godot-256-class (:inherit org-level-1))))

   `(outline-2
     ((,godot-class (:inherit org-level-2))
      (,godot-256-class (:inherit org-level-2))))

   `(outline-3
     ((,godot-class (:inherit org-level-3))
      (,godot-256-class (:inherit org-level-3))))

   `(outline-4
     ((,godot-class (:inherit org-level-4))
      (,godot-256-class (:inherit org-level-4))))

   `(outline-5
     ((,godot-class (:inherit org-level-5))
      (,godot-256-class (:inherit org-level-5))))

   `(outline-6
     ((,godot-class (:inherit org-level-6))
      (,godot-256-class (:inherit org-level-6))))

   `(outline-7
     ((,godot-class (:inherit org-level-7))
      (,godot-256-class (:inherit org-level-7))))

   `(outline-8
     ((,godot-class (:inherit org-level-8))
      (,godot-256-class (:inherit org-level-8))))

   ;; parenface
   `(paren-face
     ((,godot-256-class (:foreground ,godot-comments))))

   ;; perspective
   `(persp-selected-face
     ((,godot-class (:foreground ,godot-blue
                                 :weight bold))))

   ;; pretty-mode
   `(pretty-mode-symbol-face
     ((,godot-class (:foreground ,godot-yellow
                                 :weight normal))
      (,godot-256-class (:foreground ,godot-256-yellow
                                     :weight normal))))

   ;; popup
   `(popup-face
     ((,godot-class (:background ,godot-highlight-line
                                 :foreground ,godot-foreground))
      (,godot-256-class (:background ,godot-256-highlight-line
                                     :foreground ,godot-256-foreground))))

   `(popup-isearch-match
     ((,godot-class (:background ,godot-green))
      (,godot-256-class (:background ,godot-256-green))))

   `(popup-menu-face
     ((,godot-class (:background ,godot-highlight-line
                                 :foreground ,godot-foreground))
      (,godot-256-class (:background ,godot-256-highlight-line
                                     :foreground ,godot-256-foreground))))

   `(popup-menu-mouse-face
     ((,godot-class (:background ,godot-blue
                                 :foreground ,godot-foreground))
      (,godot-256-class (:background ,godot-256-blue
                                     :foreground ,godot-256-foreground))))

   `(popup-menu-selection-face
     ((,godot-class (:background ,godot-magenta
                                 :foreground ,godot-background))
      (,godot-256-class (:background ,godot-256-magenta
                                     :foreground ,godot-256-background))))

   `(popup-scroll-bar-background-face
     ((,godot-class (:background ,godot-comments))
      (,godot-256-class (:background ,godot-256-comments))))

   `(popup-scroll-bar-foreground-face
     ((,godot-class (:background ,godot-emphasis))
      (,godot-256-class (:background ,godot-256-emphasis))))

   `(popup-tip-face
     ((,godot-class (:background ,godot-highlight-line
                                 :foreground ,godot-foreground))
      (,godot-256-class (:background ,godot-256-highlight-line
                                     :foreground ,godot-256-foreground))))

   ;; rainbow-delimiters
   `(rainbow-delimiters-depth-1-face
     ((,godot-class (:foreground ,godot-violet))
      (,godot-256-class (:foreground ,godot-256-violet))))

   `(rainbow-delimiters-depth-2-face
     ((,godot-class (:foreground ,godot-blue))
      (,godot-256-class (:foreground ,godot-256-blue))))

   `(rainbow-delimiters-depth-3-face
     ((,godot-class (:foreground ,godot-green))
      (,godot-256-class (:foreground ,godot-256-green))))

   `(rainbow-delimiters-depth-4-face
     ((,godot-class (:foreground ,godot-yellow))
      (,godot-256-class (:foreground ,godot-256-yellow))))

   `(rainbow-delimiters-depth-5-face
     ((,godot-class (:foreground ,godot-orange))
      (,godot-256-class (:foreground ,godot-256-orange))))

   `(rainbow-delimiters-depth-6-face
     ((,godot-class (:foreground ,godot-red))
      (,godot-256-class (:foreground ,godot-256-red))))

   `(rainbow-delimiters-depth-7-face
     ((,godot-class (:foreground ,godot-violet))
      (,godot-256-class (:foreground ,godot-256-violet))))

   `(rainbow-delimiters-depth-8-face
     ((,godot-class (:foreground ,godot-blue))
      (,godot-256-class (:foreground ,godot-256-blue))))

   `(rainbow-delimiters-depth-9-face
     ((,godot-class (:foreground ,godot-green))
      (,godot-256-class (:foreground ,godot-256-green))))

   `(rainbow-delimiters-depth-10-face
     ((,godot-class (:foreground ,godot-yellow))
      (,godot-256-class (:foreground ,godot-256-yellow))))

   `(rainbow-delimiters-depth-11-face
     ((,godot-class (:foreground ,godot-orange))
      (,godot-256-class (:foreground ,godot-256-orange))))

   `(rainbow-delimiters-depth-12-face
     ((,godot-class (:foreground ,godot-red))
      (,godot-256-class (:foreground ,godot-256-red))))

   `(rainbow-delimiters-unmatched-face
     ((,godot-class (:foreground ,godot-foreground
                                 :background ,godot-background
                                 :inverse-video t))
      (,godot-256-class (:foreground ,godot-256-foreground
                                     :background ,godot-256-background
                                     :inverse-video t))))

   ;; realgud
   `(realgud-overlay-arrow1
     ((,godot-class (:foreground ,godot-green-d))
      (,godot-256-class (:foreground ,godot-256-green-d))))

   `(realgud-overlay-arrow2
     ((,godot-class (:foreground ,godot-yellow-d))
      (,godot-256-class (:foreground ,godot-256-yellow-d))))

   `(realgud-overlay-arrow3
     ((,godot-class (:foreground ,godot-orange-d))
      (,godot-256-class (:foreground ,godot-256-orange-d))))

   `(realgud-bp-enabled-face
     ((,godot-class (:inherit error))
      (,godot-256-class (:inherit error))))

   `(realgud-bp-disabled-face
     ((,godot-class (:inherit secondary-selection))
      (,godot-256-class (:inherit secondary-selection))))

   `(realgud-bp-line-enabled-face
     ((,godot-class (:foreground ,godot-red-d))
      (,godot-256-class (:foreground ,godot-256-red-d))))

   `(realgud-bp-line-disabled-face
     ((,godot-class (:inherit secondary-selection))
      (,godot-256-class (:inherit secondary-selection))))

   `(realgud-line-number
     ((,godot-class (:inerhit godot-line-number))
      (,godot-256-class (:inerhit godot-line-number))))

   `(realgud-backtrace-number
     ((,godot-class (:foreground ,godot-yellow-d
                                 :weight bold))
      (,godot-256-class (:foreground ,godot-256-yellow
                                     :weight bold))))

   ;; rhtm-mode
   `(erb-face
     ((,godot-class (:foreground ,godot-emphasis
                                 :background ,godot-background))
      (,godot-256-class (:foreground ,godot-256-emphasis
                                     :background ,godot-256-background))))

   `(erb-delim-face
     ((,godot-class (:foreground ,godot-cyan
                                 :background ,godot-background))
      (,godot-256-class (:foreground ,godot-256-cyan
                                     :background ,godot-256-background))))

   `(erb-exec-face
     ((,godot-class (:foreground ,godot-emphasis
                                 :background ,godot-background))
      (,godot-256-class (:foreground ,godot-256-emphasis
                                     :background ,godot-256-background))))

   `(erb-exec-delim-face
     ((,godot-class (:foreground ,godot-cyan
                                 :background ,godot-background))
      (,godot-256-class (:foreground ,godot-256-cyan
                                     :background ,godot-256-background))))

   `(erb-out-face
     ((,godot-class (:foreground ,godot-emphasis
                                 :background ,godot-background))
      (,godot-256-class (:foreground ,godot-256-emphasis
                                     :background ,godot-256-background))))

   `(erb-out-delim-face
     ((,godot-class (:foreground ,godot-cyan
                                 :background ,godot-background))
      (,godot-256-class (:foreground ,godot-256-cyan
                                     :background ,godot-256-background))))

   `(erb-comment-face
     ((,godot-class (:foreground ,godot-emphasis
                                 :background ,godot-background))
      (,godot-256-class (:foreground ,godot-256-emphasis
                                     :background ,godot-256-background))))

   `(erb-comment-delim-face
     ((,godot-class (:foreground ,godot-cyan
                                 :background ,godot-background))
      (,godot-256-class (:foreground ,godot-256-cyan
                                     :background ,godot-256-background))))

   ;; rst-mode
   `(rst-level-1-face
     ((,godot-class (:background ,godot-yellow
                                 :foreground ,godot-background))
      (,godot-256-class (:background ,godot-256-yellow
                                     :foreground ,godot-256-background))))

   `(rst-level-2-face
     ((,godot-class (:background ,godot-cyan
                                 :foreground ,godot-background))
      (,godot-256-class (:background ,godot-256-cyan
                                     :foreground ,godot-256-background))))

   `(rst-level-3-face
     ((,godot-class (:background ,godot-blue
                                 :foreground ,godot-background))
      (,godot-256-class (:background ,godot-256-blue
                                     :foreground ,godot-256-background))))

   `(rst-level-4-face
     ((,godot-class (:background ,godot-violet
                                 :foreground ,godot-background))
      (,godot-256-class (:background ,godot-256-violet
                                     :foreground ,godot-256-background))))

   `(rst-level-5-face
     ((,godot-class (:background ,godot-magenta
                                 :foreground ,godot-background))
      (,godot-256-class (:background ,godot-256-magenta
                                     :foreground ,godot-256-background))))

   `(rst-level-6-face
     ((,godot-class (:background ,godot-red
                                 :foreground ,godot-background))
      (,godot-256-class (:background ,godot-256-red
                                     :foreground ,godot-256-background))))

   ;; rpm-mode
   `(rpm-spec-dir-face
     ((,godot-class (:foreground ,godot-green))
      (,godot-256-class (:foreground ,godot-256-green))))

   `(rpm-spec-doc-face
     ((,godot-class (:foreground ,godot-green))
      (,godot-256-class (:foreground ,godot-256-green))))

   `(rpm-spec-ghost-face
     ((,godot-class (:foreground ,godot-red))
      (,godot-256-class (:foreground ,godot-256-red))))

   `(rpm-spec-macro-face
     ((,godot-class (:foreground ,godot-yellow))
      (,godot-256-class (:foreground ,godot-256-yellow))))

   `(rpm-spec-obsolete-tag-face
     ((,godot-class (:foreground ,godot-red))
      (,godot-256-class (:foreground ,godot-256-red))))

   `(rpm-spec-package-face
     ((,godot-class (:foreground ,godot-red))
      (,godot-256-class (:foreground ,godot-256-red))))

   `(rpm-spec-section-face
     ((,godot-class (:foreground ,godot-yellow))
      (,godot-256-class (:foreground ,godot-256-yellow))))

   `(rpm-spec-tag-face
     ((,godot-class (:foreground ,godot-blue))
      (,godot-256-class (:foreground ,godot-256-blue))))

   `(rpm-spec-var-face
     ((,godot-class (:foreground ,godot-red))
      (,godot-256-class (:foreground ,godot-256-red))))

   ;; sh-mode
   `(sh-quoted-exec
     ((,godot-class (:foreground ,godot-violet
                                 :weight bold))
      (,godot-256-class (:foreground ,godot-256-violet
                                     :weight bold))))

   `(sh-escaped-newline
     ((,godot-class (:foreground ,godot-yellow
                                 :weight bold))
      (,godot-256-class (:foreground ,godot-256-yellow
                                     :weight bold))))

   `(sh-heredoc
     ((,godot-class (:foreground ,godot-yellow
                                 :weight bold))
      (,godot-256-class (:foreground ,godot-256-yellow
                                     :weight bold))))

   ;; smartparens
   `(sp-pair-overlay-face
     ((,godot-class (:background ,godot-highlight-line))
      (,godot-256-class (:background ,godot-256-highlight-line))))

   `(sp-wrap-overlay-face
     ((,godot-class (:background ,godot-highlight-line))
      (,godot-256-class (:background ,godot-256-highlight-line))))

   `(sp-wrap-tag-overlay-face
     ((,godot-class (:background ,godot-highlight-line))
      (,godot-256-class (:background ,godot-256-highlight-line))))

   `(sp-show-pair-enclosing
     ((,godot-class (:inherit highlight))
      (,godot-256-class (:inherit highlight))))

   `(sp-show-pair-match-face
     ((,godot-class (:foreground ,godot-green
                                 :background ,godot-background
                                 :weight normal
                                 :inverse-video t))
      (,godot-256-class (:foreground ,godot-256-green
                                     :background ,godot-256-background
                                     :weight normal
                                     :inverse-video t))))

   `(sp-show-pair-mismatch-face
     ((,godot-class (:foreground ,godot-red
                                 :background ,godot-background
                                 :weight normal
                                 :inverse-video t))
      (,godot-256-class (:foreground ,godot-256-red
                                     :background ,godot-256-background
                                     :weight normal
                                     :inverse-video t))))

   ;; show-paren
   `(show-paren-match
     ((,godot-class (:foreground ,godot-green
                                 :background ,godot-background
                                 :weight normal
                                 :inverse-video t))
      (,godot-256-class (:foreground ,godot-256-green
                                     :background ,godot-256-background
                                     :weight normal
                                     :inverse-video t))))

   `(show-paren-mismatch
     ((,godot-class (:foreground ,godot-red
                                 :background ,godot-background
                                 :weight normal
                                 :inverse-video t))
      (,godot-256-class (:foreground ,godot-256-red
                                     :background ,godot-256-background
                                     :weight normal
                                     :inverse-video t))))

   ;; mic-paren
   `(paren-face-match
     ((,godot-class (:foreground ,godot-green
                                 :background ,godot-background
                                 :weight normal
                                 :inverse-video t))
      (,godot-256-class (:foreground ,godot-256-green
                                     :background ,godot-256-background
                                     :weight normal
                                     :inverse-video t))))

   `(paren-face-mismatch
     ((,godot-class (:foreground ,godot-red
                                 :background ,godot-background
                                 :weight normal
                                 :inverse-video t))
      (,godot-256-class (:foreground ,godot-256-red
                                     :background ,godot-256-background
                                     :weight normal
                                     :inverse-video t))))

   `(paren-face-no-match
     ((,godot-class (:foreground ,godot-red
                                 :background ,godot-background
                                 :weight normal
                                 :inverse-video t))
      (,godot-256-class (:foreground ,godot-256-red
                                     :background ,godot-256-background
                                     :weight normal
                                     :inverse-video t))))

   ;; SLIME
   `(slime-repl-inputed-output-face
     ((,godot-class (:foreground ,godot-red))
      (,godot-256-class (:foreground ,godot-256-red))))

   ;; smerge
   `(smerge-base
     ((,godot-class (:background ,godot-diff-blue-base))
      (,godot-256-class (:background ,godot-256-diff-blue-base))))
   `(smerge-upper
     ((,godot-class (:background ,godot-diff-red-base))
      (,godot-256-class (:background ,godot-256-diff-red-base))))
   `(smerge-lower
     ((,godot-class (:background ,godot-diff-green-base))
      (,godot-256-class (:background ,godot-256-diff-green-base))))
   ;; WARNING: defining this face will overwrite the next two when displaying a
   ;; smerge diff in a file.
   ;; `(smerge-refined-changed
   ;;    ((,godot-class (:background ,godot-diff-blue-emphasis))
   ;;      (,godot-256-class (:background ,godot-256-diff-blue-emphasis))))
   `(smerge-refined-added
     ((,godot-class (:background ,godot-diff-green-emphasis))
      (,godot-256-class (:background ,godot-256-diff-green-emphasis))))
   `(smerge-refined-removed
     ((,godot-class (:background ,godot-diff-red-emphasis))
      (,godot-256-class (:background ,godot-256-diff-red-emphasis))))

   ;; speedbar
   `(speedbar-button-face
     ((,godot-class (:inherit ,godot-pitch
                              :foreground ,godot-comments))
      (,godot-256-class (:inherit ,godot-pitch
                                  :foreground ,godot-256-comments))))

   `(speedbar-directory-face
     ((,godot-class (:inherit ,godot-pitch
                              :foreground ,godot-blue))
      (,godot-256-class (:inherit ,godot-pitch
                                  :foreground ,godot-256-blue))))

   `(speedbar-file-face
     ((,godot-class (:inherit ,godot-pitch
                              :foreground ,godot-foreground))
      (,godot-256-class (:inherit ,godot-pitch
                                  :foreground ,godot-256-foreground))))

   `(speedbar-highlight-face
     ((,godot-class (:inherit ,godot-pitch
                              :background ,godot-highlight-line))
      (,godot-256-class (:inherit ,godot-pitch
                                  :background ,godot-256-highlight-line))))

   `(speedbar-selected-face
     ((,godot-class (:inherit ,godot-pitch
                              :foreground ,godot-yellow
                              :underline t))
      (,godot-256-class (:inherit ,godot-pitch
                                  :foreground ,godot-256-yellow
                                  :underline t))))

   `(speedbar-separator-face
     ((,godot-class (:inherit ,godot-pitch
                              :background ,godot-blue
                              :foreground ,godot-background
                              :overline ,godot-cyan-lc))
      (,godot-256-class (:inherit ,godot-pitch
                                  :background ,godot-256-blue
                                  :foreground ,godot-256-background
                                  :overline ,godot-256-cyan-lc))))

   `(speedbar-tag-face
     ((,godot-class (:inherit ,godot-pitch
                              :foreground ,godot-green))
      (,godot-256-class (:inherit ,godot-pitch
                                  :foreground ,godot-256-green))))

   ;; sunrise commander headings
   `(sr-active-path-face
     ((,godot-class (:background ,godot-blue
                                 :foreground ,godot-background
                                 :height ,godot-height-plus-1
                                 :weight bold))
      (,godot-256-class (:background ,godot-256-blue
                                     :foreground ,godot-256-background
                                     :height ,godot-height-plus-1
                                     :weight bold))))

   `(sr-editing-path-face
     ((,godot-class (:background ,godot-yellow
                                 :foreground ,godot-background
                                 :weight bold
                                 :height ,godot-height-plus-1))
      (,godot-256-class (:background ,godot-256-yellow
                                     :foreground ,godot-256-background
                                     :weight bold
                                     :height ,godot-height-plus-1))))

   `(sr-highlight-path-face
     ((,godot-class (:background ,godot-green
                                 :foreground ,godot-background
                                 :weight bold
                                 :height ,godot-height-plus-1))
      (,godot-256-class (:background ,godot-256-green
                                     :foreground ,godot-256-background
                                     :weight bold
                                     :height ,godot-height-plus-1))))

   `(sr-passive-path-face
     ((,godot-class (:background ,godot-comments
                                 :foreground ,godot-background
                                 :weight bold
                                 :height ,godot-height-plus-1))
      (,godot-256-class (:background ,godot-256-comments
                                     :foreground ,godot-256-background
                                     :weight bold
                                     :height ,godot-height-plus-1))))

   ;; sunrise commander marked
   `(sr-marked-dir-face
     ((,godot-class (:inherit digodot-red-marked))
      (,godot-256-class (:inherit digodot-red-marked))))

   `(sr-marked-file-face
     ((,godot-class (:inherit digodot-red-marked))
      (,godot-256-class (:inherit digodot-red-marked))))

   `(sr-alt-marked-dir-face
     ((,godot-class (:background ,godot-magenta
                                 :foreground ,godot-background
                                 :weight bold))
      (,godot-256-class (:background ,godot-256-magenta
                                     :foreground ,godot-256-background
                                     :weight bold))))

   `(sr-alt-marked-file-face
     ((,godot-class (:background ,godot-magenta
                                 :foreground ,godot-background
                                 :weight bold))
      (,godot-256-class (:background ,godot-256-magenta
                                     :foreground ,godot-256-background
                                     :weight bold))))

   ;; sunrise commander fstat
   `(sr-directory-face
     ((,godot-class (:inherit digodot-red-directory
                              :weight normal))
      (,godot-256-class (:inherit digodot-red-directory
                                  :weight normal))))

   `(sr-symlink-directory-face
     ((,godot-class (:inherit digodot-red-directory
                              :slant italic
                              :weight normal))
      (,godot-256-class (:inherit digodot-red-directory
                                  :slant italic
                                  :weight normal))))

   `(sr-symlink-face
     ((,godot-class (:inherit digodot-red-symlink
                              :slant italic
                              :weight normal))
      (,godot-256-class (:inherit digodot-red-symlink
                                  :slant italic
                                  :weight normal))))

   `(sr-broken-link-face
     ((,godot-class (:inherit digodot-red-warning
                              :slant italic
                              :weight normal))
      (,godot-256-class (:inherit digodot-red-warning
                                  :slant italic
                                  :weight normal))))

   ;; sunrise commander file types
   `(sr-compressed-face
     ((,godot-class (:foreground ,godot-foreground))
      (,godot-256-class (:foreground ,godot-256-foreground))))

   `(sr-encrypted-face
     ((,godot-class (:foreground ,godot-foreground))
      (,godot-256-class (:foreground ,godot-256-foreground))))

   `(sr-log-face
     ((,godot-class (:foreground ,godot-foreground))
      (,godot-256-class (:foreground ,godot-256-foreground))))

   `(sr-packaged-face
     ((,godot-class (:foreground ,godot-foreground))
      (,godot-256-class (:foreground ,godot-256-foreground))))

   `(sr-html-face
     ((,godot-class (:foreground ,godot-foreground))
      (,godot-256-class (:foreground ,godot-256-foreground))))

   `(sr-xml-face
     ((,godot-class (:foreground ,godot-foreground))
      (,godot-256-class (:foreground ,godot-256-foreground))))

   ;; sunrise commander misc
   `(sr-clex-hotchar-face
     ((,godot-class (:background ,godot-red
                                 :foreground ,godot-background
                                 :weight bold))
      (,godot-256-class (:background ,godot-256-red
                                     :foreground ,godot-256-background
                                     :weight bold))))

   ;; syslog-mode
   `(syslog-ip-face
     ((,godot-class (:background unspecified
                                 :foreground ,godot-yellow))
      (,godot-256-class (:background unspecified
                                     :foreground ,godot-256-yellow))))

   `(syslog-hour-face
     ((,godot-class (:background unspecified
                                 :foreground ,godot-green))
      (,godot-256-class (:background unspecified
                                     :foreground ,godot-256-green))))

   `(syslog-error-face
     ((,godot-class (:background unspecified
                                 :foreground ,godot-red
                                 :weight bold))
      (,godot-256-class (:background unspecified
                                     :foreground ,godot-256-red
                                     :weight bold))))

   `(syslog-warn-face
     ((,godot-class (:background unspecified
                                 :foreground ,godot-orange
                                 :weight bold))
      (,godot-256-class (:background unspecified
                                     :foreground ,godot-256-orange
                                     :weight bold))))

   `(syslog-info-face
     ((,godot-class (:background unspecified
                                 :foreground ,godot-blue
                                 :weight bold))
      (,godot-256-class (:background unspecified
                                     :foreground ,godot-256-blue
                                     :weight bold))))

   `(syslog-debug-face
     ((,godot-class (:background unspecified
                                 :foreground ,godot-cyan
                                 :weight bold))
      (,godot-256-class (:background unspecified
                                     :foreground ,godot-256-cyan
                                     :weight bold))))

   `(syslog-su-face
     ((,godot-class (:background unspecified
                                 :foreground ,godot-magenta))
      (,godot-256-class (:background unspecified
                                     :foreground ,godot-256-magenta))))

   ;; table
   `(table-cell
     ((,godot-class (:foreground ,godot-foreground
                                 :background ,godot-highlight-line))
      (,godot-256-class (:foreground ,godot-256-foreground
                                     :background ,godot-256-highlight-line))))

   ;; term
   `(term-color-black
     ((,godot-class (:foreground ,godot-background
                                 :background ,godot-highlight-line))
      (,godot-256-class (:foreground ,godot-256-background
                                     :background ,godot-256-highlight-line))))

   `(term-color-red
     ((,godot-class (:foreground ,godot-red
                                 :background ,godot-red-d))
      (,godot-256-class (:foreground ,godot-256-red
                                     :background ,godot-256-red-d))))

   `(term-color-green
     ((,godot-class (:foreground ,godot-green
                                 :background ,godot-green-d))
      (,godot-256-class (:foreground ,godot-256-green
                                     :background ,godot-256-green-d))))

   `(term-color-yellow
     ((,godot-class (:foreground ,godot-yellow
                                 :background ,godot-yellow-d))
      (,godot-256-class (:foreground ,godot-256-yellow
                                     :background ,godot-256-yellow-d))))

   `(term-color-blue
     ((,godot-class (:foreground ,godot-blue
                                 :background ,godot-blue-d))
      (,godot-256-class (:foreground ,godot-256-blue
                                     :background ,godot-256-blue-d))))

   `(term-color-magenta
     ((,godot-class (:foreground ,godot-magenta
                                 :background ,godot-magenta-d))
      (,godot-256-class (:foreground ,godot-256-magenta
                                     :background ,godot-256-magenta-d))))

   `(term-color-cyan
     ((,godot-class (:foreground ,godot-cyan
                                 :background ,godot-cyan-d))
      (,godot-256-class (:foreground ,godot-256-cyan
                                     :background ,godot-256-cyan-d))))

   `(term-color-white
     ((,godot-class (:foreground ,godot-emphasis
                                 :background ,godot-foreground))
      (,godot-256-class (:foreground ,godot-256-emphasis
                                     :background ,godot-256-foreground))))

   `(term-default-fg-color
     ((,godot-class (:inherit term-color-white))
      (,godot-256-class (:inherit term-color-white))))

   `(term-default-bg-color
     ((,godot-class (:inherit term-color-black))
      (,godot-256-class (:inherit term-color-black))))

   ;; tooltip. (NOTE: This setting has no effect on the os widgets for me
   ;; zencoding uses this)
   `(tooltip
     ((,godot-class (:background ,godot-yellow-hc
                                 :foreground ,godot-background
                                 :inherit ,godot-pitch))))

   ;; treemacs
   `(treemacs-directory-face
     ((,godot-class (:foreground ,godot-violet
                                 :background ,godot-background
                                 :weight bold))
      (,godot-256-class (:foreground ,godot-256-violet
                                     :background ,godot-256-background
                                     :weight bold))))

   `(treemacs-header-face
     ((,godot-class (:foreground ,godot-yellow
                                 :background ,godot-background
                                 :underline t
                                 :weight bold))
      (,godot-256-class (:foreground ,godot-256-yellow
                                     :background ,godot-256-background
                                     :underline t
                                     :weight bold))))

   `(treemacs-git-modified-face
     ((,godot-class (:foreground ,godot-green
                                 :background ,godot-background))
      (,godot-256-class (:foreground ,godot-256-green
                                     :background ,godot-256-background))))

   `(treemacs-git-renamed-face
     ((,godot-class (:foreground ,godot-red
                                 :background ,godot-background))
      (,godot-256-class (:foreground ,godot-256-red
                                     :background ,godot-256-background))))

   `(treemacs-git-ignored-face
     ((,godot-class (:foreground ,godot-gray-l
                                 :background ,godot-background))
      (,godot-256-class (:foreground ,godot-256-gray-l
                                     :background ,godot-256-background))))

   `(treemacs-git-untracked-face
     ((,godot-class (:foreground ,godot-red
                                 :background ,godot-background))
      (,godot-256-class (:foreground ,godot-256-red
                                     :background ,godot-256-background))))

   `(treemacs-git-added-face
     ((,godot-class (:foreground ,godot-green
                                 :background ,godot-background))
      (,godot-256-class (:foreground ,godot-256-green
                                     :background ,godot-256-background))))

   `(treemacs-git-conflict-face
     ((,godot-class (:foreground ,godot-orange
                                 :background ,godot-background))
      (,godot-256-class (:foreground ,godot-256-orange
                                     :background ,godot-256-background))))

   ;; tuareg
   `(tuareg-font-lock-governing-face
     ((,godot-class (:foreground ,godot-magenta
                                 :weight bold))
      (,godot-256-class (:foreground ,godot-256-magenta
                                     :weight bold))))

   `(tuareg-font-lock-multistage-face
     ((,godot-class (:foreground ,godot-blue
                                 :background ,godot-highlight-line
                                 :weight bold))
      (,godot-256-class (:foreground ,godot-256-blue
                                     :background ,godot-256-highlight-line
                                     :weight bold))))

   `(tuareg-font-lock-operator-face
     ((,godot-class (:foreground ,godot-emphasis))
      (,godot-256-class (:foreground ,godot-256-emphasis))))

   `(tuareg-font-lock-error-face
     ((,godot-class (:foreground ,godot-yellow
                                 :background ,godot-red
                                 :weight bold))
      (,godot-256-class (:foreground ,godot-256-yellow
                                     :background ,godot-256-red
                                     :weight bold))))

   `(tuareg-font-lock-interactive-output-face
     ((,godot-class (:foreground ,godot-cyan))
      (,godot-256-class (:foreground ,godot-256-cyan))))

   `(tuareg-font-lock-interactive-error-face
     ((,godot-class (:foreground ,godot-red))
      (,godot-256-class (:foreground ,godot-256-red))))

   ;; undo-tree
   `(undo-tree-visualizer-default-face
     ((,godot-class (:foreground ,godot-comments
                                 :background ,godot-background))
      (,godot-256-class (:foreground ,godot-256-comments
                                     :background ,godot-256-background))))

   `(undo-tree-visualizer-unmodified-face
     ((,godot-class (:foreground ,godot-green))
      (,godot-256-class (:foreground ,godot-256-green))))

   `(undo-tree-visualizer-current-face
     ((,godot-class (:foreground ,godot-blue
                                 :inverse-video t))
      (,godot-256-class (:foreground ,godot-256-blue
                                     :inverse-video t))))

   `(undo-tree-visualizer-active-branch-face
     ((,godot-class (:foreground ,godot-emphasis
                                 :background ,godot-background
                                 :weight bold))
      (,godot-256-class (:foreground ,godot-256-emphasis
                                     :background ,godot-256-background
                                     :weight bold))))

   `(undo-tree-visualizer-register-face
     ((,godot-class (:foreground ,godot-yellow))
      (,godot-256-class (:foreground ,godot-256-yellow))))

   ;; volatile highlights
   `(vhl/default-face
     ((,godot-class (:background ,godot-highlight-alt))
      (,godot-256-class (:background ,godot-256-highlight-alt))))

   ;; w3m
   `(w3m-anchor
     ((,godot-class (:inherit link))
      (,godot-256-class (:inherit link))))

   `(w3m-arrived-anchor
     ((,godot-class (:inherit link-visited))
      (,godot-256-class (:inherit link-visited))))

   `(w3m-form
     ((,godot-class (:background ,godot-background
                                 :foreground ,godot-foreground))
      (,godot-256-class (:background ,godot-256-background
                                     :foreground ,godot-256-foreground))))

   `(w3m-header-line-location-title
     ((,godot-class (:background ,godot-highlight-line
                                 :foreground ,godot-yellow))
      (,godot-256-class (:background ,godot-256-highlight-line
                                     :foreground ,godot-256-yellow))))

   `(w3m-header-line-location-content

     ((,godot-class (:background ,godot-highlight-line
                                 :foreground ,godot-foreground))
      (,godot-256-class (:background ,godot-256-highlight-line
                                     :foreground ,godot-256-foreground))))

   `(w3m-bold
     ((,godot-class (:foreground ,godot-emphasis
                                 :weight bold))
      (,godot-256-class (:foreground ,godot-256-emphasis
                                     :weight bold))))

   `(w3m-image-anchor
     ((,godot-class (:background ,godot-background
                                 :foreground ,godot-cyan
                                 :inherit link))
      (,godot-256-class (:background ,godot-256-background
                                     :foreground ,godot-256-cyan
                                     :inherit link))))

   `(w3m-image
     ((,godot-class (:background ,godot-background
                                 :foreground ,godot-cyan))
      (,godot-256-class (:background ,godot-256-background
                                     :foreground ,godot-256-cyan))))

   `(w3m-lnum-minibuffer-prompt
     ((,godot-class (:foreground ,godot-emphasis))
      (,godot-256-class (:foreground ,godot-256-emphasis))))

   `(w3m-lnum-match
     ((,godot-class (:background ,godot-highlight-line))
      (,godot-256-class (:background ,godot-256-highlight-line))))

   `(w3m-lnum
     ((,godot-class (:underline nil
                                :bold nil
                                :foreground ,godot-red))
      (,godot-256-class (:underline nil
                                    :bold nil
                                    :foreground ,godot-256-red))))

   `(w3m-session-select
     ((,godot-class (:foreground ,godot-foreground))
      (,godot-256-class (:foreground ,godot-256-foreground))))

   `(w3m-session-selected
     ((,godot-class (:foreground ,godot-emphasis
                                 :bold t
                                 :underline t))
      (,godot-256-class (:foreground ,godot-256-emphasis
                                     :bold t
                                     :underline t))))

   `(w3m-tab-background
     ((,godot-class (:background ,godot-background
                                 :foreground ,godot-foreground))
      (,godot-256-class (:background ,godot-256-background
                                     :foreground ,godot-256-foreground))))

   `(w3m-tab-selected-background
     ((,godot-class (:background ,godot-background
                                 :foreground ,godot-foreground))
      (,godot-256-class (:background ,godot-256-background
                                     :foreground ,godot-256-foreground))))

   `(w3m-tab-mouse
     ((,godot-class (:background ,godot-highlight-line
                                 :foreground ,godot-yellow))
      (,godot-256-class (:background ,godot-256-highlight-line
                                     :foreground ,godot-256-yellow))))

   `(w3m-tab-selected
     ((,godot-class (:background ,godot-highlight-line
                                 :foreground ,godot-emphasis
                                 :bold t))
      (,godot-256-class (:background ,godot-256-highlight-line
                                     :foreground ,godot-256-emphasis
                                     :bold t))))

   `(w3m-tab-unselected
     ((,godot-class (:background ,godot-highlight-line
                                 :foreground ,godot-foreground))
      (,godot-256-class (:background ,godot-256-highlight-line
                                     :foreground ,godot-256-foreground))))

   `(w3m-tab-selected-retrieving
     ((,godot-class (:background ,godot-highlight-line
                                 :foreground ,godot-red))
      (,godot-256-class (:background ,godot-256-highlight-line
                                     :foreground ,godot-256-red))))

   `(w3m-tab-unselected-retrieving
     ((,godot-class (:background ,godot-highlight-line
                                 :foreground ,godot-orange))
      (,godot-256-class (:background ,godot-256-highlight-line
                                     :foreground ,godot-256-orange))))

   `(w3m-tab-unselected-unseen
     ((,godot-class (:background ,godot-highlight-line
                                 :foreground ,godot-violet))
      (,godot-256-class (:background ,godot-256-highlight-line
                                     :foreground ,godot-256-violet))))

   ;; web-mode
   `(web-mode-builtin-face
     ((,godot-class (:foreground ,godot-red))
      (,godot-256-class (:foreground ,godot-256-red))))

   `(web-mode-comment-face
     ((,godot-class (:foreground ,godot-comments))
      (,godot-256-class (:foreground ,godot-256-comments))))

   `(web-mode-constant-face
     ((,godot-class (:foreground ,godot-violet))
      (,godot-256-class (:foreground ,godot-256-violet))))

   `(web-mode-current-element-highlight-face
     ((,godot-class (:underline unspecified
                                :weight unspecified
                                :background ,godot-highlight-line))
      (,godot-256-class (:underline unspecified
                                    :weight unspecified
                                    :background ,godot-256-highlight-line))))

   `(web-mode-doctype-face
     ((,godot-class (:foreground ,godot-comments
                                 :slant italic
                                 :weight bold))
      (,godot-256-class (:foreground ,godot-256-comments
                                     :slant italic
                                     :weight bold))))

   `(web-mode-folded-face
     ((,godot-class (:underline t))
      (,godot-256-class (:underline t))))

   `(web-mode-function-name-face
     ((,godot-class (:foreground ,godot-green))
      (,godot-256-class (:foreground ,godot-256-green))))

   `(web-mode-html-attr-name-face
     ((,godot-class (:foreground ,godot-blue))
      (,godot-256-class (:foreground ,godot-256-blue))))

   `(web-mode-html-attr-custom-face
     ((,godot-class (:inherit web-mode-html-attr-name-face))
      (,godot-256-class (:inherit web-mode-html-attr-name-face))))

   `(web-mode-html-attr-engine-face
     ((,godot-class (:inherit web-mode-block-delimiter-face))
      (,godot-256-class (:inherit web-mode-block-delimiter-face))))

   `(web-mode-html-attr-equal-face
     ((,godot-class (:inherit web-mode-html-attr-name-face))
      (,godot-256-class (:inherit web-mode-html-attr-name-face))))

   `(web-mode-html-attr-value-face
     ((,godot-class (:foreground ,godot-yellow))
      (,godot-256-class (:foreground ,godot-256-yellow))))

   `(web-mode-html-tag-face
     ((,godot-class (:foreground ,godot-green))
      (,godot-256-class (:foreground ,godot-256-green))))

   `(web-mode-html-tag-bracket-face
     ((,godot-class (:foreground ,godot-gray))
      (,godot-256-class (:foreground ,godot-256-gray))))

   `(web-mode-keyword-face
     ((,godot-class (:foreground ,godot-red))
      (,godot-256-class (:foreground ,godot-256-red))))

   `(web-mode-preprocessor-face
     ((,godot-class (:foreground ,godot-yellow
                                 :slant normal
                                 :weight unspecified))
      (,godot-256-class (:foreground ,godot-256-yellow
                                     :slant normal
                                     :weight unspecified))))

   `(web-mode-string-face
     ((,godot-class (:foreground ,godot-yellow))
      (,godot-256-class (:foreground ,godot-256-yellow))))

   `(web-mode-type-face
     ((,godot-class (:inherit font-lock-type-face))
      (,godot-256-class (:inherit font-lock-type-face))))

   `(web-mode-variable-name-face
     ((,godot-class (:foreground ,godot-orange))
      (,godot-256-class (:foreground ,godot-256-orange))))

   `(web-mode-warning-face
     ((,godot-class (:inherit font-lock-warning-face))
      (,godot-256-class (:inherit font-lock-warning-face))))

   `(web-mode-block-face
     ((,godot-class (:background unspecified))
      (,godot-256-class (:background unspecified))))

   `(web-mode-block-delimiter-face
     ((,godot-class (:inherit font-lock-preprocessor-face))
      (,godot-256-class (:inherit font-lock-preprocessor-face))))

   `(web-mode-block-comment-face
     ((,godot-class (:inherit web-mode-comment-face))
      (,godot-256-class (:inherit web-mode-comment-face))))

   `(web-mode-block-control-face
     ((,godot-class (:inherit font-lock-preprocessor-face))
      (,godot-256-class (:inherit font-lock-preprocessor-face))))

   `(web-mode-block-string-face
     ((,godot-class (:inherit web-mode-string-face))
      (,godot-256-class (:inherit web-mode-string-face))))

   `(web-mode-comment-keyword-face
     ((,godot-class (:box 1 :weight bold))
      (,godot-256-class (:box 1 :weight bold))))

   `(web-mode-css-at-rule-face
     ((,godot-class (:inherit font-lock-constant-face))
      (,godot-256-class (:inherit font-lock-constant-face))))

   `(web-mode-css-pseudo-class-face
     ((,godot-class (:inherit font-lock-builtin-face))
      (,godot-256-class (:inherit font-lock-builtin-face))))

   `(web-mode-css-color-face
     ((,godot-class (:inherit font-lock-builtin-face))
      (,godot-256-class (:inherit font-lock-builtin-face))))

   `(web-mode-css-filter-face
     ((,godot-class (:inherit font-lock-function-name-face))
      (,godot-256-class (:inherit font-lock-function-name-face))))

   `(web-mode-css-function-face
     ((,godot-class (:inherit font-lock-builtin-face))
      (,godot-256-class (:inherit font-lock-builtin-face))))

   `(web-mode-css-function-call-face
     ((,godot-class (:inherit font-lock-function-name-face))
      (,godot-256-class (:inherit font-lock-function-name-face))))

   `(web-mode-css-priority-face
     ((,godot-class (:inherit font-lock-builtin-face))
      (,godot-256-class (:inherit font-lock-builtin-face))))

   `(web-mode-css-property-name-face
     ((,godot-class (:inherit font-lock-variable-name-face))
      (,godot-256-class (:inherit font-lock-variable-name-face))))

   `(web-mode-css-selector-face
     ((,godot-class (:inherit font-lock-keyword-face))
      (,godot-256-class (:inherit font-lock-keyword-face))))

   `(web-mode-css-string-face
     ((,godot-class (:inherit web-mode-string-face))
      (,godot-256-class (:inherit web-mode-string-face))))

   `(web-mode-javascript-string-face
     ((,godot-class (:inherit web-mode-string-face))
      (,godot-256-class (:inherit web-mode-string-face))))

   `(web-mode-json-comment-face
     ((,godot-class (:inherit web-mode-comment-face))
      (,godot-256-class (:inherit web-mode-comment-face))))

   `(web-mode-json-context-face
     ((,godot-class (:foreground ,godot-violet))
      (,godot-256-class (:foreground ,godot-256-violet))))

   `(web-mode-json-key-face
     ((,godot-class (:foreground ,godot-violet))
      (,godot-256-class (:foreground ,godot-256-violet))))

   `(web-mode-json-string-face
     ((,godot-class (:inherit web-mode-string-face))
      (,godot-256-class (:inherit web-mode-string-face))))

   `(web-mode-param-name-face
     ((,godot-class (:foreground ,godot-foreground))
      (,godot-256-class (:foreground ,godot-256-foreground))))

   `(web-mode-part-comment-face
     ((,godot-class (:inherit web-mode-comment-face))
      (,godot-256-class (:inherit web-mode-comment-face))))

   `(web-mode-part-face
     ((,godot-class (:inherit web-mode-block-face))
      (,godot-256-class (:inherit web-mode-block-face))))

   `(web-mode-part-string-face
     ((,godot-class (:inherit web-mode-string-face))
      (,godot-256-class (:inherit web-mode-string-face))))

   `(web-mode-symbol-face
     ((,godot-class (:foreground ,godot-violet))
      (,godot-256-class (:foreground ,godot-256-violet))))

   `(web-mode-whitespace-face
     ((,godot-class (:background ,godot-red))
      (,godot-256-class (:background ,godot-256-red))))

   ;; whitespace-mode
   `(whitespace-space
     ((,godot-class (:background unspecified
                                 :foreground ,godot-comments
                                 :inverse-video unspecified
                                 :slant italic))
      (,godot-256-class (:background unspecified
                                     :foreground ,godot-256-comments
                                     :inverse-video unspecified
                                     :slant italic))))

   `(whitespace-hspace
     ((,godot-class (:background unspecified
                                 :foreground ,godot-emphasis
                                 :inverse-video unspecified))
      (,godot-256-class (:background unspecified
                                     :foreground ,godot-256-emphasis
                                     :inverse-video unspecified))))

   `(whitespace-tab
     ((,godot-class (:background unspecified
                                 :foreground ,godot-red
                                 :inverse-video unspecified
                                 :weight bold))
      (,godot-256-class (:background unspecified
                                     :foreground ,godot-256-red
                                     :inverse-video unspecified
                                     :weight bold))))

   `(whitespace-newline
     ((,godot-class(:background unspecified
                                :foreground ,godot-comments
                                :inverse-video unspecified))
      (,godot-256-class (:background unspecified
                                     :foreground ,godot-256-comments
                                     :inverse-video unspecified))))

   `(whitespace-trailing
     ((,godot-class (:background unspecified
                                 :foreground ,godot-orange-lc
                                 :inverse-video t))
      (,godot-256-class (:background unspecified
                                     :foreground ,godot-256-orange-lc
                                     :inverse-video t))))

   `(whitespace-line
     ((,godot-class (:background unspecified
                                 :foreground ,godot-magenta
                                 :inverse-video unspecified))
      (,godot-256-class (:background unspecified
                                     :foreground ,godot-256-magenta
                                     :inverse-video unspecified))))

   `(whitespace-space-before-tab
     ((,godot-class (:background ,godot-red-lc
                                 :foreground unspecified
                                 :inverse-video unspecified))
      (,godot-256-class (:background ,godot-256-red-lc
                                     :foreground unspecified
                                     :inverse-video unspecified))))

   `(whitespace-indentation
     ((,godot-class (:background unspecified
                                 :foreground ,godot-yellow
                                 :inverse-video unspecified
                                 :weight bold))
      (,godot-256-class (:background unspecified
                                     :foreground ,godot-256-yellow
                                     :inverse-video unspecified
                                     :weight bold))))

   `(whitespace-empty
     ((,godot-class (:background unspecified
                                 :foreground ,godot-red-lc
                                 :inverse-video t))
      (,godot-256-class (:background unspecified
                                     :foreground ,godot-256-red-lc
                                     :inverse-video t))))

   `(whitespace-space-after-tab
     ((,godot-class (:background unspecified
                                 :foreground ,godot-orange
                                 :inverse-video t
                                 :weight bold))
      (,godot-256-class (:background unspecified
                                     :foreground ,godot-256-orange
                                     :inverse-video t
                                     :weight bold))))

   ;; wanderlust
   `(wl-highlight-folder-few-face
     ((,godot-class (:foreground ,godot-red))
      (,godot-256-class (:foreground ,godot-256-red))))

   `(wl-highlight-folder-many-face
     ((,godot-class (:foreground ,godot-red))
      (,godot-256-class (:foreground ,godot-256-red))))

   `(wl-highlight-folder-path-face
     ((,godot-class (:foreground ,godot-orange))
      (,godot-256-class (:foreground ,godot-256-orange))))

   `(wl-highlight-folder-unread-face
     ((,godot-class (:foreground ,godot-blue))
      (,godot-256-class (:foreground ,godot-256-blue))))

   `(wl-highlight-folder-zero-face
     ((,godot-class (:foreground ,godot-foreground))
      (,godot-256-class (:foreground ,godot-256-foreground))))

   `(wl-highlight-folder-unknown-face
     ((,godot-class (:foreground ,godot-blue))
      (,godot-256-class (:foreground ,godot-256-blue))))

   `(wl-highlight-message-citation-header
     ((,godot-class (:foreground ,godot-red))
      (,godot-256-class (:foreground ,godot-256-red))))

   `(wl-highlight-message-cited-text-1
     ((,godot-class (:foreground ,godot-red))
      (,godot-256-class (:foreground ,godot-256-red))))

   `(wl-highlight-message-cited-text-2
     ((,godot-class (:foreground ,godot-green))
      (,godot-256-class (:foreground ,godot-256-green))))

   `(wl-highlight-message-cited-text-3
     ((,godot-class (:foreground ,godot-blue))
      (,godot-256-class (:foreground ,godot-256-blue))))

   `(wl-highlight-message-cited-text-4
     ((,godot-class (:foreground ,godot-blue))
      (,godot-256-class (:foreground ,godot-256-blue))))

   `(wl-highlight-message-header-contents-face
     ((,godot-class (:foreground ,godot-green))
      (,godot-256-class (:foreground ,godot-256-green))))

   `(wl-highlight-message-headers-face
     ((,godot-class (:foreground ,godot-red))
      (,godot-256-class (:foreground ,godot-256-red))))

   `(wl-highlight-message-important-header-contents
     ((,godot-class (:foreground ,godot-green))
      (,godot-256-class (:foreground ,godot-256-green))))

   `(wl-highlight-message-header-contents
     ((,godot-class (:foreground ,godot-green))
      (,godot-256-class (:foreground ,godot-256-green))))

   `(wl-highlight-message-important-header-contents2
     ((,godot-class (:foreground ,godot-green))
      (,godot-256-class (:foreground ,godot-256-green))))

   `(wl-highlight-message-signature
     ((,godot-class (:foreground ,godot-green))
      (,godot-256-class (:foreground ,godot-256-green))))

   `(wl-highlight-message-unimportant-header-contents
     ((,godot-class (:foreground ,godot-foreground))
      (,godot-256-class (:foreground ,godot-256-foreground))))

   `(wl-highlight-summary-answegodot-red-face
     ((,godot-class (:foreground ,godot-blue))
      (,godot-256-class (:foreground ,godot-256-blue))))

   `(wl-highlight-summary-disposed-face
     ((,godot-class (:foreground ,godot-foreground
                                 :slant italic))
      (,godot-256-class (:foreground ,godot-256-foreground
                                     :slant italic))))

   `(wl-highlight-summary-new-face
     ((,godot-class (:foreground ,godot-blue))
      (,godot-256-class (:foreground ,godot-256-blue))))

   `(wl-highlight-summary-normal-face
     ((,godot-class (:foreground ,godot-foreground))
      (,godot-256-class (:foreground ,godot-256-foreground))))

   `(wl-highlight-summary-thread-top-face
     ((,godot-class (:foreground ,godot-yellow))
      (,godot-256-class (:foreground ,godot-256-yellow))))

   `(wl-highlight-thread-indent-face
     ((,godot-class (:foreground ,godot-magenta))
      (,godot-256-class (:foreground ,godot-256-magenta))))

   `(wl-highlight-summary-refiled-face
     ((,godot-class (:foreground ,godot-foreground))
      (,godot-256-class (:foreground ,godot-256-foreground))))

   `(wl-highlight-summary-displaying-face
     ((,godot-class (:underline t
                                :weight bold))
      (,godot-256-class (:underline t
                                    :weight bold))))

   ;; weechat
   `(weechat-error-face
     ((,godot-class (:inherit error))
      (,godot-256-class (:inherit error))))

   `(weechat-highlight-face
     ((,godot-class (:foreground ,godot-emphasis
                                 :weight bold))
      (,godot-256-class (:foreground ,godot-256-emphasis
                                     :weight bold))))

   `(weechat-nick-self-face
     ((,godot-class (:foreground ,godot-green
                                 :weight unspecified
                                 :inverse-video t))
      (,godot-256-class (:foreground ,godot-256-green
                                     :weight unspecified
                                     :inverse-video t))))

   `(weechat-prompt-face
     ((,godot-class (:inherit minibuffer-prompt))
      (,godot-256-class (:inherit minibuffer-prompt))))

   `(weechat-time-face
     ((,godot-class (:foreground ,godot-comments))
      (,godot-256-class (:foreground ,godot-256-comments))))

   ;; which-func-mode
   `(which-func
     ((,godot-class (:foreground ,godot-green))
      (,godot-256-class (:foreground ,godot-256-green))))

   ;; which-key
   `(which-key-key-face
     ((,godot-class (:foreground ,godot-green
                                 :weight bold))
      (,godot-256-class (:foreground ,godot-256-green
                                     :weight bold))))

   `(which-key-separator-face
     ((,godot-class (:foreground ,godot-comments))
      (,godot-256-class (:foreground ,godot-256-comments))))

   `(which-key-note-face
     ((,godot-class (:foreground ,godot-comments))
      (,godot-256-class (:foreground ,godot-256-comments))))

   `(which-key-command-description-face
     ((,godot-class (:foreground ,godot-foreground))
      (,godot-256-class (:foreground ,godot-256-foreground))))

   `(which-key-local-map-description-face
     ((,godot-class (:foreground ,godot-yellow-hc))
      (,godot-256-class (:foreground ,godot-256-yellow-hc))))

   `(which-key-group-description-face
     ((,godot-class (:foreground ,godot-red
                                 :weight bold))
      (,godot-256-class (:foreground ,godot-256-red
                                     :weight bold))))

   ;; window-divider-mode
   `(window-divider
     ((,godot-class (:foreground ,godot-highlight))
      (,godot-256-class (:foreground ,godot-highlight))))
   `(window-divider-first-pixel ((t (:inherit window-divider))))
   `(window-divider-last-pixel ((t (:inherit window-divider))))

   ;; window-number-mode
   `(window-number-face
     ((,godot-class (:foreground ,godot-green))
      (,godot-256-class (:foreground ,godot-256-green))))

   ;; yascroll
   `(yascroll:thumb-text-area
     ((,godot-class (:foreground ,godot-comments
                                 :background ,godot-comments))
      (,godot-256-class (:foreground ,godot-256-comments
                                     :background ,godot-256-comments))))

   `(yascroll:thumb-fringe
     ((,godot-class (:foreground ,godot-comments
                                 :background ,godot-comments))
      (,godot-256-class (:foreground ,godot-256-comments
                                     :background ,godot-256-comments))))

   ;; zencoding
   `(zencoding-preview-input
     ((,godot-class (:background ,godot-highlight-line
                                 :box ,godot-emphasis))
      (,godot-256-class (:background ,godot-256-highlight-line
                                     :box ,godot-256-emphasis)))))

  (custom-theme-set-variables
   'godot
   `(ansi-color-names-vector [,godot-background ,godot-red ,godot-green ,godot-yellow
                                                ,godot-blue ,godot-magenta ,godot-cyan ,godot-foreground])

   ;; compilation
   `(compilation-message-face 'default)

   ;; fill-column-indicator
   `(fci-rule-color ,godot-highlight-line)

   ;; magit
   `(magit-diff-use-overlays nil)

   ;; highlight-changes
   `(highlight-changes-colors '(,godot-magenta ,godot-violet))

   ;; highlight-tail
   `(highlight-tail-colors
     '((,godot-highlight-line . 0)
       (,godot-green-lc . 20)
       (,godot-cyan-lc . 30)
       (,godot-blue-lc . 50)
       (,godot-yellow-lc . 60)
       (,godot-orange-lc . 70)
       (,godot-magenta-lc . 85)
       (,godot-highlight-line . 100)))

   ;; pos-tip
   `(pos-tip-foreground-color ,godot-background)
   `(pos-tip-background-color ,godot-yellow-hc)

   ;; vc
   `(vc-annotate-color-map
     '((20 . ,godot-red)
       (40 . "#CF4F1F")
       (60 . "#C26C0F")
       (80 . ,godot-yellow)
       (100 . "#AB8C00")
       (120 . "#A18F00")
       (140 . "#989200")
       (160 . "#8E9500")
       (180 . ,godot-green)
       (200 . "#729A1E")
       (220 . "#609C3C")
       (240 . "#4E9D5B")
       (260 . "#3C9F79")
       (280 . ,godot-cyan)
       (300 . "#299BA6")
       (320 . "#2896B5")
       (340 . "#2790C3")
       (360 . ,godot-blue)))
   `(vc-annotate-very-old-color nil)
   `(vc-annotate-background nil)

   ;; weechat
   `(weechat-color-list
     '(unspecified ,godot-background ,godot-highlight-line
                   ,godot-red-d ,godot-red
                   ,godot-green-d ,godot-green
                   ,godot-yellow-d ,godot-yellow
                   ,godot-blue-d ,godot-blue
                   ,godot-magenta-d ,godot-magenta
                   ,godot-cyan-d ,godot-cyan
                   ,godot-foreground ,godot-emphasis))))

;;;###autoload
(when (and (boundp 'custom-theme-load-path) load-file-name)
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'godot)

;; Local Variables:
;; no-byte-compile: t
;; fill-column: 95
;; End:

;;; godot-theme.el ends here
