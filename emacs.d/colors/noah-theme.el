
(deftheme noah-theme "My personal theme")

(let (
      ;(*background-color*   "#000000")
      (*brown*              "#E64")
      (*comments*           "#7C7C7C")
      (*constant*           "#99CC99")
      (*current-line*       "#151515")
      (*cursor-block*       "#FFFFFFF")
      (*cursor-underscore*  "#FFFAAA")
      (*keywords*           "#96CBFE")
      (*light-purple*       "#FFCCFF")
      (*line-number*        "#3D3D3D")
      (*method-declaration* "#FFD2A7")
      (*mode-line-bg*       "#202020")
      (*mode-line-fg*       "#CCCCCC")
      (*normal*             "#F6F3E8")
      (*number*             "#FF73FD")
      (*operators*          "#FFFFB6")
      (*red*                "#FF6C60")
      (*red-light*          "#FFB6B0")
      (*regexp*             "#E9C")
      (*regexp-alternate*   "#FF0")
      (*regexp-alternate-2* "#B18A3D")
      (*search-selection*   "#2F2F00")
      (*string*             "#A8FF60")
      (*string-inner*       "#00A0A0")
      (*variable*           "#C6C5FE")
      (*visual-selection*   "#262D51"))

  (custom-tehem-set-faces
   'noah-theme

   `(bold ((t (:bold t))))
   `(button ((t (:foreground, *keywords* :underline t))))
   `(default ((t (:background, *background-color* :foreground, *normal*))))
   `(escape-glyph ((t (:foreground, *string-inner*))))
   `(header-line ((t (:background, *mode-line-bg* :foreground, *normal*)))) ;; info header
   `(highlight ((t (:background, *current-line*))))
   `(highlight-face ((t (:background, *current-line*))))
   `(hl-line ((t (:background, *current-line* :underline t))))
   `(info-xref ((t (:foreground, *keywords* :underline t))))
   `(region ((t (:background, *visual-selection*))))
   `(underline ((nil (:underline t))))

   ;; show-paren
   `(show-paren-mismatch ((t (:background, *red* :foreground, *normal* :weight bold))))
   `(show-paren-match ((t (:background, *keywords* :foreground, *normal* :weight bold))))


   )

;;;###autoload
(when (and (boundp 'custom-theme-load-path) load-file-name)
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

;; Local Variables:
;; no-byte-compile: t
;; End:

(provide-theme 'noah-theme)
;;; My theme ends here
