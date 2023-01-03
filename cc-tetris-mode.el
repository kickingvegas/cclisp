;; tetris-mode

(add-hook 'tetris-mode-hook '(lambda ()
	                       (define-key tetris-mode-map "j" 'tetris-move-left)
	                       (define-key tetris-mode-map "i" 'tetris-rotate-prev)
	                       (define-key tetris-mode-map "k" 'tetris-rotate-next)
	                       (define-key tetris-mode-map "l" 'tetris-move-right)
	                       (define-key tetris-mode-map "m" 'tetris-move-down)))
