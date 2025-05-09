;;; 
solvlen	.byte	solvtxt-tosolve
tosolve	.byte	$33,$20,$03,$0f	; 3 CO
	.byte	$0c,$20,$0f,$06	; L OF
	.byte	$20,$34,$20,$40	;  4 _
	.byte	$57		; @
solvtxt

gamewon	lda	#0
	rts
