;;;
tosolve	.byte	$01,$20,$03,$0f	; 1 CO
	.byte	$0c,$20,$0f,$06	; L OF
	.byte	$20,$30,$20,$5b	;  0 #
	.byte	$40		; _
solvtxt

gamewon	lda	#0
	rts
